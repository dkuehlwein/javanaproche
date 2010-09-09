:- module(input_parser,[create_naproche_input/2]).


%%	create_naproche_input(+Atom,-List)
%
%	Takes an Atom, parses it for words, disregarding whitespaces and commas, and
%	creates naproche readable input of the form:
%	[sentence(Id,StartCharIndex,EndCharIndex,[word(StartCharIndex,EndCharIndex),...],Content),sentence(...].
%	The charactar '$' toggles the math-mode, i.e. the Atom 'Let $x$ be a number.'
%	becomes [sentence(1,0,20,[word(0,3),math(5,6),word(8,10),word(11,12),word(13,19)],[let,math([x]),be,a,number])].
%
%	@param Atom any Atom
%	@param List a naproche readable prolog list
%
%	@author Daniel Kühlwein
%	@author Mona Rahn
%	@author Julian Schlöder


create_naproche_input(Atom,List) :-
	%reset the error messages
	clear_messages,
	%reset the sentence-index
	retractall(sentence_index(_)),
	assertz(sentence_index(1)),

	% Create a List from the Atoms and process parsing the List.
	atom_chars(Atom,AtomList),
	input_dcg(List,0,_,AtomList,[]),
	!.

% Error case
create_naproche_input(Atom,_) :-
	add_error_message_once(inputError,create_naproche_input,Atom,0,0,'Could not parse input.'),
	!,
	fail.

%%	no_linebreak(+InIndex, -OutIndex, +DCG_in:List, -DCG_out:List)
%	Evaluates to true on any character except for # and newline and counts any passed character
no_linebreak(InIndex,OutIndex) -->
	[Char],
	{
	\+ Char='#',
	\+ char_type(Char, newline)
	},
	{NewInIndex is InIndex+1},
	no_linebreak(NewInIndex,OutIndex),
	!.

no_linebreak(InIndex,OutIndex) -->
	[Char],
        {
	\+ Char='#',
	\+ char_type(Char, newline)
        },
	{OutIndex is InIndex+1}.

%%	input_dcg(Out:List)
%
%	Takes a list of characters and creates a naproche readable ist, e.g. of the form 
%	[sentence(Id,StartCharIndex,EndCharIndex,[word(StartCharIndex,EndCharIndex),...],Content),sentence(...].
%
%	@param In is a naproche readable list
%	@param Out is a naproche readable list


input_dcg([],InIndex,InIndex) -->
	[].

% indicates commentary. ignored:
input_dcg(Out,InIndex,OutIndex) -->
	['%'],
	no_linebreak(InIndex,NewInIndex),
	[Char],
	{Char='#';
	 char_type(Char, newline)
	},
	{NewInIndex1 is NewInIndex +2},
	!,
	input_dcg(Out,NewInIndex1,OutIndex).

% special case: double-linebreak (new paragraph)
input_dcg([sentence(Index,InIndex,NewInIndex,Wordlist,Content)|Out],InIndex,OutIndex) -->
	[Char,Char],
	{Char='#';
	 Char='\\';
	 char_type(Char, newline)
	},
	{new_sentence_index(Index),
	 NewInIndex is InIndex+2,
	 Wordlist=[word(InIndex,NewInIndex)],
	 Content=['##']
	},
	!,
	input_dcg(Out,NewInIndex,OutIndex).

% All whitespace-characters are irrelevant (including single-linebreak), so they are ignored. However, they are indexed.
input_dcg(Out,InIndex,OutIndex) -->
	[Char],
	{
	char_type(Char, white);
	char_type(Char, newline);
	char_type(Char, end_of_line);
	Char ='#'
	},
	!,
	{NewInIndex is InIndex +1},
	input_dcg(Out,NewInIndex,OutIndex).	


% Environments (\begin / \end)
input_dcg([sentence(Index,InIndex,NewInIndex2,Wordlist,Content)|Out],InIndex,OutIndex) -->
	['\\','b','e','g','i','n','{'],
	{new_sentence_index(Index)},
	{NewInIndex0 is InIndex+7},
	alnum(Word,NewInIndex0,NewInIndex1),
	['}'],
	{NewInIndex2 is NewInIndex1+1},
	!,
	{Content=[Word],
	Wordlist=[word(NewInIndex0,NewInIndex1)]},
	input_dcg(Out,NewInIndex2,OutIndex).

input_dcg([sentence(Index,InIndex,NewInIndex2,Wordlist,Content)|Out],InIndex,OutIndex) -->
	['\\','e','n','d','{'],
	{new_sentence_index(Index)},
	{NewInIndex0 is InIndex+5},
	alnum(Word,NewInIndex0,NewInIndex1),
	['}'],
	{NewInIndex2 is NewInIndex1+1},
	!,
	{concat('End_',Word,Con),
	Content=[Con],
	Wordlist=[word(NewInIndex0,NewInIndex1)]},
	input_dcg(Out,NewInIndex2,OutIndex).


% sentence case
input_dcg([sentence(Index,InIndex,NewInIndex,Wordlist,Content)|Out],InIndex,OutIndex) -->
{
	new_sentence_index(Index)
	},
	sentence(Content,InIndex,NewInIndex,[],Wordlist),
	!,
	input_dcg(Out,NewInIndex,OutIndex).

%%	sentence(-Out:List, +InIndex, -OutIndex, +InWordlist:List, -OutWordlist:List)
%
%	Parses the input until the next '.' or ':', 
%	concats characters to words.
%	Counts indices of starting and ending characters the whole sentence and each word. 
%	Saves start end end of words in Wordlist
%
%	@param In list of words parsed so far
%	@param Out complete sentence.

% White Space Case
sentence(Out,InIndex,OutIndex,InWordlist,OutWordlist) -->
	[Char],
	{
	char_type(Char, white);
	char_type(Char, newline);
	char_type(Char, end_of_line);
	Char ='#'
	},
	!,
	{NewInIndex is InIndex+1},
	sentence(Out,NewInIndex,OutIndex,InWordlist,OutWordlist).

% Word Case
sentence([Word|Out],InIndex,OutIndex,InWordlist,OutWordlist) -->
	word(Word,InIndex,NewInIndex),
	!,
	{append(InWordlist,[word(InIndex,NewInIndex)],NewOutWordlist)},
	%append indices of found word to the incoming Wordlist and continue with the result.
	sentence(Out,NewInIndex,OutIndex,NewOutWordlist,OutWordlist).

% Skip commas
sentence([Char|Out],InIndex,OutIndex,InWordlist,OutWordlist) -->
	[Char],
	{
	Char=',',
	!,
	NewInIndex is InIndex
	},
	sentence(Out,NewInIndex,OutIndex,InWordlist,OutWordlist).

% Math Case
sentence([math(Content)|Out],InIndex,OutIndex,InWordlist,OutWordlist) -->
	['$'],
	{NewInIndex0 is InIndex+1},
	% Input math in ended by a $
	input_math(Content,NewInIndex0,NewInIndex1), 
	['$'],
	{NewInIndex2 is NewInIndex1+1},
	!,
	{append(InWordlist,[math(NewInIndex0,NewInIndex1)],NewOutWordlist)},
	%append indices of found math-clause to the incoming Wordlist and continue with the result.
	%$-characters are not counted as belonging to the math-clause
	sentence(Out,NewInIndex2,OutIndex,NewOutWordlist,OutWordlist).

sentence([_|Out],InIndex,OutIndex,InWordlist,OutWordlist) -->
	['$'],
	{NewInIndex0 is InIndex+1},
	math_fail(Y,NewInIndex0,NewInIndex1),
	{
	atom_concat('$',Y,X),
	add_error_message_once(inputError,sentence,X,InIndex,NewInIndex1,'Could not parse math mode.'),
	!
	},
	sentence(Out,NewInIndex1,OutIndex,InWordlist,OutWordlist),
	{ fail }.



% Sentence end case

sentence([],InIndex,OutIndex,Wordlist,Wordlist) -->
	([.];[:]),
	!,
	{OutIndex is InIndex +1}.

sentence(_,InIndex,OutIndex,Wordlist,Wordlist) -->
	([];[_]),
	{OutIndex is InIndex +1},
	{add_error_message_once(inputError,sentence,0,InIndex,OutIndex,'No terminal symbol found.'),
	!,	
	fail}.



%%	word(-Out:List,+InIndex,-OutIndex)
%
%	parses a word till the next whitespace character.
%	Generates indices of first and last character.

%To allow i.e. and e.g. constructions
word('i.e.',InIndex,OutIndex) -->
        [i,'.',e,'.'],
	{OutIndex is InIndex +4},
        !.
word('e.g.',InIndex,OutIndex) -->
        [e,'.',g,'.'],
	{OutIndex is InIndex +4},
        !.

word(Out,InIndex,OutIndex) -->
       [Char],
       {
       char_type(Char, alnum),
       to_lower(Char,Num),
       atom_codes(LowChar,[Num]),
       NewInIndex is InIndex+1
       },
       word(TmpOut,NewInIndex,OutIndex),
       {atom_concat(LowChar, TmpOut, Out)}.

word(Out,InIndex,OutIndex) -->
	[Char],
	{
	char_type(Char, alnum),
	to_lower(Char,Num),
	atom_codes(Out,[Num]),
	OutIndex is InIndex+1
	}.


%%      lalnum(-Out:atom,+InIndex,-OutIndex)
%
%       parses a latex-command (alnum starting with \)
%       Counts characters.

lalnum(Out,InIndex,OutIndex) -->
       [Char],
       {
       Char = '\\',
       NewInIndex is InIndex+1
       },
       alnum(TmpOut,NewInIndex,OutIndex),
       !,
       {
       atom_concat(Char, TmpOut, Out)
       }.



%%	input_math(-Out:List,+InIndex,-OutIndex, +DCG_in:List, -DCG_out:List)
%
%	parses until the next $
%	generates indices of first and last character of the whole math-input clause

input_math([Math|Out],InIndex,OutIndex) -->
       [Math],
       {
       Math='=';
       Math=',';
       Math='(';
       Math=')';
       Math='*';
       Math='+';
       Math='^';
       Math='/'
       },
       {NewInIndex is InIndex +1},
       input_math(Out,NewInIndex,OutIndex).

input_math(Out,InIndex,OutIndex) -->
       [Math],
       {
       	char_type(Math,white);
       	char_type(Math,newline);
	char_type(Math,end_of_line);
	Math ='#'
       },
       !,
       {NewInIndex is InIndex +1},
       input_math(Out,NewInIndex,OutIndex).


input_math([Math|Out],InIndex,OutIndex) -->
	[Math],
	{char_type(Math, alnum),
	NewInIndex is InIndex+1},
	input_math(Out,NewInIndex,OutIndex).


input_math([Char|Out],InIndex,OutIndex) -->
	[Char],
	{Char='<'},
	[],
	!,
        {NewInIndex is InIndex +1},
	input_math(Out,NewInIndex,OutIndex).

input_math([Char|Out],InIndex,OutIndex) -->
	[Char],
	{Char='>'},
	[],
	!,
        {NewInIndex is InIndex +1},
	input_math(Out,NewInIndex,OutIndex).

% General LaTeX commands
%
%
% Latex commands with two arguments
input_math(Out,InIndex,OutIndex) -->
	lalnum(Latex,InIndex,NewInIndex0),
	['{'],
        {NewInIndex1 is NewInIndex0 +1},
	input_math(X,NewInIndex1,NewInIndex2),
	['}','{'],
        {NewInIndex3 is NewInIndex2 +2},
	input_math(Y,NewInIndex3,NewInIndex4),
	['}'],
        {NewInIndex5 is NewInIndex4 +1},
	!,
	[],
	input_math(Rest,NewInIndex5,OutIndex),
	{
	append([[Latex],['('],X,[','],Y,[')'],Rest],Out)
	}.

% Special case: \mathbb{X} does not represent a function
% hence: no parentheses and a special representation (\bb)
input_math(Out,InIndex,OutIndex) -->
	lalnum(Latex,InIndex,NewInIndex0),
	{Latex='\\mathbb'},
	['{'],
        {NewInIndex1 is NewInIndex0 +1},
	input_math(X,NewInIndex1,NewInIndex2),
	['}'],
        {NewInIndex3 is NewInIndex2 +1},
	!,
	[],
	input_math(Rest,NewInIndex3,OutIndex),
	{
	append([['\\bb'],X,Rest],Out)
	}.

% Latex commands with one argument
input_math(Out,InIndex,OutIndex) -->
	lalnum(Latex,InIndex,NewInIndex0),
	['{'],
        {NewInIndex1 is NewInIndex0 +1},
	input_math(X,NewInIndex1,NewInIndex2),
	['}'],
        {NewInIndex3 is NewInIndex2 +1},
	!,
	([' '];[]),
	input_math(Rest,NewInIndex3,OutIndex),
	{
	append([[Latex],['('],X,[')'],Rest],Out)
	}.

% Latex commands with no argument
input_math([Latex|Out],InIndex,OutIndex) -->
 	lalnum(Latex,InIndex,NewInIndex),
	[],
 	!,
 	input_math(Out,NewInIndex,OutIndex).

% Latex error message
input_math(Out,InIndex,OutIndex) -->
	['\\'],
	{NewInIndex is InIndex+1},
	{
	add_error_message_once(inputError,input_math,'\\',InIndex,NewInIndex,'Missing Latex command.'),
	!},
	input_math(Out,NewInIndex,OutIndex),
	{
	fail
	}.

input_math([],InIndex,InIndex) -->
       [].


%%      alnum(-Out:atom, +InIndex, -OutIndex)
%
%       parses a word (containing alphanumeric characters)
%       generates indices of first and last charaters

alnum(Out,InIndex,OutIndex) -->
       [Char],
       {
       char_type(Char, alnum)
       },
       {NewInIndex is InIndex+1},
       alnum(TmpOut,NewInIndex,OutIndex),
       !,
       {
       atom_concat(Char, TmpOut, Out)
       }.


alnum(Char,InIndex,OutIndex) -->
	[Char],
        {
        char_type(Char, alnum)
        },
	{OutIndex is InIndex+1}.


%%	math_fail(-Out:atom,+InIndex,-OutIndex)
%	
%	returns code from $ to the next $ if math_input fails
%	returns indices of the characters directly after the first $ and directly before the last $

math_fail('$',InIndex,OutIndex) -->
	[$],
	{OutIndex is InIndex+1},
	!.

math_fail(Out,InIndex,OutIndex) -->
	[Char],
	{NewInIndex is InIndex+1},
	math_fail(TmpOut,NewInIndex,OutIndex),
	{atom_concat(Char,TmpOut,Out)},
	!.

math_fail(Char,InIndex,OutIndex) -->
	[Char],
	{OutIndex is InIndex+1},
	{add_error_message_once(inputError,math_fail,0,InIndex,OutIndex,'Missing $.')}.

%%	new_sentence_index(-Index)
%
%	Generates new sentence Index (sentences are enumerated)
new_sentence_index(Index) :-
	retract(sentence_index(Index)),
    	succ(Index, NewIndex),
      	assertz(sentence_index(NewIndex)).

%%	add_error_message_once(+Type, +Position, +Subject, +Start, +End, +Description)
%
%	Appends new error message with position being a comma-seperated set containing three elements
%	intended for the syntactical position ("Position", e.g. "sentence") and two absolute indices ("Start" and "End")
add_error_message_once(Type, Position, Subject, Start, End, Description) :-
	atom_concat(', ',Start,Tmp),
	atom_concat(Tmp,', ',Tmp2),
	atom_concat(Tmp2,End,Tmp3),
	atom_concat(Position, Tmp3, NewPosition),
	add_error_message_once(Type, NewPosition, Subject, Description).
