:- module(input_utils,[process_input/2]).

:- use_module(library(pldoc)).
:- use_module(naproche(discharge_obligations)).
:- use_module(naproche(stats)).

/**     <module>  contains predicates to be used during the input parsing.
 *
 *      This module contains predicates which are only used during the parsing of the input.
 */

%%	process_input(+Input:Atom,+Submit:Atom)
%
%	Calls different predicats, depending on Submit.
%	In case of failure, it throws on error messages.
%
%	@param Input is the input text from the html form.
%	@param Submit is the output the user asked for. Must be either 
%		'Create Naproche Input', 'Create PRS' or 'Check Input'

process_input(Input,Submit) :-
	process_input_intern(Input,Submit),
	write_errors.

process_input(_,_) :-
%	get_messages(Messages),
%	print_list_html(Messages),
	write_errors.

process_input_intern(Input, 'Parse Input') :-
	% Case 1 : Only create Naproche Input
	!,
%	create_naproche_input(Input,NaprocheText),
	print_list_html(Input).

process_input_intern(Input, 'Create PRS') :-
	% Case 2 : Show PRS
	!,
%	create_naproche_input(Input,NaprocheText),
	clear_messages,
	!,
	build_prs(PRS,Input),
	write_prs_xhtml(PRS).
	
	/*
	For XML output:

	prs_to_xml(PRS,XML),
	open('output-file.xml', write, Stream),
	xml_write(Stream, XML, []),
	close(Stream).
	*/

process_input_intern(Input, 'Logical Check') :-
	% Case 3 : Check Input
	!,

	% Time Information
	statistics(real_time,_),

	format('<div id="output">~n',[]),
/*
	% ----------------------- Create Input ---------------------------------------

        format('<br/> Parsing Input ', []),
	create_naproche_input(Input,NaprocheText),

	% Time
	statistics(real_time,XTParse),
	XTParse = [_,TParse],
	XTPRS = [_,TPRS],
        format('Time spent: ~w sec <br/> ~n', [TParse]),
*/
	% ----------------------- Building PRS -----------------------------------------
	
	!,
    format('Building PRS', []),
	build_prs(PRS,Input),
	clear_messages,

	% Display
	( debug(on) ->
		PRSSet = debug;
		PRSSet = user
	),
	write_prs_xhtml_to_file(PRS,PRSSet,PRSFile),
	format('<a href = "~w" > View PRS </a>',[PRSFile]),

	% Time Update
	statistics(real_time,XTPRS),
	XTPRS = [_,TPRS],
    format('Time spent: ~w sec <br/> ~n', [TPRS]),
	% We are only interested in the first PRS created
	!,

	% ------------------------ Creating Proof Obligations ------------------------------

    format('<br/> Creating Proof Obligations ', []),
	trim_stacks,
	create_obligations(PRS,PGraph,Obligations),
	
	% Display
	display_graph(PGraph,prs,PGFile),
	format('<a href = "~w" > View PRS Graph </a>',[PGFile]),

	% Time Update
	statistics(real_time,XTCreate),
	XTCreate = [_,TCreate],
    format('Time spent: ~w sec <br/> ~n', [TCreate]),

	% --------------------- Discharge Proof Obligations -------------------------------

        format('<br/> Discharging Proof Obligations~n ', []),
	discharge_obligations(Obligations,PGraph,_EndGraph,Stats),

	% Display
%	display_graph(EndGraph,end,EndGFile),
%	format('<a href = "~w" > View Final Graph </a>',[EndGFile]),

	% Time Update
	statistics(real_time,XTDis),
	XTDis = [_,TDis],
        format('Time spent: ~w sec <br/> ~n', [TDis]),

	% ----------------------- Writing Statistics --------------------------------------

        format('<br/> Creating Statistics ~n', []),
	write_stats_php(Stats,StatsFile),
	format('<a href = "~w" > Final Stats </a>',[StatsFile]),

	% Time Update
	statistics(real_time,XTStats),
	XTStats = [_,TStats],
        format('Time spent: ~w sec <br/>~n', [TStats]),
        format('</div>', []),
	!.

%%	print_list_html(+List)
%
%	Print a list as html.

print_list_html([]) :-
	format('Unknown Prolog Error.').

print_list_html(List) :-
	format('[ <br/>',[]),
	print_list_html_main(List).	

print_list_html_main([Head|[]]) :-
	print_html([Head]),
	format('<br/> ]',[]).

print_list_html_main([Head|Tail]) :-
	print_html([Head]),
	format(', <br/>',[]),
	print_list_html_main(Tail).


%%	write_errors
%
%	Create a file error.php which contains all the errors.

write_errors :-
	session_id(SId),
	check_src(SRC),

	concat_atom([SRC,'/tmp/',SId,'/error.php'],File),
	get_messages(Messages),

	open(File,write,OS),

	format(OS,'
<?php $prologErrorList = array(~n',[]),

	write_errors_intern(OS,Messages),

	format(OS,'); ?>',[]),

	close(OS).

write_errors_intern(_OS,[]).

write_errors_intern(OS,[message(Level,Type,Predicate,Indicator,Exp)]) :-
	format(OS,'array("~w","~w","~w","~w","~w")~n',[Level,Type,Predicate,Indicator,Exp]),
	!.

write_errors_intern(OS,[message(Level,Type,Predicate,Indicator,Exp)|Rest]) :-
	format(OS,'array("~w","~w","~w","~w","~w"),~n',[Level,Type,Predicate,Indicator,Exp]),
	!,
	write_errors_intern(OS,Rest).


%%	create_naproche_input(+Atom,-List)
%
%	Takes an Atom, usually the input of an CGI-form, parses it for words, disregarding whitespaces, and
%	creates naproche readable input, i.e. of the form [sentence(Id,Content),sentence....].
%	The charactar '$' toggles the math-mode, i.e. the Atom 'Let $x$ be a number.'
%	becomes [sentence(1,[let,math([x]),be,a,number])].
%
%	@param Atom any Atom
%	@param List a naproche readable prolog list
%
%	@author Daniel Kühlwein
%	@author Mona Rahn


create_naproche_input(Atom,List) :-
	%reset the index
	retractall(sentence_index(_)),
	assertz(sentence_index(1)),

	% Create a List from the Atoms and process parsing the List.
	atom_chars(Atom,AtomList),
	input_dcg(List,0,_,AtomList,[]),
	!.

% Error case
create_naproche_input(Atom,_) :-
	add_error_message_once(inputError,create_naproche_input,Atom,'Could not parse input.'),
	!,
	fail.

%%	input_dcg(Out:List)
%
%	Takes a list of characters and creates a naproche readable ist, e.g. of the form 
%	[sentence(Id,Content),sentence....].
%
%	@param In is a naproche readable list
%	@param Out is a naproche readable list

input_dcg([],InIndex,InIndex) -->
	[].

% Textfield in HTML ends with several whitespaces, therefore we allow them here.
input_dcg(Out,InIndex,OutIndex) -->
	[Char],
	{
	char_type(Char, white);
	char_type(Char, newline);
	char_type(Char, end_of_line)
	},
	!,
	{NewInIndex is InIndex +1},
	input_dcg(Out,NewInIndex,OutIndex).	

% Allow "\\" everywhere in the file, not only in sentences
input_dcg(Out,InIndex,OutIndex) -->
	[\,\,\,\],
	!,
	{NewInIndex is InIndex +1},
	input_dcg(Out,NewInIndex,OutIndex).	

input_dcg([sentence(Index,InIndex,NewInIndex,Wordlist,Content)|Out],InIndex,OutIndex) -->
	{
	new_sentence_index(Index)
	},
	sentence(Content,InIndex,NewInIndex,[],Wordlist),
	!,
	input_dcg(Out,NewInIndex,OutIndex).


%%	sentence(-Out:List)
%
%	Parses the input until the next '.' or ':', 
%	concats characters to words.
%
%	@param In list of words parsed so far
%	@param Out complete sentence.

% White Space Case

sentence(Out,InIndex,OutIndex,InWordlist,OutWordlist) -->
	[Char],
	{
	char_type(Char, white);
	char_type(Char, newline);
	char_type(Char, end_of_line)
	},
	!,
	{NewInIndex is InIndex+1},
	sentence(Out,NewInIndex,OutIndex,InWordlist,OutWordlist).

% Word Case
sentence([Word|Out],InIndex,OutIndex,InWordlist,OutWordlist) -->
	word(Word,InIndex,NewInIndex),
	!,
	{append(InWordlist,[word(InIndex,NewInIndex)],NewOutWordlist)},
	sentence(Out,NewInIndex,OutIndex,NewOutWordlist,OutWordlist).

% Skip \\ in Latex
sentence(Out,InIndex,OutIndex,InWordlist,OutWordlist) -->
	[\,\,\,\],
	!,
	{NewInIndex is InIndex+2},
	sentence(Out,NewInIndex,OutIndex,InWordlist,OutWordlist).

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
	sentence(Out,NewInIndex2,OutIndex,NewOutWordlist,OutWordlist).

sentence([_|Out],InIndex,OutIndex,InWordlist,OutWordlist) -->
	['$'],
	{NewInIndex is InIndex+1},
	math_fail(Y,NewInIndex,NewNewInIndex),
	{
	atom_concat('$',Y,X),
	add_error_message_once(inputError,sentence,X,'Could not parse math mode.'),
	!
	},
	sentence(Out,NewNewInIndex,OutIndex,InWordlist,OutWordlist),
	{ fail }.



% Sentence end case

sentence([],InIndex,OutIndex,Wordlist,Wordlist) -->
	([.];[:]),
	!,
	{OutIndex is InIndex +1}.

sentence([_,InIndex,OutIndex],Wordlist,Wordlist) -->
	([];[_]),
	{OutIndex is InIndex +1},
	{add_error_message_once(inputError,sentence,0,'No terminal symbol found.'),
	!,	
	fail}.



%%	word(-Out:List)
%
%	parses a word till the next whitespace character.

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


%%      lalnum(-Out:atom)
%
%       parses a latex-command (word starting with \ or \\)

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



%%	input_math(-Out:List)
%
%	parsed until the next $

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

% input_math([succ|Out]) -->
% 	[\],
% 	[Math],
% 	{
% 	atom_codes(Math,[39]),
% 	!
% 	},
% 	input_math(Out).

input_math(Out,InIndex,OutIndex) -->
       [Math],
       {
       	char_type(Math,white);
       	char_type(Math,newline);
	char_type(Math,end_of_line)       
       },
       !,
       {NewInIndex is InIndex +1},
       input_math(Out,NewInIndex,OutIndex).


input_math([Math|Out],InIndex,OutIndex) -->
       alnum(Math,InIndex,NewInIndex),
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
input_math([WrongCode2|Out],InIndex,OutIndex) -->
	['\\'],
	{NewInIndex0 is InIndex+1},
	alnum(WrongCode,NewInIndex0,NewInIndex1),
	{
	!,
	atom_concat(\,WrongCode,WrongCode2),
	add_error_message_once(inputError,input_math,WrongCode2,'Latex command not supported.')},
	input_math(Out,NewInIndex1,OutIndex),
	{
	fail
	}.

input_math([Out],InIndex,OutIndex) -->
	['\\'],
	{NewInIndex is InIndex+1},
	{
	add_error_message_once(inputError,input_math,'','Missing Latex command.'),
	!},
	input_math(Out,NewInIndex,OutIndex),
	{
	fail
	}.

input_math([],InIndex,InIndex) -->
       [].


%%      alnum(-Out:atom)
%
%       parses a word

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


%%	math_fail(-Out:atom)
%	
%	returns code from $ to the next $ if math_input fails

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
	{add_error_message_once(inputError,math_fail,0,'Missing $.')}.


new_sentence_index(Index) :-
	retract(sentence_index(Index)),
    	succ(Index, NewIndex),
      	assertz(sentence_index(NewIndex)).


print_args([]).
print_args([A0|T]) :-
        A0 =.. [Name, Value],
        format('<B>~w</B>=<EM>~w</EM><BR>~n', [Name, Value]),
        print_args(T).

