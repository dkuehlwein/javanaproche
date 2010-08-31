:- module(development_utils,[
	write_sentences/1,
	check_error/1,
	find_prs_difference/2,
	display_tree/1,
	display_tree_list/1,
	check_dcg_error/0]).

:- use_module(library(pldoc)).

:- op(901,xfx, user:(=>)).
:- op(901,xfx, user:(==>)).
:- op(901,xfx, user:(:=)).
:- op(901,xfx, user:(<=>)).
:- op(901,xfx, user:(v)).
:- op(901,xfx, user:(<=)).
:- op(999,xfx, user:(::)).

/**	<module>	Predicates for the development process
 *
 * 	This module contains predicates which are only needed for the development process.
 */

%% write_sentences(+Sentences) is det
%
% Writes a list of Naproche sentences to the current output stream.
%
% The Sentences must be in the format
%
%==
% sentence('1.2.3-5', [assume, that, math('x'), is, positive, ...])
%==
%
write_sentences([]).
write_sentences([sentence(Id, Tokens)|Sentences]) :-
  format('~w ~t~15|', [Id]),
  write_sentence_tokens(Tokens), nl,
  write_sentences(Sentences).

write_sentence_tokens([]).
write_sentence_tokens([Token|Tokens]) :-
  atomic(Token), !, write(Token), write(' '), write_sentence_tokens(Tokens).
write_sentence_tokens([math(MathContent)|Tokens]) :-
  format(MathContent), write(' '), write_sentence_tokens(Tokens).

%%	check_error(+Msg:atom)
%
%	Checks if the Errors file contains Msg, and only Msg.
%	Used for testing whether predicaes throw the right errors, see
%	throw_error/1.

check_error(Msg) :-
	open('Errors',read,IS),
	read_file(IS,Error_Msg),
	name(Msg,Error_Msg),
	close(IS).

%%	read_file(IS:Stream,Msg:atom)
%
%	Reads IS into Msg.


read_file(IS,Msg) :-
	get0(IS,Int_Char),

	% If end_of_file then stop, else write char and continue
	( Int_Char = -1 ->
		Msg = ""
		;
		( 
		  read_file(IS,Old_Msg),
		  append([Int_Char],Old_Msg,Msg)
		)
	).

%%	find_prs_difference(+PRS1,+PRS2)
%
%	Searches for a difference between PRS1 and PRS2, and writes where the difference could be found.
%	Possible written output can be of the forms
%	* "ID difference: Id1, Id2" (for two different IDs Id1 and Id2)
%	* "difference in drefs/mrefs/rrefs/accbefore/accafter of PRS Id" (for one of the five mentioned PRS components and an ID Id)
%	* "difference in condition Number of PRS Id" (for some number Number and some ID Id)
%	The predicate fails if the two PRSs are identical.

find_prs_difference(PRS1,PRS2) :-
	PRS1 = id~Id1,
	PRS2 = id~Id2,
	\+ Id1 = Id2,
	!,
	term_to_atom(Id1,Id1Atom),
	term_to_atom(Id2,Id2Atom),
	concat_atom(['ID difference: ',Id1Atom,', ',Id2Atom],Message),
	write(Message).

find_prs_difference(PRS1,PRS2) :-
	PRS1 = id~Id,
	find_content_difference(Id,PRS1,PRS2).


find_content_difference(Id,PRS1,PRS2) :-
	PRS1 = drefs~Drefs1,
	PRS2 = drefs~Drefs2,
	\+ Drefs1 = Drefs2,
	!,
	term_to_atom(Id,IdAtom),
	atom_concat('difference in drefs of PRS ',IdAtom,Message),
	write(Message).

find_content_difference(Id,PRS1,PRS2) :-
	PRS1 = mrefs~Mrefs1,
	PRS2 = mrefs~Mrefs2,
	\+ Mrefs1 = Mrefs2,
	!,
	term_to_atom(Id,IdAtom),
	atom_concat('difference in mrefs of PRS ',IdAtom,Message),
	write(Message).

find_content_difference(Id,PRS1,PRS2) :-
	PRS1 = rrefs~Rrefs1,
	PRS2 = rrefs~Rrefs2,
	\+ Rrefs1 = Rrefs2,
	!,
	term_to_atom(Id,IdAtom),
	atom_concat('difference in rrefs of PRS ',IdAtom,Message),
	write(Message).

find_content_difference(Id,PRS1,PRS2) :-
	PRS1 = accbefore~Acc1,
	PRS2 = accbefore~Acc2,
	\+ Acc1 = Acc2,
	!,
	term_to_atom(Id,IdAtom),
	atom_concat('difference in accbefore of PRS ',IdAtom,Message),
	write(Message).

find_content_difference(Id,PRS1,PRS2) :-
	PRS1 = accafter~Acc1,
	PRS2 = accafter~Acc2,
	\+ Acc1 = Acc2,
	!,
	term_to_atom(Id,IdAtom),
	atom_concat('difference in accafter of PRS ',IdAtom,Message),
	write(Message).

find_content_difference(Id,PRS1,PRS2) :-
	PRS1 = conds~Conds1,
	PRS2 = conds~Conds2,
	find_condition_difference(Id,1,Conds1,Conds2).


find_condition_difference(Id,N,[Head|Tail1],[Head|Tail2]) :-
	!,
	M is N + 1,
	find_condition_difference(Id,M,Tail1,Tail2).

find_condition_difference(Id,N,[Head1|_],[Head2|_]) :-
	Head1 =.. [Type1|_],
	Head2 =.. [Type2|_],
	\+ Type1 = Type2,
	!,
	term_to_atom(Id,IdAtom),
	concat_atom(['difference in condition ',N,' of PRS ',IdAtom],Message),
	write(Message).

find_condition_difference(Id,N,[Head1|_],[Head2|_]) :-
	[Head1] = [F1 :: _ => _],
	[Head2] = [F2 :: _ => _],
	\+ F1 = F2,
	!,
	term_to_atom(Id,IdAtom),
	concat_atom(['difference in condition ',N,' of PRS ',IdAtom],Message),
	write(Message).

find_condition_difference(_,_,[Head1|_],[Head2|_]) :-
	[Head1] = [F :: PRS1A => PRS1B],
	[Head2] = [F :: PRS2A => PRS2B],
	!,
	find_prs_list_difference([PRS1A,PRS1B],[PRS2A,PRS2B]).

find_condition_difference(_,_,[Head1|_],[Head2|_]) :-
	is_prs(Head1),
	!,
	find_prs_difference(Head1,Head2).

find_condition_difference(Id,N,[Head|_],_) :-
	Head =.. [Type|_],
	( Type = math_id; Type = predicate; Type = holds ),
	!,
	term_to_atom(Id,IdAtom),
	concat_atom(['difference in condition ',N,' of PRS ',IdAtom],Message),
	write(Message).

find_condition_difference(_,_,[Head1|_],[Head2|_]) :-
	Head1 =.. [_|PRSList1],
	Head2 =.. [_|PRSList2],
	find_prs_list_difference(PRSList1,PRSList2).


find_prs_list_difference([Head1|Tail1],[Head2|Tail2]) :-
	\+ find_prs_difference(Head1,Head2),
	find_prs_list_difference(Tail1,Tail2).



%%	display_tree(+GulpFormulaTree)
%
%	Converts the GULP tree of a formula or term into a graph that can be viewed with DOT.
%	The svg is saved at /tmp/FormulaGraph.svg

display_tree(Tree) :-
	open('/tmp/FormulaGraph',write,Stream),
	write(Stream,'digraph G {\n'),
	display_single_tree(Stream,0,_,Tree),
	write(Stream,'}'),
	close(Stream),
	shell('dot -Tsvg /tmp/FormulaGraph -o /tmp/FormulaGraph.svg'),
	shell('opera /tmp/FormulaGraph.svg').

%%	display_tree_list(+List:gulp)
%
%	Creates a graph that of a list of GULP trees.
%	The svg is saved at /tmp/FormulaGraph.svg

display_tree_list(List) :-
	open('/tmp/FormulaGraph',write,Stream),
	write(Stream,'digraph G {\n'),
	display_tree_list_intern(Stream,0,_,List),
	write(Stream,'}'),
	close(Stream),
	shell('dot -Tsvg /tmp/FormulaGraph -o /tmp/FormulaGraph.svg'),
	shell('opera /tmp/FormulaGraph.svg').

display_tree_list_intern(_Stream,N,N,[]).

display_tree_list_intern(Stream,N,O,[Tree|Trees]) :-
	display_single_tree(Stream,N,M,Tree),
	display_tree_list_intern(Stream,M,O,Trees).

display_single_tree(Stream,N,O,args~Args..dref~Dref..name~Name) :-
	( var(Name) -> term_to_atom(Dref,NA) ; term_to_atom(Name,NA)),
        concat_atom(['"',N,'" [label = "',NA,'"]; \n'],Entry),
	write(Stream,Entry),
	M is N+1,
	write_args(Stream,M,O,N,Args).

write_args(_Stream,N,N,_Name,[]).

write_args(Stream,N,O,Name,[List|Args]) :-
	is_list(List),
	!,
	write_args(Stream,N,M,Name,List),
	write_args(Stream,M,O,Name,Args).

write_args(Stream,N,O,Name,[Arg|Args]) :-
    concat_atom(['"',Name,'"  ->  "', N,'"; \n'],Entry),
	write(Stream,Entry),
	display_single_tree(Stream,N,M,Arg),
	write_args(Stream,M,O,Name,Args).


%%	check_dcg_error
%
%	This predicates checks whether there are superficial differences between the codes of dcg.pl and dcg_error.pl.
%	Arguments, goals in curly brackets and clauses that aren't in DCG notation are ignored. A list of all differences is printed to the standard output.
%	Each dot in this output represents a clause that is equivalent in dcg.pl and dcg_error.pl.

% The main work is done by the sub-predicate simplify_code/2. It takes Prolog code in the form of Prolog terms, and produces a list of simplified clauses, in which all arguments, all goals in curly brackets and all clauses that aren't in DCG notation are left out. Next, we use delete_goals/3 and replace_goals/4 in order to remove some consistent differences between the simplified versions of dcg.pl and dcg_error.pl. Finally, we produce a list of all differences between the two clause lists, and write the differences to the standard output.

check_dcg_error :-
	read_file_to_terms('data/grammar/dcg.pl',DCG,[]),
	simplify_code(DCG,SimpleDCG),
	delete_goals(SimpleDCG,[new_prs,add_condition,rest_list,add_rref,add_dref,make_conditional_semantics],ShortDCG),
	replace_goals(ShortDCG,[copula,transitive_verb,intransitive_verb,transitive_verb_parser,intransitive_verb_parser,vp_working_on_np_coord,meta_vp_parser],[verb,verb,verb,verb,verb,vp,meta_vp],FinalDCG),
	read_file_to_terms('data/grammar/dcg_error.pl',DCGError,[]),
	simplify_code(DCGError,SimpleDCGError),
	delete_goals(SimpleDCGError,[call_position,record_position,rest_list],ShortDCGError),
	replace_goals(ShortDCGError,[identifier],[[_]],FinalDCGError),
	find_list_differences(FinalDCG,FinalDCGError,DifferenceList),
	write_differences(DifferenceList).

%	find_list_differences(+List1,+List2,-DifferenceList)
%
%	This predicate compares List1 and List2 and produces a list of the differences between them (DifferenceList).
%	Every agreement between the two lists is represented in DifferenceList by a pair of lists: The first list in this pair
%	contains all elements of List1 that had to be traversed before reaching the agreement; the second list in the pair 
%	contains all elements of List2 that had to be traversed before reaching the agreement. So when there is an agreement
%	at the beginning of the two lists or directly after another agreement, this will be represented by the pair [[],[]] (i.e.
%	no elements had to be traversed in order to reach this agreement).
%	When List1 and List2 contain two elements with switched positions, this is represented in a special way, namely by a triple
%	[s,Elt1,Elt2], where Elt1 and Elt2 are the switched elements in the order in which they appear in List1. (If this element 
%	switch does not occur at the beginning of the lists or after an agreement or another switch, then it won't be recongnised
%	 as a switch.)

find_list_differences([],[],[]).

find_list_differences([HeadA,HeadB|Tail1],[HeadB,HeadA|Tail2],[[s,HeadA,HeadB]|TailDifferences]) :-
	\+ HeadA = HeadB,
	find_list_differences(Tail1,Tail2,TailDifferences).

find_list_differences(List1,List2,[[Prefix1,Prefix2]|TailDifferences]) :-
	look_for_first_agreement(2,List1,List2,Prefix1,Prefix2,Tail1,Tail2),
	find_list_differences(Tail1,Tail2,TailDifferences).

%	look_for_first_agreement(+DistanceSum,+List1,+List2,-Prefix1,-Prefix2,-Tail1,-Tail2)
%
%	This predicate looks for the nearest agreement between elements in List1 and List2. An agreement of the
%	nth element of List1 and the mth element of List2 is nearer than an agreement of the kth element of List1
%	and the lth element of List2 iff n+m<k+l or n+m=k+l and n<m. The minimum value DistanceSum (n+m) can take
% 	is 2 (yes, we call the first element 1st, on not 0th), so we first look for an agreement at DistanceSum 2,
%	and add 1 to DistanceSum as long as no agreement is found.
%	When an agreement is found, the elements in List1 and List2 before the nearest agreement are listed in
%	Prefix1 and Prefix2 respectively, and the tails of List1 and List2 after the agreement unify with Tail1
%	and Tail2 respectively.

look_for_first_agreement(DistanceSum,List1,List2,Prefix1,Prefix2,Tail1,Tail2) :-
	length(List1,Length1),
	length(List2,Length2),
	make_number_pair_list(DistanceSum,Length1,Length2,PairList),
	!,
	( look_for_agreement(PairList,List1,List2,Prefix1,Prefix2,Tail1,Tail2); 
	( NewDistanceSum is DistanceSum+1, look_for_first_agreement(NewDistanceSum,List1,List2,Prefix1,Prefix2,Tail1,Tail2) )).

% We enter this case if make_number_pair_list fails, i.e. if DistanceSum is greater than Length1+Length2.
look_for_first_agreement(_,List1,List2,List1,List2,[],[]).
	
%	make_number_pair_list(+DistanceSum,+Length1,+Length2,-PairList)
%
%	Using the variables n and m as in the documentation of look_for_first_agreement/7, DistanceSum is n+m,
%	Length1 is the maximum value that n may take, Length2 is the maximum value m may take, and PairList lists
%	all possible values (>1) that n and m may take such that n+m is DistanceSum. We start with the minimum
%	possible value for n, and go in steps of [+1,-1] from there. For example, make_number_pair_list(10,8,7,PairList)
%	returns PairList = [[3,7],[4,6],[5,5],[6,4],[7,3],[8,2]]. 
%	The predicate fails if DistanceSum is greater than Length1+Length2.

make_number_pair_list(DistanceSum,Length1,Length2,PairList) :-
	StartIndex is max(1,DistanceSum-Length2),
	EndIndex is min(DistanceSum-1,Length1),
	EndIndex >= StartIndex,
	make_number_pair_list_intern(DistanceSum,StartIndex,EndIndex,PairList).

make_number_pair_list_intern(Sum,Index,EndIndex,[[Index,Difference]|TailPairList]) :-
	Difference is Sum - Index,
	( Index = EndIndex, !, TailPairList = [];
	NewIndex is Index + 1,
	make_number_pair_list_intern(Sum,NewIndex,EndIndex,TailPairList) ).

look_for_agreement([[Position1,Position2]|_],List1,List2,Prefix1,Prefix2,Tail1,Tail2) :-
	Prefix1Length is Position1 - 1,
	Prefix2Length is Position2 - 1,
	length(Prefix1,Prefix1Length),
	length(Prefix2,Prefix2Length),
	append(Prefix1,[Elt|Tail1],List1),
	append(Prefix2,[Elt|Tail2],List2),
	!.

look_for_agreement([_|Tail],List1,List2,Prefix1,Prefix2,Tail1,Tail2) :-
	look_for_agreement(Tail,List1,List2,Prefix1,Prefix2,Tail1,Tail2).


%	write_differences(+DifferenceList)
%
%	This predicate writes the content of DifferenceList to the standard output, so that it appears in 
%	a very readable format to the user. 

write_differences([[[],[]],[[],[]]|Tail]) :-
	!,
	write(':\n'),
	write_differences(Tail).

write_differences([[[],[]]|Tail]) :-
	!,
	write('.\n'),
	write_differences(Tail).

write_differences([[s,Elt1,Elt2]|Tail]) :-
	!,
	write('Switch: '),
	write(Elt1),
	write(', '),
	write(Elt2),
	write('\n'),
	write_differences(Tail).

write_differences([[DCGGoals,DCGErrorGoals]|Tail]) :-
	write('DCG: '),
	write(DCGGoals),
	write('\n'),
	write('DCGError: '),
	write(DCGErrorGoals),
	write('\n.\n'),
	write_differences(Tail).

write_differences([]).

%	delete_goals(+Clauses,+Goals,-ShortClauses)
%
%	This deletes all appearences of goals in Goals from the clauses in Clauses, unifying the result with ShortClauses.

delete_goals(Clauses,[Head|Tail],ShortClauses) :-
	delete_goal(Clauses,Head,TmpClauses),
	delete_goals(TmpClauses,Tail,ShortClauses).

delete_goals(Clauses,[],Clauses).

/*
delete_goal([Head|Tail],Goal,[Head|ShortTail]) :-
	var(Head),
	delete_goal(Tail,Goal,ShortTail).
*/

delete_goal([Head|Tail],Goal,[ShortHead|ShortTail]) :-
	delete(Head,Goal,ShortHead),
	delete_goal(Tail,Goal,ShortTail).

delete_goal([],_,[]).

%	replace_goals(+Clauses,+Goals,+Replacements,-NewClauses)
%
%	Goals and Replacements must be two lists of the same length n. For every m<=n, the nth element of Goals
%	is replaced in Clauses by the nth element of Replacements, yielding NewClauses.
%	Additionally, this predicate replaces goals that are atoms that represent variables by actual variables.

replace_goals(Clauses,[Head1|Tail1],[Head2|Tail2],NewClauses) :-
	replace_goal(Clauses,Head1,Head2,TmpClauses),
	replace_goals(TmpClauses,Tail1,Tail2,NewClauses).

replace_goals(Clauses,[],[],Clauses).

replace_goal([Head|Tail],Goal,Replacement,[Head|NewTail]) :-
	var(Head),
	replace_goal(Tail,Goal,Replacement,NewTail).

replace_goal([Head|Tail],Goal,Replacement,[NewHead|NewTail]) :-
	replace(Head,Goal,Replacement,NewHead),
	replace_goal(Tail,Goal,Replacement,NewTail).

replace_goal([],_,_,[]).

replace([Elt|Tail],var,Repl,[Repl|NewTail]) :-
	atom(Elt),
	term_to_atom(EltTerm,Elt),
	var(EltTerm),
	!,
	replace(Tail,Elt,Repl,NewTail).

replace([Elt|Tail],Elt,Repl,[Repl|NewTail]) :-
	!,
	replace(Tail,Elt,Repl,NewTail).

replace([Head|Tail],Elt,Repl,[Head|NewTail]) :-
	replace(Tail,Elt,Repl,NewTail).

replace([],_,_,[]).

%	simplify_code(+Code,-SimpleCode)
%
%	Code is Prolog code as Prolog terms. SimpleCode is a list of lists representing the clauses of the code,
%	with arguments, goal in curly brackets and clauses that aren't in DCG notation removed.

simplify_code(Code,SimpleCode) :-
	termlist_to_charcodelist(Code,CodeStringList),
	parse_code(TmpSimpleCode,CodeStringList,[]),
	restore_variables(TmpSimpleCode,SimpleCode).

%	restore_variables(+ListIn,-ListOut)
%
%	ListIn is a list of lists (which in our case represent clauses). If an element of an embedded list (i.e. a 
%	goal of a clause) is an atom that represents a variable (i.e. returns a variable when transformed into a term
%	using term_to_atom/2), then it is replaced by an actual variable. The same holds for variables that are
%	embedded in a list one level deeper.

restore_variables([HeadIn|TailIn],[HeadOut|TailOut]) :-
	restore_variables_in_single_clause(HeadIn,HeadOut),
	restore_variables(TailIn,TailOut).

restore_variables([],[]).

restore_variables_in_single_clause([HeadIn|TailIn],[_|TailOut]) :-
	% The first line is needed, for else the third will cause an error if HeadIn is '\''.
	\+ HeadIn = '\'',
	atom(HeadIn),
	term_to_atom(HeadInTerm,HeadIn),
	var(HeadInTerm),
	!,
	restore_variables_in_single_clause(TailIn,TailOut).

restore_variables_in_single_clause([HeadIn|TailIn],[HeadIn|TailOut]) :-
	\+ is_list(HeadIn),
	!,
	restore_variables_in_single_clause(TailIn,TailOut).

restore_variables_in_single_clause([HeadIn|TailIn],[HeadOut|TailOut]) :-
	restore_variables_in_list(HeadIn,HeadOut),
	restore_variables_in_single_clause(TailIn,TailOut).

restore_variables_in_single_clause([],[]).

restore_variables_in_list([HeadIn|TailIn],[_|TailOut]) :-
	% The first line is needed, for else the third will cause an error if HeadIn is '\''.
	\+ HeadIn = '\'',
	atom(HeadIn),
	term_to_atom(HeadInTerm,HeadIn),
	var(HeadInTerm),
	!,
	restore_variables_in_list(TailIn,TailOut).

restore_variables_in_list([Head|TailIn],[Head|TailOut]) :-
	restore_variables_in_list(TailIn,TailOut).

restore_variables_in_list([],[]).

%	termlist_to_charcodelist(+TermList,-CharCodeListList)
%
%	A list of terms is transformed into a list containing for each term in TermList a list of the character codes of the
%	characters in the atom that represents the term.

termlist_to_charcodelist([HeadIn|TailIn],[HeadOut|TailOut]) :-
	term_to_atom(HeadIn,HeadAtom),
	string_to_atom(HeadString,HeadAtom),
	string_to_list(HeadString,HeadOut),
	termlist_to_charcodelist(TailIn,TailOut).

termlist_to_charcodelist([],[]).

%	parse_code(+SimpleCode,+CodeList,-RestList)
%
%	This predicate parses Prolog code that is represented as a list of lists of character codes, and builds a simpler
%	representation of the code, SimpleCode. See the documentation of simplify_code/2 for details about SimpleCode.

parse_code([[call]|SimpleCode]) -->
	[CallClause],
	% Check whether ":-call" is contained in CallClause:
	{ sublist([58, 45, 99, 97, 108, 108],CallClause) },
	!,
	parse_code(SimpleCode).

parse_code(SimpleCode) -->
	[StandardNotationClause],
	% Check whether "-->" is contained in StandardNotationClause:
	{ \+ sublist([45,45,62],StandardNotationClause) },
	parse_code(SimpleCode).

parse_code([SimpleDCGClause|SimpleCode]) -->
	[DCGClause],
	{ parse_clause(SimpleDCGClause,DCGClause,[]) },
	!,
	parse_code(SimpleCode).

parse_code([]) --> [].

parse_clause([Head,'-->'|Body]) -->
	%'
	% The above comment line is for some strange reason needed in order to have the right syntax highlighting in vim.
	clause_head(Head),
	clause_body(Body).
	
clause_head(Head) -->
	goal(Head),
	[45, 45, 62].

goal(Goal) -->
	[32],
	!,
	goal(Goal).

goal(List) -->
	[91],
	!,
	goal_list(List),
	[93].

goal(Type) -->
	gulp_term,
	goal_tail(Type),
	!.

goal(Predicate) -->
	predicate(PredicateString),
	{ string_to_atom(PredicateString,Predicate) },
	arguments.

goal_tail(univ) -->
	[61,46,46],
	!,
	gulp_term.

goal_tail(equality) -->
	[61],
	!,
	gulp_term.

goal_list([Head|Tail]) -->
	goal(Head),
	!,
	goal_list_tail(Tail).

goal_list([]) -->
	[],
	!.

goal_list_tail(Tail) -->
	[44],
	!,
	goal_list(Tail).

goal_list_tail(Tail) -->
	[124],
	!,
	goal(Tail).

goal_list_tail([]) --> [].

arguments -->
	[40],
	!,
	terms,
	[41].

arguments --> [].

terms -->
	gulp_term,
	terms_tail.

terms_tail -->
	[44],
	!,
	terms.

terms_tail -->
	[124],
	!,
	terms.

terms_tail --> [].

gulp_term -->
	[32],
	!,
	gulp_term.

gulp_term -->
	term,
	[46,46],
	!,
	gulp_term.

gulp_term -->
	term.

term -->
	[39],
	atom,
	[39].

term -->
	[40,61,41],
	!,
	term.

term -->
	[40],
	terms,
	[41],
	!.

term -->
	[40],
	!,
	goal_list(_),
	[41].

term -->
	[91],
	terms,
	[93],
	!,
	gulp_term.

term -->
	[91],
	!,
	goal_list(_),
	[93],
	gulp_term.

term -->
	[61,62],
	!,
	term.

term -->
	[61,61,62],
	!,
	term.

term -->
	[58,61],
	!,
	term.

term -->
	[60,61],
	!,
	term.

term -->
	[114, 101, 115, 116, 45],
	!,
	term.

term -->
	[116,111,45],
	!,
	term.

term -->
	[Symbol],
	% Check that Symbol isn't ",", ";", "-", ".", "=", ")", "[", "]", "|", "{" or "}":
	{ \+ Symbol = 44, \+ Symbol = 59, \+ Symbol = 45, \+ Symbol = 46, \+ Symbol = 61, \+ Symbol = 41, \+ Symbol = 91, \+ Symbol = 93, \+ Symbol = 124, \+ Symbol = 123, \+ Symbol = 125, ! },
	term.

term --> [].

atom -->
	[Symbol],
	{ \+ Symbol = 39 },
	atom.

atom --> [].

%predicate([46,46|Tail]) -->
%	[46,46],
%	!,
%	predicate_tail(Tail).

predicate([Head|Tail]) -->
	[Head],
	% Check that Head isn't ",", ";", "(", ")", "-", "[", "]", "|", "{" or "}":
	{ \+ Head = 44, \+ Head = 59, \+ Head = 40, \+ Head = 41, \+ Head = 45, \+ Head = 91, \+ Head = 93, \+ Head = 124, \+ Head = 123, \+ Head = 125, ! },
	predicate_tail(Tail).

predicate_tail(Predicate) -->
	predicate(Predicate),
	!.

predicate_tail([]) --> [].

clause_body(Body) -->
	[32],
	!,
	clause_body(Body).

clause_body(Body) -->
	[40],
	clause_body(Body1),
	[41,44],
	!,
	clause_body(Body2),
	{ append(Body1,Body2,Body) }.
	
clause_body(Body) -->
	[40],
	!,
	clause_body(Body),
	[41].

clause_body(Body) -->
	[123],
	clause_body(_),
	[125,44],
	!,
	clause_body(Body).

clause_body([]) -->
	[123],
	!,
	clause_body(_),
	[125].

clause_body([Goal|Tail]) -->
	goal(Goal),
	[44],
	!,
	clause_body(Tail).

clause_body(['\\+'|Tail]) -->
	[92,43],
	!,
	clause_body(Tail).

clause_body([Goal,';'|Tail]) -->
	goal(Goal),
	[59],
	!,
	clause_body(Tail).

clause_body([Goal,'->'|Tail]) -->
	goal(Goal),
	[45,62],
	!,
	clause_body(Tail).

clause_body([Goal]) -->
	goal(Goal).


%%	find_sub_prs(+Id,+PRS,-SubPRS)
%
%	SubPRS is the SubPRS of PRS with ID Id.

find_sub_prs(Id,PRS,PRS) :-
	PRS = id~Id.

find_sub_prs(Id,PRS,SubPRS) :-
	PRS = conds~Conds,
	find_sub_prs_in_conds(Id,Conds,SubPRS).

find_sub_prs_in_conds(Id,[Head|Tail],SubPRS) :-
	is_prs(Head),
	(
		find_sub_prs(Id,Head,SubPRS)
		;
		find_sub_prs_in_conds(Id,Tail,SubPRS)
	).

find_sub_prs_in_conds(Id,[Head|Tail],SubPRS) :-
	Head =.. [_|Args],
	(
		find_sub_prs_in_cond_args(Id,Args,SubPRS)
		;
		find_sub_prs_in_conds(Id,Tail,SubPRS)
	).

find_sub_prs_in_cond_args(Id,[Head|Tail],SubPRS) :-
	(
		( is_prs(Head), find_sub_prs(Id,Head,SubPRS) )
		;
		find_sub_prs_in_cond_args(Id,Tail,SubPRS)
	).	
