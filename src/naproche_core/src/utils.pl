:- module(utils,[
	first/3,
	slice/4,
	line_and_index_from_id/3,
	clean/0,
	write_list_to_file/2,
	union_in_right_order/3,
	subtract_set/3,
	sublist/2,
	union_without_instantiation/3,
	member_without_instantiation/2,
	intersection_without_instantiation/3,
	subtract_without_instantiation/4,
	nth1_without_instantiation/3,
	member_subsumes/2,
	nth1_back/4,
	search_math_id/2,
	search_plural_dref/2,
	change_feature/4,
	make_n_copies/3
	]).

:- use_module(library(pldoc)).

% assumption marker
:- op(901, xfx, user:(=>)).

% implication marker
:- op(901, xfx, user:(==>)).

% definition marker
:- op(901, xfx, user:(:=)).
% or
:- op(901, xfx, user: (v)).

PRS_A := PRS_B :-
        is_prs(PRS_A),
        is_prs(PRS_B).

PRS_A => PRS_B :-
        is_prs(PRS_A),
        is_prs(PRS_B).

PRS_A ==> PRS_B :-
        is_prs(PRS_A),
        is_prs(PRS_B).


/**     <module>  contains predicates to be used in all parts of the code
 *
 *      This module contains predicates which are not restricted to only one part of the sourcecode, e.g prs or logic.
 */

%% first(-Element, +List, +Template)
% 
% Element is the first element of List which can be unified with Template. 
first(_, [], _).
first(Y, [Y|_], Template) :-
        Y = Template, !.
first(X, [_|Rest], Template) :-
        first(X, Rest, Template).

%% slice(+L1, +I, +K, ?L2)
%
% Slices the list L1 between and including the indices I and K and unifies the
% result with L2.
slice([X|_],1,1,[X]).
slice([X|Xs],1,K,[X|Ys]) :- K > 1, 
   K1 is K - 1, slice(Xs,1,K1,Ys).
slice([_|Xs],I,K,Ys) :- I > 1, 
   I1 is I - 1, K1 is K - 1, slice(Xs,I1,K1,Ys).


%% line_and_index_from_id(+Id, -Line, -Index)
%
% The internal representation of the proof sentences includes an id in the form
% '1.2.3.4-12', where the characters before the dash stand for the id of the
% line on which the sentence is and the second part (after the dash stands for
% the running index of the sentence in that line. So a sentence with an id of
% '1.2.3.4-12' would be the 12th sentence in the line with id 1.2.3.4.
%
% This predicate extracts both parts.
line_and_index_from_id(Id, Line, Index) :-
        atom_codes(Id, IdCharList),
        append(Line, "-", LineWithDash),
        append(LineWithDash, Index, IdCharList), !.


%%      clean
%
%       Deletes all files in /tmp

clean :-
	check_src(Loc),
	session_id(Id),
	concat_atom(['rm -f ',Loc,'/tmp/',Id,'/*'],Command),
        shell(Command),
        !.
clean :- !.


%%      write_file(OS:Stream)
%
%       Takes each character from OS and writes it on the command line.


write_file(OS) :-
        get0(OS,Int_Char),

        % If end_of_file then stop, else write char and continue
        ( Int_Char = -1 ->
                true
                ;
                ( name(New_Char,[Int_Char]),
                  write(New_Char),
                  write_file(OS)
                )
        ).

%%      write_list_to_file(+File,+MessageList)
%
%       write Message to log file
write_list_to_file(File,MessageList) :-
        MessageList=[Head|Tail],
        !,
        open(File,append,OS),
        write(OS,Head),
        write(OS,' '),
        close(OS),
        write_list_to_file(File,Tail).
write_list_to_file(_,[]) :- !.



%------------------- Union in right order -------------------------------
%%	union_in_right_order(+List1:list(DOBSOD),+List2:list(DOBSOD),Output:list(DOBSOD))
%
%	Unions List1 and List2 in the right order: Output consists of List1 and the elements of List2
%	that don't appear in List1.
%	
%	Example:
%
%	==
%	union_in_right_order([a,b,x,c],[x,y],X),
%	X = [a, b, x, c, y].
%	
%	==
%
%	@author Mona Rahn

union_in_right_order(List1,List2,Output) :- 
	subtract_set(List2,List1,List2WithoutList1),
	append(List1,List2WithoutList1,Output).


%%	subtract_set(+Minuend:list(DOBSOD),+Subtrahend:list(DOBSOD),Output:list(DOBSOD))
%
%	Output consists of the elements in List2 that don't appear in List1
%	while the order of the elements is preserved.
%
%	Example:
%	
%	==
%	
%	subtract_set([a,b,c],[c,d],[a,b]).
%
%	==
%
%	@author Mona Rahn

subtract_set(Minuend,[],Minuend) :- !.
subtract_set([],_,[]) :- !.

subtract_set(Minuend,Subtrahend,Difference) :-
	Minuend=[X|Rest],
	
	% subtract Subtrahend from the rest of Minuend
	subtract_set(Rest,Subtrahend,DifferenceRest),

	% add X if X doesn't appear in Subtrahend
	subtract_set_update_output(X,Subtrahend,DifferenceRest,Difference),
	!.

% subtract_set_update_output checks if X is a member of Subtrahend
% if no, X is added to Output
subtract_set_update_output(X,Subtrahend,DifferenceRest,Difference) :-
	member(X,Subtrahend),
	!,
	Difference = DifferenceRest.

subtract_set_update_output(X,_Subtrahend,DifferenceRest,Difference) :-
	Difference = [X|DifferenceRest].


%%  sublist(+X:list, +Y:list)
%
%   The predicate checks if the list is a sublist of the list Y. It uses the predicates "prefix" and "suffix" and the prolog predicate "append".

prefix(X,Y) :- append(X,_,Y).
suffix(X,Y) :- append(_,X,Y).
sublist(X,Y):- suffix(S,Y),prefix(X,S).


%%	union_without_instantiation(+List1,+List2,-UnionList)
%
%	UnionList is the union of List1 and List2. The order of the elements in UnionList is determined
%	by going through List 2 and List1 backwards (just as in the inbuilt predicate union/3). The
% 	difference to union/3 is that no instantiation is allowed.

union_without_instantiation([Head|Tail],List2,UnionTail) :-
	member_without_instantiation(Head,List2),
	!,
	union_without_instantiation(Tail,List2,UnionTail).

union_without_instantiation([Head|Tail],List2,[Head|UnionTail]) :-
	union_without_instantiation(Tail,List2,UnionTail).

union_without_instantiation([],List2,List2).

member_without_instantiation(X,[Head|_]) :-
	X == Head,
	!.

member_without_instantiation(X,[_|Tail]) :-
	member_without_instantiation(X,Tail).


%%	intersection_without_instantiation(+List1,+List2,-IntersectionList)
%
%	IntersectionList is the intersection of List1 and List2, created without 
%	instantiating any of the element in List1 or List2.

intersection_without_instantiation([Head|Tail],List2,IntersectionList) :-
	member_without_instantiation(Head,List2),
	!,
	IntersectionList = [Head|IntersectionTail],
	intersection_without_instantiation(Tail,List2,IntersectionTail).

intersection_without_instantiation([_|Tail],List2,IntersectionTail) :-
	intersection_without_instantiation(Tail,List2,IntersectionTail).

intersection_without_instantiation([],_,[]).


%%	subtract_without_instantiation(+List,+Delete,-Difference,-Rest)
%
%	Difference is the result of deleting Delete from List, without instantiating any element of
%	Delete or List. Rest is the difference between List and Rest.

subtract_without_instantiation([Head|Tail],Delete,Difference,[Head|RestTail]) :-
    member_without_instantiation(Head,Delete),
    !,
    subtract_without_instantiation(Tail,Delete,Difference,RestTail).

subtract_without_instantiation([Head|Tail],Delete,[Head|DifferenceTail],Rest) :-
    subtract_without_instantiation(Tail,Delete,DifferenceTail,Rest).

subtract_without_instantiation([],_,[],[]).

%%	nth1_without_instantiation(?Index,+List,?Element)
%
%	Element is the Index-th element of List.

nth1_without_instantiation(1,[Head|_],Element) :-
	Head == Element.

nth1_without_instantiation(N,[_|Tail],Element) :-
	\+ var(N),
	N > 0,
	M is N-1,
	nth1_without_instantiation(M,Tail,Element).

nth1_without_instantiation(N,[_|Tail],Element) :-
	nth1_without_instantiation(M,Tail,Element),
	N is M+1.

%%	member_subsumes(?Member,+List)
%
%	Member is checked to be an element of List. Member may be instantiated to an element of List,
%	but elements of List may not be instantiated to unify with Member.

member_subsumes(X,[Head|_]):-
	subsumes(X,Head),
	!.

member_subsumes(X,[_|Tail]) :-
	member_subsumes(X,Tail).


%%	nth1_back(+Index,+List,-Element,-Rest)
%
%	Element is the nth element of List, counting from the back. Rest is List without its 
%	nth element from the back. Counting starts at 1.

nth1_back(Index,List,Element,Rest) :-
	reverse(List,ReversedList),
	nth1(Index,ReversedList,Element),
	delete_nth1(Index,ReversedList,ReversedRest),
	reverse(ReversedRest,Rest).

delete_nth1(1,[_|Rest],Rest) :-
	!.

delete_nth1(N,[Head|Tail],[Head|RestTail]) :-
	M is N - 1,
	delete_nth1(M,Tail,RestTail).

%%  search_math_id(+MathId,+ConditionList)
%
%   This predicate is similar to the in-built member/2 predicate, only that the Drefs in math_id
%   conditions are not allowed to be instantiated (but the mrefs yes).

search_math_id(math_id(Dref1,Mref),[math_id(Dref2,Mref)|_]) :-
    Dref1 == Dref2.

search_math_id(MathId,[_|Tail]) :-
    search_math_id(MathId,Tail).

%%  search_plural_dref(+PluralDrefCond,+ConditionList)
%
%   This predicate is similar to the in-built member/2 predicate, only that the Drefs in
%   plural_dref conditions are not allowed to be instantiated (but the dref lists yes).

search_plural_dref(plural_dref(Dref1,DrefList),[plural_dref(Dref2,DrefList)|_]) :-
    Dref1 == Dref2.

search_plural_dref(PluralDrefCond,[_|Tail]) :-
    search_plural_dref(PluralDrefCond,Tail).

%%	change_feature(+FeatureStructureIn,+Feature,+Value,-FeatureStructureOut)
%
%	FeatureStructureOut is a copy of FeatureStructureIn, only that the value of the
%	feature Feature has been replaced by Value.

change_feature(FSIn,Feature,Value,FSOut) :-
	atom_concat(Feature,'~x',FeatureXAtom),
	term_to_atom(FeatureXTerm,FeatureXAtom),
	gulp4:g_tf(FeatureXTerm,FeatureXTranslatedTerm),
	FeatureXTranslatedTerm =.. [g__|FeatureXValueList],
	first_nonvar_in_list(FeatureXValueList,N),
	FSIn =.. [g__|ValueListIn],
	replace_nth(N,ValueListIn,g_(Value),ValueListOut),	
	FSOut =.. [g__|ValueListOut].

%%	first_nonvar_in_list(+List,-Index)
%
%	The Index-th element of List is the first non-variable element of List.

first_nonvar_in_list([Head|_],1):-
	\+ var(Head),
	!.

first_nonvar_in_list([_|Tail],N):-
	first_nonvar_in_list(Tail,M),
	N is M+1.

%%	delete_without_instantiation(+ListIn,+Element,-ListOut)
%
%	Similar to the in-built predicate delete/3, but without instantiation and 
%	deleting only the first occurence of Element in the list.

delete_without_instantiation([Head|Tail],Element,Tail) :-
	Head == Element,
	!.

delete_without_instantiation([Head|TailIn],Element,[Head|TailOut]) :-
	delete_without_instantiation(TailIn,Element,TailOut).

%%	replace_nth(+Index, +List, +Element, -NewList) is det
%
%	Replace the Nth (1-based) element of a list.
%	(From the Prolog 5.9.7 library record.pl)

replace_nth(1, [_|T], V, [V|T]) :- !.
replace_nth(I, [H|T0], V, [H|T]) :-
	I2 is I - 1,
	replace_nth(I2, T0, V, T).


%%	make_n_copies(+Elt,+Number,+DrefList,-ListOfCopies)
%
%	ListOfCopies is a list of length N of the form [Elt,Elt,...,Elt].
%	If the Elt isn't a PRS, these are perfect copies. If it is a PRSs,
%	the drefs introduced by Elt are not copied but left to be distinct
%	variables.

make_n_copies(PRS,N,ListOfCopies) :-
	is_prs(PRS),
	length(ListOfCopies,N),
	ListOfCopies = [PRS|Tail],
	append(ListOfCopiesWithoutLastElement,[_],ListOfCopies),
	maplist(copy_term,ListOfCopiesWithoutLastElement,Tail),
	maplist(identical_accbefore,Tail,ListOfCopiesWithoutLastElement).

make_n_copies(Elt,N,ListOfCopies) :-
	length(ListOfCopies,N),
	ListOfCopies = [Elt|Tail],
	append(ListOfCopiesWithoutLastElement,[_],ListOfCopies),
	maplist(=,ListOfCopiesWithoutLastElement,Tail).


identical_accbefore(accbefore~AccBefore..imagined_accbefore~ImaginedAccBefore,accbefore~AccBefore..imagined_accbefore~ImaginedAccBefore).
% To understand this "imagined_accbefore", read the comments at prs:dissolve_plural_prss/3 and in the second clause of prs:make_static_copies/4.
