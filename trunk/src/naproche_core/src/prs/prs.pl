:- module(prs,[is_prs/1,build_prs/2]).

:- use_module(naproche(dcg)).
:- use_module(naproche(dcg_utils)).
:- use_module(naproche(dcg_error)).
:- use_module(naproche(dcg_error_utils)).
:- use_module(naproche(math_lexicon)).
:- use_module(naproche(utils)).

:- op(901,xfx, user:(=>)).
:- op(901,xfx, user:(==>)).
:- op(901,xfx, user:(:=)).
:- op(901,xfx, user:(<=>)).
:- op(901,xfx, user:(v)).
:- op(901,xfx, user:(<=)).
:- op(999,xfx, user:(::)).

/**     <module> PRS construction

This module defines the main predicate for building PRSs.

INPUT:

The input is a prolog list of sentences. A sentence is always of the form sentence(Id,List), 
where Id is a number and List is a prolog list with the sentence content. An item of that
list can be of the following forms:

* An atom consisting of small letters (representing an English word)
* math(List), where List is a prolog list with mathematical content
* ','
* 'i.e.'
* An atom consisting of numbers and dots (as a name of an axiom, lemma or theorem)

Every item in the list in math(List) is either a comma or in math_lexicon.


OUTPUT:

A PRS is a seven-tupel of GULP-features:

* id is a unique number or compound term identifying the PRS. It is of the form n or
f(n) or f(g(n)) or ..., where n is a number, and f, g, ... are prolog predicates
(e.g.lemma, conseq, comma_disjunct).
* rrefs is a list of PRS ids.
* drefs is a list of numbers. (Before applying create_drefs/1, i.e. when leaving
naproche_text/4, it is a list of uninstantiated variables-.)
* mrefs is a list of trees of variables, terms and/or formulas.
* accbefore and accafter are lists of compound terms of the form math_id(n,tree), 
where n is a number (a dref) and tree is a tree of a variable, a term or a formula.
* conds is a list of PRS conditions

A PRS condition is one of the following:

* holds(n), where n is a number (a dref).
* math_id(n,tree), where n is a number (a dref) and tree is a tree of a variable,
term or formula
* predicate(n,atom), where n is a number (a dref) and atom is an atom.
* predicate(m,n,atom), where m and n are numbers (drefs) and atom is an atom.
* contradiction.
* plural_dref(n,list), where n is a number (a dref) and list is a list of drefs.
* B, where B is a PRS.
* B=>C, where B and C are PRSs.
* B==>C, where B and C are PRSs.
* B<=>C, where B and C are PRSs.
* B<=C, where B and C are PRSs.
* B:=C, where B and C are PRSs.
* f::B=>C, where f is a function symbol and B and C are PRSs.
* BvC, where B and C are PRSs.
* ><(PRSList), where PRSList is a prolog list of PRSs
* <>(PRSList), where PRSList is a prolog list of PRSs
* neg(B), where B is a PRS.
* the(n,B), where n is a number (a dref) and B is a PRS.
* static(B), where B is a PRS.

During the PRS construction, there is additionally a temporary kind of PRS condition, 
which is always resolved during the construction algorithm:

* plural(n,B), where n is a number (a dref) and B is a PRS.

@author Marcos Cramer
*/

%%	build_prs(-PRS,+Text)
%
%	Builds the PRS for an input text.

build_prs(PRS,Text):-
%	dcg_simple:naproche_text(Text,[]),
	reset_progress_record(sentences,Text),
	( naproche_text(_,Text,[]), !;
	  dcg_error_utils:write_errors(Text), fail ),
	error_logger:setval(refids,[]),
	error_logger:setval(var_types,[]),
	InPRS = id~0..accbefore~[]..drefs~[]..mrefs~[]..rrefs~[]..conds~[]..accafter~[]..dref_cond_links~[],
	naproche_text(InPRS,TmpPRS,_,Text,[]),
	!,
	dissolve_grouped_args(TmpPRS,TmpPRS2,[],DrefPairs),
	!,
	dissolve_plural_prss(TmpPRS2,TmpPRS3,DrefPairs),
	!,
	reverse_prs(TmpPRS3,PRS),
	!,
	create_drefs(PRS,1,_),
	!.


%% 	is_prs(+What) is det
%
% 	True if What looks like a GULP encoded PRS (with no uninstantiated variables).
%
% 	It is defined as:
%
%	==
% 	is_prs(What) :-
% 	 subsumes_chk(id~_..drefs~_..mrefs~_..conds~_..rrefs~_, What).
%	==

is_prs(What) :-
 subsumes_chk(id~_..drefs~_..mrefs~_..conds~_..rrefs~_, What).

%% 	is_unistantiated_prs(+What) is det
%
% 	True if What looks like a GULP encoded PRS, possibly containing uninstantiated variables.
%

is_uninstantiated_prs(What) :-
 \+ \+ id~_..drefs~_..mrefs~_..conds~_..rrefs~_ = What.

%% 	reverse_prs(+PRSIn,-PRSOut)
%
%	Reverses the conditions, drefs, mrefs and the two accessibles lists of PRSIn and of all its sub-PRSs.

reverse_prs(PRSIn,PRSOut):-
	PRSIn = id~Id..drefs~DrefsIn..mrefs~MrefsIn..rrefs~RrefsIn..accbefore~AccBeforeIn..accafter~AccAfterIn..conds~CondsIn,
	reverse(DrefsIn,DrefsOut),
	reverse(MrefsIn,MrefsOut),
	reverse(RrefsIn,RrefsOut),
	reverse(AccBeforeIn,AccBeforeOut),
	reverse(AccAfterIn,AccAfterOut),
	reverse(CondsIn,TmpCondsOut),
	change_subprss(reverse_prs,TmpCondsOut,CondsOut),
	PRSOut = id~Id..drefs~DrefsOut..mrefs~MrefsOut..rrefs~RrefsOut..accbefore~AccBeforeOut..accafter~AccAfterOut..conds~CondsOut.

%%	create_drefs(?PRS,+CounterIn,-CounterOut)
%
%	PRS enters with uninstantiated drefs. The drefs are instantiated to an 
%	increasing index.

create_drefs(PRS,CIn,COut):-
	PRS = drefs~Drefs..conds~Conds,
	make_indexes(Drefs,CIn,NewCIn),
	create_drefs_in_conds(Conds,NewCIn,COut).

create_drefs_in_conds([Head|Tail],CIn,COut):-
	is_prs(Head),
	create_drefs(Head,CIn,NewCIn),
	create_drefs_in_conds(Tail,NewCIn,COut).

create_drefs_in_conds([the(Dref,PRS)|Tail],CIn,COut):-
	make_indexes([Dref],CIn,C1),
	create_drefs(PRS,C1,NewCIn),
	create_drefs_in_conds(Tail,NewCIn,COut).

create_drefs_in_conds([static(PRS)|Tail],CIn,COut):-
	create_drefs(PRS,CIn,NewCIn),
	create_drefs_in_conds(Tail,NewCIn,COut).

create_drefs_in_conds([plural(_,PRS)|Tail],CIn,COut):-
	create_drefs(PRS,CIn,NewCIn),
	create_drefs_in_conds(Tail,NewCIn,COut).

create_drefs_in_conds([theorem(_,PRS1,PRS2)|Tail],CIn,COut):-
	create_drefs(PRS1,CIn,C1),
	create_drefs(PRS2,C1,NewCIn),
	create_drefs_in_conds(Tail,NewCIn,COut).

create_drefs_in_conds([Head|Tail],CIn,COut):-
	Head =.. [_Operator,Head1,Head2],
	is_prs(Head1),
	is_prs(Head2),
	!,
	create_drefs(Head1,CIn,C1),
	create_drefs(Head2,C1,NewCIn),
	create_drefs_in_conds(Tail,NewCIn,COut).

create_drefs_in_conds([_ :: Head1 => Head2|Tail],CIn,COut):-
	create_drefs(Head1,CIn,C1),
	create_drefs(Head2,C1,NewCIn),
	create_drefs_in_conds(Tail,NewCIn,COut).

create_drefs_in_conds([><(HeadList)|Tail],CIn,COut):-
	create_drefs_in_conds(HeadList,CIn,NewCIn),
	create_drefs_in_conds(Tail,NewCIn,COut).

create_drefs_in_conds([<>(HeadList)|Tail],CIn,COut):-
	create_drefs_in_conds(HeadList,CIn,NewCIn),
	create_drefs_in_conds(Tail,NewCIn,COut).

create_drefs_in_conds([neg(Head)|Tail],CIn,COut):-
	create_drefs(Head,CIn,NewCIn),
	create_drefs_in_conds(Tail,NewCIn,COut).

create_drefs_in_conds([_|Tail],CIn,COut):-
	create_drefs_in_conds(Tail,CIn,COut).

create_drefs_in_conds([],CIn,CIn).

make_indexes([Head|Tail],CIn,COut):-
	Head is CIn,
	NewCIn is CIn+1,
	make_indexes(Tail,NewCIn,COut).

make_indexes([],CIn,CIn).


%%	dissolve_grouped_args(+PRSIn,-PRSOut,+DrefPairsIn,-DrefPairsOut)
%
%	All predicate conditions with grouped arguments get dissolved.
%	If the grouped argument is a plural dref linked to some dref list,
%	all pairs of drefs from that list are used for the new conditions.
%	Else an implication condition is created.

dissolve_grouped_args(
	id~Id..drefs~Drefs..mrefs~Mrefs..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links..conds~CondsIn,
	id~Id..drefs~Drefs..mrefs~Mrefs..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links..conds~CondsOut,
	DrefPairsIn,
	DrefPairsOut
) :-
	dissolve_grouped_args_in_conds(AccAfter,DrefPairsIn,DrefPairsOut,CondsIn,CondsOut).

%%	dissolve_grouped_args_in_conds(+Acc,+DrefPairsIn,-DrefPairsOut,+CondsIn,-CondsOut)
%
%	This predicate works through the condition list and dissolves all
%	predicate conditions with grouped arguments found in it.
%	DrefPairs saves a list of pairs of the form [Dref,DrefPair], where 
%	Dref is an undefined plural dref and DrefPair is the pair of drefs
%	replacing it in a grouped cond.

dissolve_grouped_args_in_conds(Acc,DrefPairsIn,DrefPairsOut,[Head|TailIn],CondsOut) :-
	Head =.. [predicate|Args],
	length(Args,N),
	last(Args,Predicate),
	% Args contains the predicate's name. So if N is the arity of Predicate,
	% then this is a condition with grouped arguments.
	math_lexicon(Predicate,type~relation..arity~N..grouped~I),
	dissolve_grouped_arg_cond(Acc,Args,I,DrefPairsIn,TmpDrefPairs,NewConds),
	dissolve_grouped_args_in_conds(Acc,TmpDrefPairs,DrefPairsOut,TailIn,TailOut),
	append(NewConds,TailOut,CondsOut).

dissolve_grouped_args_in_conds(Acc,DrefPairsIn,DrefPairsOut,[HeadIn|TailIn],[HeadOut|TailOut]) :-
	is_prs(HeadIn),
	dissolve_grouped_args(HeadIn,HeadOut,DrefPairsIn,TmpDrefPairs),
	dissolve_grouped_args_in_conds(Acc,TmpDrefPairs,DrefPairsOut,TailIn,TailOut).

dissolve_grouped_args_in_conds(Acc,DrefPairsIn,DrefPairsOut,[HeadIn|TailIn],[HeadOut|TailOut]) :-
	HeadIn =.. [Functor|ArgsIn],
	length(ArgsIn,N),
	length(DrefPairsList,N),
	DrefPairsList = [DrefPairsIn|DrefPairsTail],
	append(DrefPairsTail,[TmpDrefPairs],DrefPairsListOut),
	maplist(dissolve_grouped_args_in_cond_arg,ArgsIn,ArgsOut,DrefPairsList,DrefPairsListOut),
	HeadOut =.. [Functor|ArgsOut],
	dissolve_grouped_args_in_conds(Acc,TmpDrefPairs,DrefPairsOut,TailIn,TailOut).

dissolve_grouped_args_in_conds(_,DrefPairs,DrefPairs,[],[]).

%%	dissolve_grouped_args_in_cond_arg(ArgIn,ArgOut,DrefPairsIn,DrefPairsOut)
%
%	If ArgIn is a PRS, this predicate does the same as dissolve_grouped_args.
%	Else ArgsOut is ArgsIn.

dissolve_grouped_args_in_cond_arg(ArgIn,ArgOut,DrefPairsIn,DrefPairsOut) :-
	is_prs(ArgIn),
	dissolve_grouped_args(ArgIn,ArgOut,DrefPairsIn,DrefPairsOut).

dissolve_grouped_args_in_cond_arg(Arg,Arg,DrefPairs,DrefPairs).

%%	dissolve_grouped_arg_cond(+Acc,+Args,+Index,+DrefPairListIn,-DrefPairListOut,-NewConds)
%
%	If the Index-th element of Args is a plural dref linked to some dref
%	list in Acc, then for any pair of drefs from that list we create a
%	predicate condition in NewConds. Else NewConds consists just of an
%	implication condition.

dissolve_grouped_arg_cond(Acc,Args,I,DrefPairList,DrefPairList,NewConds) :-
	nth1(I,Args,Dref),
	search_plural_dref(plural_dref(Dref,DrefList),Acc),
	make_pairs(DrefList,DrefPairs),
	maplist(make_dissolved_cond(Args,I),DrefPairs,NewConds).

dissolve_grouped_arg_cond(_,Args,I,DrefPairListIn,DrefPairListOut,[NewCond]) :-
	nth1(I,Args,Dref),
	( search_in_dref_function([Dref,DrefPair],DrefPairListIn) -> DrefPairListOut = DrefPairListIn ; DrefPairListOut = [[Dref,DrefPair]|DrefPairListIn] ),
	make_dissolved_cond(Args,I,DrefPair,NewCond).

%%	make_pairs(+List,-PairList)
%
%	For every pair of elements E1 and E2 from List, we
%	add a pair [E1,E2] to PairList.

make_pairs([Head|Tail],PairList) :-
	reverse(Tail,ReversedTail),
	maplist(make_pair(Head),ReversedTail,NewPairs),
	make_pairs(Tail,TailPairList),
	append(TailPairList,NewPairs,PairList).

make_pairs([],[]).

make_pair(X,Y,[X,Y]).

%%	search_in_dref_function(+Pair,+Function)
%
%	Function is a list of pairs of the form [Dref,Y]. This predicate succeeds if Pair is
%	one of the pairs in Function, allowing its seconds element to instantiate but not its 
%	first.

search_in_dref_function([Dref1,Y],[[Dref2,Y]|_]) :-
    Dref1 == Dref2,
    !.

search_in_dref_function(Pair,[_|Tail]) :-
    search_in_dref_function(Pair,Tail).

%	make_dissolved_cond(+Args,+Index,+DrefPair,-Condition)
%
%	The Index-th element of Args is replaced by two elements,
%	namely the two drefs in DrefPair. The result prefixed with
%	"predicate" is the Condition.

make_dissolved_cond(Args,I,[Dref1,Dref2],Cond) :-
	J is I-1,
	length(List,J),
	append(List,[_|Tail],Args),
	append(List,[Dref1,Dref2|Tail],NewArgs),
	Cond =.. [predicate|NewArgs].


%%	dissolve_plural_prss(+PRSIn,-PRSOut,+DrefPairs)
%
%	Every PRS condition of the form "plural(PluralDref,PRS)" contained in PRSIn and its sub-PRSs is replaced
%	by copies of "static(PRS)" - one copy for each dref that PluralDref refers to. Plural drefs that aren't 
%	defined are considered to refer to the dref pair to which it is mapped by DrefPairs, or to a single dref
%	if it is not mentioned in DrefPairs (to avoid having to rewrite the accessible lists, we chose the PluralDref
%	itself as the single Dref that it refers to).
%	When an undefined plural dref refers to a dref pair, this dref pair has to be introduced at the point where the
%	plural dref was originally introduced, and the original plural dref has to be replaced by this dref pair in
%	all accessible lists in which it appears.
%	The subpredicates for this predicate are similar in structure to the subpredicates for dissolve_grouped_args/4,
%	and are therefore not documented in detail.

dissolve_plural_prss(
	id~Id..drefs~DrefsIn..mrefs~Mrefs..rrefs~Rrefs..accbefore~AccBeforeIn..accafter~AccAfterIn..dref_cond_links~Links..conds~CondsIn,
	id~Id..drefs~DrefsOut..mrefs~Mrefs..rrefs~Rrefs..accbefore~AccBeforeOut..accafter~AccAfterOut..dref_cond_links~Links..conds~CondsOut..imagined_accbefore~ImaginedAccBefore,
	DrefPairs
) :-
	dissolve_pp_in_conds(CondsIn,CondsOut,DrefPairs),
	(
		DrefPairs = []
		->
		[DrefsIn,AccBeforeIn,AccAfterIn] = [DrefsOut,AccBeforeOut,AccAfterOut]
		;
		% We replace all plural drefs in the domain of DrefPairs by the dref pairs to which DrefPairs
		% maps them.
		% The ImaginedAccBefore is needed, because for the purpose of make_static_copies the replaced
		% drefs have to be considered to still be part of accbefore. (See also the comment in the second
		% clause of make_static_copies.)
		maplist(replace_dref_by_dref_pair(DrefPairs),[DrefsIn,AccBeforeIn,AccAfterIn],[DrefsOut,AccBeforeOut,AccAfterOut],[_,ImaginedAccBefore,_])
	).
	
dissolve_pp_in_conds([plural(PluralDref,PRS)|TailIn],CondsOut,DrefPairs) :-
	make_static_copies(PluralDref,PRS,DrefPairs,StaticConds),
	dissolve_pp_in_conds(TailIn,TailOut,DrefPairs),
	append(StaticConds,TailOut,CondsOut).

dissolve_pp_in_conds([HeadIn|TailIn],[HeadOut|TailOut],DrefPairs) :-
	is_prs(HeadIn),
	dissolve_plural_prss(HeadIn,HeadOut,DrefPairs),
	dissolve_pp_in_conds(TailIn,TailOut,DrefPairs).

dissolve_pp_in_conds([HeadIn|TailIn],[HeadOut|TailOut],DrefPairs) :-
	HeadIn =.. [Functor|ArgsIn],
	maplist(dissolve_pp_in_cond_arg(DrefPairs),ArgsIn,ArgsOut),
	HeadOut =.. [Functor|ArgsOut],
	dissolve_pp_in_conds(TailIn,TailOut,DrefPairs).

dissolve_pp_in_conds([],[],_).


dissolve_pp_in_cond_arg(DrefPairs,ArgIn,ArgOut) :-
	is_prs(ArgIn),
	!,
	dissolve_plural_prss(ArgIn,ArgOut,DrefPairs).

dissolve_pp_in_cond_arg(_,Arg,Arg).

%%	make_static_copies(+PluralDref,+PRS,+DrefPairs,-StaticConds)
%
%	If the accbefore list of PRS contains a condition of the form plural_dref(PluralDref,[Dref1,...,DrefN]),
%	make N copies of "static(PRS)", replacing PluralDref by DrefI in the Ith copy.
%	Else, if PluralDref is mapped to [Dref1,Dref2] by DrefPairs, make two copies of "static(PRS)", replacing
%	PluralDref by DrefI in the Ith copy, and add a condition stating that Dref1 and Dref2 are distinct.
%	Else just make the condition "static(PRS)".

make_static_copies(PluralDref,PRS,DrefPairs,StaticConds) :-
	PRS = accbefore~AccBefore,
	search_plural_dref(plural_dref(PluralDref,DrefList),AccBefore),
	dissolve_plural_prss(PRS,DissolvedPRS,DrefPairs),
	length(DrefList,N),
	make_n_copies(PluralDref,N,PluralDrefCopies),
	make_n_copies(DissolvedPRS,N,PRSCopies),
	maplist(substitute_dref,PluralDrefCopies,DrefList,PRSCopies,TmpPRSList),
	add_counters_to_ids(1,TmpPRSList,PRSList),
	make_prs_list_static(PRSList,ReversedStaticConds),
	reverse(ReversedStaticConds,StaticConds).

make_static_copies(PluralDref,PRS,DrefPairs,[predicate(Dref2,Dref1,[distinct])|StaticConds]) :-
	search_in_dref_function([PluralDref,DrefPair],DrefPairs),
	DrefPair = [Dref1,Dref2],
	dissolve_plural_prss(PRS,DissolvedPRS,DrefPairs),
	% PluralDref has been removed from accbefore of DissolvedPRS, but still appears in
	% its conditions. In order to preserve it when it gets copied by "make_n_copies", we
	% have to give it an "imagined_accbefore".
	DissolvedPRS = imagined_accbefore~[PluralDref],
	make_n_copies(PluralDref,2,PluralDrefCopies),
	make_n_copies(DissolvedPRS,2,PRSCopies),
	maplist(substitute_dref,PluralDrefCopies,DrefPair,PRSCopies,TmpPRSList),
	add_counters_to_ids(1,TmpPRSList,PRSList),
	make_prs_list_static(PRSList,ReversedStaticConds),
	reverse(ReversedStaticConds,StaticConds).

make_static_copies(_,PRS,_,[static(PRS)]).
	

make_prs_list_static([Head|TailIn],[static(Head)|TailOut]) :-
	make_prs_list_static(TailIn,TailOut).

make_prs_list_static([],[]).

%%	add_counters_to_ids(+Counter,+PRSListIn,-PRSListOut)
%
%	If Counter is 1, the ID of the Nth PRS and all its sub-PRSs in PRSListIn gets prefixed with "sN".

add_counters_to_ids(N,[HeadIn|TailIn],[HeadOut|TailOut]) :-
	term_to_atom(N,NAtom),
	atom_concat('s',NAtom,Prefix),
	prefix_ids(Prefix,HeadIn,HeadOut),
	M is N+1,
	add_counters_to_ids(M,TailIn,TailOut).

add_counters_to_ids(_,[],[]).


prefix_ids(P,PRSIn,PRSOut) :-
	PRSIn = id~IdIn..conds~CondsIn,
	IdOut =.. [P,IdIn],
	change_subprss(prefix_ids(P),CondsIn,CondsOut),
	change_feature(PRSIn,id,IdOut,TmpPRS),
	change_feature(TmpPRS,conds,CondsOut,PRSOut).
	
%%  substitute_dref(+Dref1,+Dref2,+PRSIn,-PRSOut)
%
%   All occurences of Dref1 in conds in the PRS and its sub-PRSs are substituted by Dref2.

substitute_dref(
	Dref1,
	Dref2,
	id~Id..drefs~Drefs..mrefs~Mrefs..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links..conds~CondsIn,
	id~Id..drefs~Drefs..mrefs~Mrefs..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links..conds~CondsOut
) :-
	substitute_dref_in_conds(Dref1,Dref2,CondsIn,CondsOut).


substitute_dref_in_conds(Dref1,Dref2,[HeadIn|TailIn],[HeadOut|TailOut]) :-
	is_prs(HeadIn),
	substitute_dref(Dref1,Dref2,HeadIn,HeadOut),
	substitute_dref_in_conds(Dref1,Dref2,TailIn,TailOut).

substitute_dref_in_conds(Dref1,Dref2,[HeadIn|TailIn],[HeadOut|TailOut]) :-
	HeadIn =.. [Functor|ArgsIn],
	maplist(substitute_dref_in_cond_arg(Dref1,Dref2),ArgsIn,ArgsOut),
	HeadOut =.. [Functor|ArgsOut],
	substitute_dref_in_conds(Dref1,Dref2,TailIn,TailOut).

substitute_dref_in_conds(_,_,[],[]).


substitute_dref_in_cond_arg(Dref1,Dref2,ArgIn,ArgOut) :-
	is_prs(ArgIn),
	!,
	substitute_dref(Dref1,Dref2,ArgIn,ArgOut).

substitute_dref_in_cond_arg(Dref1,Dref2,Arg,Dref2) :-
	Dref1 == Arg.
	
substitute_dref_in_cond_arg(_,_,Arg,Arg).

%%	replace_dref_by_dref_pair(+DrefPairs,+ListIn,-ListOut,-ReplacedDrefs)
%
%	For any [Dref,[Dref1,Dref2]] in DrefPairs, replace all occurences of Dref in ListIn
%	by Dref1,Dref2. ReplacedDrefs lists all drefs that were actually replaced.

replace_dref_by_dref_pair(DrefPairs,[Head|TailIn],[Dref1,Dref2|TailOut],[Head|ReplacedDrefsTail]) :-
	search_in_dref_function([Head,[Dref2,Dref1]],DrefPairs),
	replace_dref_by_dref_pair(DrefPairs,TailIn,TailOut,ReplacedDrefsTail).

replace_dref_by_dref_pair(DrefPairs,[Head|TailIn],[Head|TailOut],ReplacedDrefs) :-
	replace_dref_by_dref_pair(DrefPairs,TailIn,TailOut,ReplacedDrefs).

replace_dref_by_dref_pair(_,[],[],[]).


%%  change_subprss(+Predicate,+PRSCondsIn,-PRSCondsOut)
%
%   Predicate is a 2-place predicate with a PRS input and a PRS output. Predicate is
%   applied to all sup-prss in PRSCondsIn.

change_subprss(P,[HeadIn|TailIn],[HeadOut|TailOut]) :-
    is_prs(HeadIn),
    !,
    call(P,HeadIn,HeadOut),
    change_subprss(P,TailIn,TailOut).

change_subprss(P,[Var|TailIn],[Var|TailOut]) :-
	var(Var),
	!,
    change_subprss(P,TailIn,TailOut).

change_subprss(P,[Functor :: PRS1In => PRS2In|TailIn],[Functor :: PRS1Out => PRS2Out|TailOut]) :-
    !,
	call(P,PRS1In,PRS1Out),
    call(P,PRS2In,PRS2Out),
    change_subprss(P,TailIn,TailOut).

change_subprss(P,[HeadIn|TailIn],[HeadOut|TailOut]) :-
    HeadIn =.. [Functor|ArgsIn],
    maplist(change_subprss_in_cond_arg(P),ArgsIn,ArgsOut),
    HeadOut =.. [Functor|ArgsOut],
    change_subprss(P,TailIn,TailOut).

change_subprss(_,[],[]).

change_subprss_in_cond_arg(P,ArgIn,ArgOut) :-
    is_prs(ArgIn),
    !,
    call(P,ArgIn,ArgOut).

change_subprss_in_cond_arg(P,ArgIn,ArgOut) :-
    is_list(ArgIn),
    !,
	change_subprss(P,ArgIn,ArgOut).

change_subprss_in_cond_arg(_,Arg,Arg).
