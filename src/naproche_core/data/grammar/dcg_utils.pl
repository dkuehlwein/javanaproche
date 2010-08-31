:- module(dcg_utils,[
	check_var_acc/3,
	create_math_ids/5,
	not_accessible/3,
	expand_id/3,
	change_id/3,
	merge/2,
	prs_merge/4,
	prs_conjunction/4,
	prs_disjunction/4,
	disjunctive_cond/4,
	dissolve_subprss/4,
	no_new_var/2,
	extract_drefs/2,
	no_assumption_closing/3,
	expecting_proof_and_qed/4,
	exclude_without_instantiation/3,
	rest_list/3,
	add_case_distinction/3,
	disjunct_cases/2,
	disjunct_number/3,
	new_prs/5,
	add_condition/6,
	add_dref/6,
	add_drefs/3,
	add_rref/5,
	mark_plural_dref/3,
	remember_dref_cond_links/4,
	remember_dref_cond_links/3,
	remember_dref_math_id_links/3,
	make_conditional_semantics/7,
	dissolve_plurals/2,
	add_accessible/4,
	update_var_types/4
]).

:- use_module(naproche(error_logger)).
:- use_module(naproche(math_lexicon)).
:- use_module(naproche(utils)).

:- op(601,xfx, user:(=>)).
:- op(601,xfx, user:(==>)).
:- op(601,xfx, user:(:=)).
:- op(601,xfx, user:(<=>)).
:- op(601,xfx, user:(v)).
:- op(601,xfx, user:(<=)).
:- op(699,xfx, user:(::)).

/**	<module> DCG utils
*	
*	This module defines some additional predicates used in dcg.pl
*
*	@author Marcos Cramer
*	@author Daniel Kuehlwein
*	@author John Schmid
*/


%%	check_var_acc(+VariableList,+PRSIn,-PRSOut)
%
%	Checks for each entry in VariableList whether it is accessible in PRSIn.
%	If not, then it adds a math_id condition, a new dref and a new mref. 
%	If the variable is linked to a certain noun by a var_type_fix, a predicate condition is added too.

check_var_acc([],PRSIn,PRSIn).

check_var_acc([Head|Tail],PRSIn,PRSOut) :-
	PRSIn = accafter~Acc,
	member_subsumes(math_id(_,Head),Acc),
	check_var_acc(Tail,PRSIn,PRSOut).

% Here we add a math_id and a predicate condition:
check_var_acc([Head|Tail],PRSIn,PRSOut):-
	getval(var_types,VarTypes),
	% We make a copy of VarTypes so that we don't instantiate anything in VarTypes.
	copy_term(VarTypes,VarTypesCopy),
	member([Head,Noun],VarTypesCopy),
	add_condition(add_dref~yes,PRSIn,math_id(Dref,Head),TmpPRS,_,_),
	add_condition(add_dref~yes,TmpPRS,predicate(Dref,Noun),TmpPRS2,_,_),
	remember_dref_cond_links([Dref],TmpPRS,TmpPRS2,TmpPRS3),
	check_var_acc(Tail,TmpPRS3,PRSOut),
	Head = dref~Dref.

% Here we add just a math_id:
check_var_acc([Head|Tail],PRSIn,PRSOut):-
	add_condition(add_dref~yes,PRSIn,math_id(Dref,Head),TmpPRS,_,_),
	check_var_acc(Tail,TmpPRS,PRSOut),
	Head = dref~Dref.


%%	create_math_ids(+VariableList,+Features,+PRSIn,-PRSOut,-DrefList)
%
%	For every variable in VariableList, it adds a math_id condition to the PRS and the dref of
%	the math_id condition to DrefList.
%	If the variable is linked to a certain noun by a var_type_fix, a predicate condition is created too.
%	For every variable in VariableList that is in Acc, give a warning message.
%	@param Features indicates whether the add_dref feature has value yes or no, i.e. whether a dref should
%	be added for the new math_id condition.

create_math_ids([],_,PRSIn,PRSIn,[]).

create_math_ids([Head|Tail],Features,PRSIn,PRSOut,DrefList) :-
	PRSIn = id~Id..accafter~Acc,
	( \+ member_subsumes(math_id(_,Head),Acc), ! ;
	  Head = name~Variable,
	  add_warning_message_once(prsWarning,create_math_ids,[Id,Variable],'The variable introduced in this sentence is already in use') ),
	create_math_ids_aux([Head|Tail],Features,PRSIn,PRSOut,DrefList).
	
% Here we add a math_id and a predicate condition:
create_math_ids_aux([Head|Tail],Features,PRSIn,PRSOut,[Dref|DrefTail]):-
	getval(var_types,VarTypes),
	% We make a copy of VarTypes so that we don't instantiate anything in VarTypes.
	copy_term(VarTypes,VarTypesCopy),
	member([Head,Noun],VarTypesCopy),
	add_condition(Features,PRSIn,math_id(Dref,Head),TmpPRS,_,_),
	add_condition(Features,TmpPRS,predicate(Dref,Noun),TmpPRS2,_,_),
	remember_dref_cond_links([Dref],TmpPRS,TmpPRS2,TmpPRS3),
	create_math_ids(Tail,Features,TmpPRS3,PRSOut,DrefTail),
	Head = dref~Dref.
	
% Here we add just a math_id:
create_math_ids_aux([Head|Tail],Features,PRSIn,PRSOut,[Dref|DrefTail]):-
	add_condition(Features,PRSIn,math_id(Dref,Head),TmpPRS,_,_),
	create_math_ids(Tail,Features,TmpPRS,PRSOut,DrefTail),
	Head = dref~Dref.


%%	not_accessible(+VarTree,+Acc,+Id)
%
%	Throws warning if VarTree is in Acc. Always succeeds.

not_accessible(VarTree,Acc,Id) :-
	member_subsumes(math_id(_,VarTree),Acc),
	!,
	VarTree = name~Var,
	add_warning_message_once(prsWarning,not_accessible,[Id,Var],'The variable introduced in this sentence is already in use').

not_accessible(_,_,_).


%%	no_new_var(+Var,+PRS)
%
%	Checks that Var contains no variables that aren't accessible in PRS. If it does, throw error message
%	and fail.

no_new_var([],_).

no_new_var([Head|Tail],PRS):-
	PRS = accafter~Acc,
	member_subsumes(math_id(_,Head),Acc),
	!,
	no_new_var(Tail,PRS).

no_new_var([Head|_],PRS):-
	PRS = id~Id,
	Head = name~Variable,
	add_error_message_once(prsError,no_new_var,[Id,Variable],'Used newly introduced variable in the term that defines a function'),
	fail,
	!.


%%	expand_id(+Prefix,+PRSIn,-PRSOut)
%
%	Expands the ID of a PRS with with a prefix. Can also be used as expand_id(_,-PRSIn,+PRSOut) for contracting the ID.

expand_id(Prefix,PRSIn,PRSOut) :-
	PRSIn = id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links,
	PRSOut = id~NewId..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links,
	NewId =.. [Prefix,Id].

%%	change_id(+NewId,+PRSIn,-PRSOut)
%
%	The ID of PRSIn is changed to NewId. If NewId is of the form "new_prefix(Prefix)", then the ID of PRSOut is the ID
%	of PRSIn with its outermost prefix replaced by Prefix.

change_id(new_prefix(P),PRSIn,PRSOut) :-
	expand_id(_,TmpPRS,PRSIn),
	expand_id(P,TmpPRS,PRSOut).

change_id(
	NewId,
	drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links,
	drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links..id~NewId
).

%%	merge(+PRSIn,-PRSOut)
%
%	All conditions of the form merge(PRS1,PRS2) in PRSIn and its sub-PRSs are dissolved. 
%	When a condition of this form gets dissolved, the content of PRS1 and the content of PRS2 both 
%	get added to the content of the parent PRS.
%	
%	@param PRSIn is a sentence PRS, so that it contains no definition conditions.

merge(PRSIn,PRSOut) :-
	PRSIn = id~Id..drefs~Drefs..mrefs~Mrefs..conds~[merge(PRS1,PRS2)|Conds]..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links,
	TmpPRS1 = id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links,
	prs_merge(PRS1,PRS2,TmpPRS1,TmpPRS2),
	merge(TmpPRS2,PRSOut).
	
merge(PRSIn,PRSOut) :-
	PRSIn = id~Id..drefs~Drefs..mrefs~Mrefs..conds~[HeadIn|TailIn]..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links,
	merge_cond(HeadIn,HeadOut),
	TmpPRS1 = drefs~Drefs..mrefs~Mrefs..conds~TailIn,
	merge(TmpPRS1,TmpPRS2),
	TmpPRS2 = drefs~DrefsOut..mrefs~MrefsOut..conds~TailOut,
	PRSOut = id~Id..drefs~DrefsOut..mrefs~MrefsOut..conds~[HeadOut|TailOut]..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links.

merge(PRS,PRS) :-
	PRS = conds~[].

merge_cond(CondIn,CondOut) :-
	is_prs(CondIn),
	merge(CondIn,CondOut).

merge_cond(the(Dref,PRSIn),the(Dref,PRSOut)) :-
	merge(PRSIn,PRSOut).

merge_cond(plural(Dref,PRSIn),plural(Dref,PRSOut)) :-
	merge(PRSIn,PRSOut).

merge_cond(Cond,Cond) :-
	Cond =.. [Functor|_],
	( Functor = holds; Functor = math_id; Functor = contradiction; Functor = predicate; Functor = in; Functor = plural_dref ).

merge_cond(CondIn,CondOut) :-
	CondIn =.. [Functor|ArgumentsIn],
	merge_list(ArgumentsIn,ArgumentsOut),
	CondOut =.. [Functor|ArgumentsOut].

merge_list([HeadIn|TailIn],[HeadOut|TailOut]) :-
	merge(HeadIn,HeadOut),
	merge_list(TailIn,TailOut).

merge_list([],[]).


%%	prs_merge(+PRS1,+PRS2,+PRSIn,-PRSOut)
%
%	Merges PRS1 and PRS2 into parent PRS: Conditions, drefs and mrefs of PRSOut are 
%	the union of those features in PRSIn, PRS1 and PRS2. 

prs_merge(PRS1,PRS2,PRSIn,PRSOut) :-
	PRS1 = conds~Conds1..drefs~Drefs1..mrefs~Mrefs1..dref_cond_links~Links1,
	PRS2 = conds~Conds2..drefs~Drefs2..mrefs~Mrefs2..dref_cond_links~Links2,
	PRSIn = id~Id..conds~CondsIn..drefs~DrefsIn..mrefs~MrefsIn..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~LinksIn,
	% Conds2 comes before Conds1 because condition lists are in reversed order
	% (because in PRS construction the last condition is always added at the 
	% beginning of the condition list)
	append([Conds2,Conds1,CondsIn],CondsOut),
	append([Drefs2,Drefs1,DrefsIn],DrefsOut),
	append([Mrefs2,Mrefs1,MrefsIn],MrefsOut),
	length(CondsIn,NumberOfCondsIn),
	length(Conds1,NumberOfConds1),
	NumberOfCondsBeforePRS2 is NumberOfCondsIn + NumberOfConds1,
	augment_dref_cond_links(Links1,NumberOfCondsIn,NewLinks1),
	augment_dref_cond_links(Links2,NumberOfCondsBeforePRS2,NewLinks2),
	append([NewLinks2,NewLinks1,LinksIn],LinksOut),
	PRSOut = id~Id..conds~CondsOut..drefs~DrefsOut..mrefs~MrefsOut..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~LinksOut.

augment_dref_cond_links([[DrefList,IndexIn]|TailIn],Number,[[DrefList,IndexOut]|TailOut]) :-
	IndexOut is IndexIn + Number,
	augment_dref_cond_links(TailIn,Number,TailOut).

augment_dref_cond_links([],_,[]).

%%	prs_conjunction(+PRSIn,+PRS1,+PRS2,-PRSOut)
%
%	If PRS2 is empty, the content of PRS1 is added to PRSIn to get PRSOut.
%	Otherwise, PRS1 and PRS2 are added as two consecutive conditions to PRSIn.

prs_conjunction(PRSIn,PRS1,PRS2,PRSOut) :-
	PRS2 = drefs~[]..mrefs~[]..conds~[]..rrefs~[],
	!,
	PRSIn = id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..dref_cond_links~Links,
	PRS1 = drefs~Drefs1..mrefs~Mrefs1..conds~Conds1..rrefs~Rrefs1..accafter~AccAfter1..dref_cond_links~Links1,
	append(Drefs1,Drefs,DrefsOut),
	append(Mrefs1,Mrefs,MrefsOut),
	append(Conds1,Conds,CondsOut),
	append(Rrefs1,Rrefs,RrefsOut),
	% The following local cut is needed, because some PRSs still do not contain dref_cond_links
	call((append(Links1,Links,LinksOut),!)),
	PRSOut = id~Id..drefs~DrefsOut..mrefs~MrefsOut..conds~CondsOut..rrefs~RrefsOut..accbefore~AccBefore..accafter~AccAfter1..dref_cond_links~LinksOut.

prs_conjunction(PRSIn,PRS1,PRS2,PRSOut) :-
	PRS2 = accafter~AccAfter,
	PRSIn = id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..dref_cond_links~Links,
	PRSOut = id~Id..drefs~Drefs..mrefs~Mrefs..conds~[PRS2, PRS1|Conds]..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links.


%%	prs_disjunction(+PRSIn,+PRS1,+PRS2,-PRSOut)
%
%	If PRS2 is empty, the content of PRS1 is added to PRSIn to get PRSOut.
%	Otherwise, PRS1 v PRS2 is added as a condition to PRSIn.

prs_disjunction(PRSIn,PRS1,PRS2,PRSOut) :-
	PRS2 = drefs~[]..mrefs~[]..conds~[]..rrefs~[],
	!,
	PRSIn = id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..dref_cond_links~Links,
	PRS1 = drefs~Drefs1..mrefs~Mrefs1..conds~Conds1..rrefs~Rrefs1..accafter~AccAfter1..dref_cond_links~Links1,
	append(Drefs1,Drefs,DrefsOut),
	append(Mrefs1,Mrefs,MrefsOut),
	append(Conds1,Conds,CondsOut),
	append(Rrefs1,Rrefs,RrefsOut),
	append(Links1,Links,LinksOut),
	PRSOut = id~Id..drefs~DrefsOut..mrefs~MrefsOut..conds~CondsOut..rrefs~RrefsOut..accbefore~AccBefore..accafter~AccAfter1..dref_cond_links~LinksOut.

prs_disjunction(PRSIn,PRS1,PRS2,PRSOut) :-
	PRSIn = id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links,
	PRSOut = id~Id..drefs~Drefs..mrefs~Mrefs..conds~[PRS1 v PRS2|Conds]..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links.

%%	disjunctive_cond(+Id,+Acc,+DisjunctPRSs,-DisjunctionCond)
%
%	Produces a single disjunctive condition from a number of PRSs serving as disjuncts. Input argument Id and Acc are needed in order to produce IDs and accessibles of sub-PRSs.

disjunctive_cond(_,_,[Disjunct1,Disjunct2],Disjunct1 v Disjunct2).

disjunctive_cond(Id,Acc,[DisjunctHead|DisjunctTail],DisjunctHead v TailDisjunctionPRS) :-
	disjunctive_cond(or(Id),Acc,DisjunctTail,TailDisjunction),
	TailDisjunctionPRS = id~or(Id)..drefs~[]..mrefs~[]..conds~[TailDisjunction]..rrefs~[]..accbefore~Acc..accafter~Acc.

%%	dissolve_subprss(+Conds,+Drefs,+Mrefs,?PRS)
%
%	The conditions in Conds that are PRSs get "dissolved", i.e. their Conds, Drefs
%	and Mrefs are added directly to PRS. The other conditions in Conds get added to
%	the conditions of PRS. Drefs and Mrefs get added to PRS.
%
%	@param PRS enters with id, accbefore and possibly rrefs. It exits with all features
%	apart from possibly rrefs.

dissolve_subprss(Conds,Drefs,Mrefs,PRS):-
	PRS = accbefore~AccBefore,
	dissolve_subprss_1(Conds,[],Drefs,[],Mrefs,[],PRS,AccBefore).

dissolve_subprss_1([],DissolvedConds,Drefs,SubDrefs,Mrefs,SubMrefs,PRS,AccAfter):-
	append(SubDrefs,Drefs,AllDrefs),
	append(SubMrefs,Mrefs,AllMrefs),
	PRS = conds~DissolvedConds..drefs~AllDrefs..mrefs~AllMrefs..accafter~AccAfter.

dissolve_subprss_1([Head|Tail],DissolvedCondsIn,Drefs,SubDrefsIn,Mrefs,SubMrefsIn,PRS,_):-
	Head = conds~HeadConds..drefs~HeadDrefs..mrefs~HeadMrefs..accafter~AccAfter,
	append(DissolvedCondsIn,HeadConds,DissolvedCondsOut),
	append(SubDrefsIn,HeadDrefs,SubDrefsOut),
	append(SubMrefsIn,HeadMrefs,SubMrefsOut),
	dissolve_subprss_1(Tail,DissolvedCondsOut,Drefs,SubDrefsOut,Mrefs,SubMrefsOut,PRS,AccAfter).

dissolve_subprss_1([Head|Tail],DissolvedCondsIn,Drefs,SubDrefs,Mrefs,SubMrefs,PRS,Acc):-
	Head = math_id(_,_),
	append(DissolvedCondsIn,[Head],DissolvedCondsOut),
	dissolve_subprss_1(Tail,DissolvedCondsOut,Drefs,SubDrefs,Mrefs,SubMrefs,PRS,[Head|Acc]).

dissolve_subprss_1([Head|Tail],DissolvedCondsIn,Drefs,SubDrefs,Mrefs,SubMrefs,PRS,Acc):-
	append(DissolvedCondsIn,[Head],DissolvedCondsOut),
	dissolve_subprss_1(Tail,DissolvedCondsOut,Drefs,SubDrefs,Mrefs,SubMrefs,PRS,Acc).


%%	extract_drefs(+MathIds,-Drefs)
%
%	Creates a list of the drefs that are in MathIds.

extract_drefs([],[]).

extract_drefs([math_id(Dref,_)|TailMathIds],[Dref|TailDrefs]):-
	extract_drefs(TailMathIds,TailDrefs).

/*
%%	create_PRS_copies(+Dref,+PRS,+DrefList,-PRSList)
%
%	For each element D in DrefList, a copy C of PRS is created, the copy of Dref in C is unified with D, and C is added to PRSList.

create_PRS_copies(Dref,PRS,[CurrentDref|DrefList],[CopiedPRS|PRSList]) :-
	copy_term(pair(Dref,PRS),pair(CurrentDref,CopiedPRS)).

create_PRS_copies(_,_,[],[]).
*/


%%	no_assumption_closing(+Type)
%
%	This predicate parses a list of sentences iff it contains no "thus" that 
%	doesn't close an assumption introduced in that list of sentences.
%
%	@param Type is the type of text from which this predicate was called.

no_assumption_closing(Type) -->
	assumption_depth(Type,0),
	!.

%%	assumption_depth(+Type,+N)
%
%	This is an auxiliary predicate for defining no_assumption_closing. N indicates
%	how many assumptions have been opened since starting the parsing.
%
%	assumption_depth(N) parses a list of sentences iff there are at most N "thus"s that don't
%	close an assumption introduced in that list of sentences.

assumption_depth(Type,N) -->
	[sentence(_,Content)],
	{ 
	Content = [lemma|_],
	!
	},
	expecting_qeds(Type,1,N).

assumption_depth(Type,N) -->
	[sentence(_,Content)],
	{ 
	Content = [theorem|_],
	!
	},
	expecting_qeds(Type,1,N).

assumption_depth(Type,_) -->
	[sentence(_,Content)],
	{
	text_ending(Type,Content,[])
	},
	anything.

assumption_depth(Type,N) -->
	[sentence(_,Content)],
	{
	dcg:trigger(type~ass,Content,_),
	!,
	M is N + 1
	},
	assumption_depth(Type,M).

assumption_depth(Type,N) -->
	[sentence(_,Content)],
	{
	dcg:trigger(type~variable_declaration,Content,_),
	!,
	M is N + 1
	},
	assumption_depth(Type,M).

assumption_depth(Type,N) -->
	[sentence(_,Content)],
	{ 
	\+ dcg:trigger(type~ass_closing,Content,_)
	},
	assumption_depth(Type,N).

assumption_depth(Type,N) -->
	[sentence(_,Content)],
	{ 
	N \= 0,
	dcg:trigger(type~ass_closing,Content,_),
	M is N - 1
	},
	assumption_depth(Type,M).
	
assumption_depth(_,_) -->
	[].


%%      text_ending(+Type)
%
%       This predicate parses any sentence that ends a text section.

text_ending(_) -->
        [qed].

text_ending(_) -->
	[proof].

text_ending(in_case) -->
	[case,_].

text_ending(in_case) -->
	dcg:trigger(type~statement),
	dcg:trigger(type~case_closing),
	anything.


%%	execting_qeds(+Type,+M,+N)
%
%	This is an auxiliary predicate for defining no_assumption_closing. It is called
%	by assumption_depth(N). If a "lemma" or "theorem" is encountered, 1 is added to
%	the first argument. If a "qed" is encountered, 1 is subtracted from the first 
%	argument, unless the first argument was 1, in which case parsing continues with
%	assumption_depth(N).

expecting_qeds(Type,M,N) -->
	[sentence(_,Content)],
	{ 
	Content = [lemma|_], 
	!,
	P is M + 1
	},
	expecting_qeds(Type,P,N).

expecting_qeds(Type,M,N) -->
	[sentence(_,Content)],
	{ 
	Content = [theorem|_], 
	!,
	P is M + 1
	},
	expecting_qeds(Type,P,N).

expecting_qeds(Type,M,N) -->
	[sentence(_,Content)],
	{ 
	Content \= [qed],
	!
	},
	expecting_qeds(Type,M,N).

expecting_qeds(Type,1,N) -->
	{!},
	[sentence(_,_)],
	assumption_depth(Type,N).

expecting_qeds(Type,M,N) -->
	[sentence(_,_)],
	{
	P is M - 1
	},
	expecting_qeds(Type,P,N).

%%      exclude_without_instantiation(+Element:term, +List1:list, -List2:list)
%
%       List2 is List1 with elements identical (without instantiation) to Element.

exclude_without_instantiation(_Element, [], []).

exclude_without_instantiation(Element, [OtherElement | List1], [OtherElement | List2]) :-
 \+ Element == OtherElement,
 !,
 exclude_without_instantiation(Element, List1, List2).

exclude_without_instantiation(Element, [_ | List1], List2) :-
 exclude_without_instantiation(Element, List1, List2).


%% expecting_proof_and_qed(+TextType,+Id:id)
% 
% checks whether the sentences following "theorem" or "lemma" are well-structured.
% there are three possible errors: a "proof" is missing, a "qed" is missing, or a
% lemma contains another lemma. an appropriate error message is created.
%
% @param TextType is either the atom 'theorem' or 'lemma'.

expecting_proof_and_qed(TextType,Id) -->
    [sentence(_,[proof])],
    [sentence(_,[qed])],
    {
    concat_atom(['This ',TextType,' has an empty proof.'],Message),
    add_error_message_once(grammarError,expecting_proof_and_qed,Id,Message),
    !,
    fail
    }.

expecting_proof_and_qed(TextType,Id) -->
 [sentence(_,[proof])],
 !,
 expecting_qed(TextType,Id).

expecting_proof_and_qed(TextType,Id) -->
 [sentence(_,Content)],
 {
 \+ theorem_trigger(Content,[]),
 \+ lemma_trigger(Content,[]),
 \+ Content = [proof],
 \+ Content = [qed],
 \+ Content = [end_proof]
 },
 !,
 expecting_proof_and_qed(TextType,Id).

expecting_proof_and_qed(TextType,Id,_,_):-
 concat_atom(['This ',TextType,' is missing a "proof".'],Message),
 add_error_message_once(grammarError,expecting_proof_and_qed,Id,Message),
 !,
 fail.

%% expecting_qed(+TextType,+Id:id)
%
%  checks whether the sentences after the initial "proof" contain "qed". sentences
%  following this "qed" are ignored (with help of the predicate anything). if one is
%  proving a theorem, a lemma may be inserted; after this lemma_in_theorem finishes,
%  though, the proof of the theorem continues immediately.
%
%  @param TextType is one of the three atoms 'theorem', 'lemma', 'lemma_in_theorem').

expecting_qed(lemma_in_theorem,_) -->
 ([sentence(_,[qed])];[sentence(_,[end_proof])]).


expecting_qed(lemma,_) -->
 ([sentence(_,[qed])];[sentence(_,[end_proof])]),
 anything.

expecting_qed(theorem,_) -->
 ([sentence(_,[qed])];[sentence(_,[end_proof])]),
 anything.

expecting_qed(theorem,Id1) -->
 [sentence(Id2,Content)],
 {
 lemma_trigger(Content,[])
 },
 !,
 expecting_proof_and_qed(lemma_in_theorem,Id2),
 !,
 expecting_qed(theorem,Id1).

expecting_qed(TextType,Id) -->
 [sentence(_,Content)],
 {
 \+ theorem_trigger(Content,[]),
 \+ lemma_trigger(Content,[]),
 \+ Content = [proof],
 \+ Content = [qed],
 \+ Content = [end_proof]
 },
 expecting_qed(TextType,Id).

expecting_qed(lemma,Id,[sentence(_,Content)|_],_):-
 lemma_trigger(Content,[]),
 add_error_message_once(grammarError,expecting_proof_and_qed,Id,'Lemmas may not contain lemmas.'),
 !,
 fail.

expecting_qed(TextType,Id,_,_):-
 concat_atom(['This ',TextType,' is missing a "qed".'],Message),
 add_error_message_once(grammarError,expecting_proof_and_qed,Id,Message),
 !,
 fail.

lemma_trigger --> [lemma].
lemma_trigger --> [lemma],[_].

theorem_trigger --> [theorem].
theorem_trigger --> [theorem],[_].

anything -->
 [_],
 anything.

anything -->
 [].

rest_list(List,List,List).

%%	add_case_distinction(+CaseList,+In,-Out)
%
%	Takes an ingoing PRS and a list of cases, disjuncts the cases and adds the
%	resulting PRS to the original PRS's conditions.
%
%	Effectively, at least one case on the list must hold.

add_case_distinction(CaseList,In,Out) :-
	In = id~Id..accbefore~AccBefore..accafter~AccAfter..conds~Conds..drefs~Drefs..mrefs~Mrefs..rrefs~Rrefs,
	CaseDistinction = id~case_distinction(Id)..accbefore~AccAfter,
	disjunct_cases(CaseList,CaseDistinction),
	Out = id~Id..conds~[CaseDistinction|Conds]..accbefore~AccBefore..accafter~AccAfter..drefs~Drefs..mrefs~Mrefs..rrefs~Rrefs.

%%	disjunct_cases(+PRSList,?BoxAbove)
%
%	takes the PRS list and recursively connects PRSes through "or".
%
%	BoxAbove enters with id and accbefore and exits as a complete PRS.

disjunct_cases([PRS1,PRS2],BoxAbove) :-
	BoxAbove = conds~[PRS1 v PRS2]..drefs~[]..mrefs~[]..rrefs~[]..dref_cond_links~[]..accbefore~AccBefore..accafter~AccBefore.

disjunct_cases([PRS|PRSList],BoxAbove) :-
	BoxAbove = id~Id..accbefore~AccBefore,
	BoxToTheRight = id~or(Id)..accbefore~AccBefore,
	disjunct_cases(PRSList,BoxToTheRight),
	!,
	BoxAbove = conds~[PRS v BoxToTheRight]..drefs~[]..mrefs~[]..rrefs~[]..dref_cond_links~[]..accafter~AccBefore.

%%	disjunct_number(+Number1,+Number2,-DisjunctionNumber)
%
%	In an NP disjunction, the grammatical number of the single disjuncts affects the grammatical number of the disjunction.
%	If the disjuncts have the same number, the disjunction takes over this number.
%	If the disjuncts have different numbers, the disjunction number is "mixed".
%	Since verb agreement is not possible for NP disjunctions with number "mixed", such NP disjunctions can only be used as
%	objects or as subjects of infinitive constructs.

disjunct_number(Number,Number,Number) :-
	!.

disjunct_number(_,_,mixed).

%%	new_prs(+Id,+SuperPRS,-NewPRS,+DCGList,-DCGList)
%
%	NewPRS is an empty PRS with accessibles from the accafter of SuperPRS. The DCGList arguments are
%	added so that this predicate can be called more easily in a DCG environment.
%	If Id is of the form "prefix(P)", then the ID of NewPRS is the ID of the SuperPRS prefixed with P. 
%	If Id is of the form "new_prefix(P)", then the ID of NewPRS is the ID of SuperPRS with its prefix replaced by P.
%	If Id is "copy", then the ID of NewPRS is the ID of SUperPRS.
%	Otherwise the ID of NewPRS is ID.

new_prs(prefix(P),id~Id..accafter~Acc,id~PrefixedId..drefs~[]..mrefs~[]..conds~[]..rrefs~[]..accbefore~Acc..accafter~Acc..dref_cond_links~[],DCGList,DCGList) :-
	!,
	PrefixedId =.. [P,Id].

new_prs(new_prefix(P),id~Id..accafter~Acc,id~PrefixedId..drefs~[]..mrefs~[]..conds~[]..rrefs~[]..accbefore~Acc..accafter~Acc..dref_cond_links~[],DCGList,DCGList) :-
	!,
	Id =.. [_,IdCore],
	PrefixedId =.. [P,IdCore].

new_prs(copy,id~Id..accafter~Acc,id~Id..drefs~[]..mrefs~[]..conds~[]..rrefs~[]..accbefore~Acc..accafter~Acc..dref_cond_links~[],DCGList,DCGList) :-
	!.

new_prs(Id,accafter~Acc,id~Id..drefs~[]..mrefs~[]..conds~[]..rrefs~[]..accbefore~Acc..accafter~Acc..dref_cond_links~[],DCGList,DCGList).

%%	add_condition(+Features,+PRSIn,+Cond,-PRSOut,+DCGList,-DCGList) is det
%
%	Cond is added to the condition list of the PRS. If Cond is a math_id or plural_dref condition, it is also added to the
%	accessibles, and the dref (and mref) of the condition are added to the dref (and mref) lists (if the add_dref feature in 
%	Features has value "yes"). If it is a PRS, a condition of the form "merge(PRS1,PRS2)" or a condition of the form "theorem(_,PRS2,PRS1)", then its (or respectively
%	PRS2's) accafter are given to PRSOut. If it is a condition of the form "the(Dref,_)", then Dref is added to
%	the accessibles.  
%	The DCGList arguments are added so that this predicate can be called more easily in a DCG environment.

add_condition(Features,id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links,Cond,id~Id..drefs~DrefsOut..mrefs~MrefsOut..conds~[Cond|Conds]..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfterOut..dref_cond_links~Links,DCGList,DCGList) :-
	(
	( 
		Cond = math_id(Dref,Mref), 
		AccAfterOut = [Cond|AccAfter], 
		( Features = add_dref~yes -> DrefsOut = [Dref|Drefs] ; DrefsOut = Drefs), 
		MrefsOut = [Mref|Mrefs] 
	)
	;
	( 
		Cond = plural_dref(Dref,_), 
		AccAfterOut = [Cond|AccAfter], 
		( Features = add_dref~yes -> DrefsOut = [Dref|Drefs] ; DrefsOut = Drefs), 
		MrefsOut = Mrefs
	)
	;
	(
		DrefsOut = Drefs,
		MrefsOut = Mrefs,
		(
			( is_prs(Cond), Cond = accafter~AccAfterOut )
			;
			( Cond = merge(_,PRS2), \+ var(PRS2), PRS2 = accafter~AccAfterOut )
			;
			( Cond = theorem(_,PRS2,_), PRS2 = accafter~AccAfterOut )
			;
			( Cond = the(Dref,_), AccAfterOut = [Dref|AccAfter] )
			;
			AccAfterOut = AccAfter
		)
	)
	),
	!.

%%	add_dref(+Features,?PRSIn,+Dref,?PRSOut,+DCGList,-DCGList) is det
%
%	If the feature "dref" in Features has value "yes", Dref is added to the dref list of the PRS,
%	and to its accafter.
%	This predicate can also be used for removing drefs (also when the dref is not in accafter).

add_dref(add_dref~yes,id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links,Dref,id~Id..drefs~[Dref|Drefs]..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~[Dref|AccAfter]..dref_cond_links~Links,DCGList,DCGList) :-
	var(Dref), % This condition is needed so that add_dref can be used for removing drefs.
	!.

% The following clause is only entered when removing a dref which is not in accafter:
add_dref(add_dref~yes,id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links,Dref,id~Id..drefs~[Dref|Drefs]..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links,DCGList,DCGList) :-
	!.

add_dref(_,PRS,_,PRS,DCGList,DCGList).

%%	add_drefs(+PRSIn,+DrefList,-PRSOut)
%
%	All drefs in DrefList are addded to the PRS drefs and accafter.

add_drefs(PRSIn,[Head|Tail],PRSOut) :-
	add_dref(add_dref~yes,PRSIn,Head,TmpPRS,_,_),
	add_drefs(TmpPRS,Tail,PRSOut).

add_drefs(PRS,[],PRS).

%%	add_rref(+PRSIn,+Rref,-PRSOut,+DCGList,-DCGList) is det
%
%	Rref is added to the rref list of the PRS.

add_rref(id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links,Rref,id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~[Rref|Rrefs]..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links,DCGList,DCGList).


%%	mark_dref(+Dref,+NumberFeature,-PossiblyMarkedDref)
%
%	If the feature "number" has value "plural", Dref gets marked with a "plural" prefix. 

mark_plural_dref(Dref,number~plural,plural(Dref)) :-
	!.

mark_plural_dref(Dref,_,Dref).


%%	remember_dref_cond_links(+AdditionalDrefs,+PRSIn,+TmpPRS,-PRSOut)
%
%	This predicate is used to remember which conditions where introduced together with which drefs. 
%	For every condition that was added between PRSIn and TmpPRS, a pair of the form [DrefList,CondPosition]
%	is added to the dref_cond_links list of TmpPRS, where CondPosition is the position of the corresponding
%	condition in TmpPRS counting from the end, and DrefList is the list of drefs that were added between
%	PRSIn and TmpPRS (augmented by the dref list AdditionalDrefs). The result of adding these pairs to TmpPRS is PRSOut.

remember_dref_cond_links(AdditionalDrefs,PRSIn,TmpPRS,PRSOut) :-
	PRSIn = conds~CondsIn..drefs~DrefsIn,
	TmpPRS = conds~Conds..id~Id..drefs~Drefs..mrefs~Mrefs..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links,
	length(CondsIn,CondsInLength),
	length(Conds,CondsLength),
	append(TmpDrefList,DrefsIn,Drefs),
	append(AdditionalDrefs,TmpDrefList,DrefList),
	make_dref_cond_pairs(CondsInLength,CondsLength,DrefList,Links,LinksOut),
	PRSOut = conds~Conds..id~Id..drefs~Drefs..mrefs~Mrefs..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~LinksOut,
	!.

%%	remember_dref_cond_links(+PRSIn,+TmpPRS,-PRSOut)
%
%	Like remember_dref_con_links/4, only that no additional drefs can be added.

remember_dref_cond_links(PRSIn,TmpPRS,PRSOut) :-
	remember_dref_cond_links([],PRSIn,TmpPRS,PRSOut).

%%	make_dref_cond_pairs(+Index,+UpperBound,+DrefList,+LinksIn,-LinksOut)
%
%	For every natural number N greater than Index and less than or equal to UpperBound, a pair
%	of the form [DrefList,N] is added to LinksIn.

make_dref_cond_pairs(Index,UpperBound,DrefList,LinksIn,LinksOut) :-
	NewIndex is Index + 1,
	NewIndex =< UpperBound,
	!,
	make_dref_cond_pairs(NewIndex,UpperBound,DrefList,[[DrefList,NewIndex]|LinksIn],LinksOut).

make_dref_cond_pairs(_,_,_,LinksIn,LinksIn).

%%	remember_dref_math_id_links(+PRSIn,+TmpPRS,-PRSOut)
%
%	This predicate is used instead of remember_dref_cond_links, when the difference between PRSIn and TmpPRS
%	is the semantics of a formula rather than of an N'. For every dref added between PRSIn and TmpPRS, there
%	is a new math_id condition in TmpPRS, to which this dref gets linked.

remember_dref_math_id_links(PRSIn,TmpPRS,PRSOut) :-
	PRSIn = drefs~DrefsIn,
	TmpPRS = conds~Conds..id~Id..drefs~Drefs..mrefs~Mrefs..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links,
	append(DrefList,DrefsIn,Drefs),
	% In order to be able to consider the nth element of the condition list as the nth condition, we need to revert Conds:
	reverse(Conds,RevertedConds),
	make_dref_math_id_pairs(DrefList,RevertedConds,Links,LinksOut),
	PRSOut = conds~Conds..id~Id..drefs~Drefs..mrefs~Mrefs..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~LinksOut,
	!.

%%	make_dref_math_id_pairs(+DrefList,+Conds,+LinksIn,-LinksOut)
%
%	For every dref D in DrefList, there has to be a math_id condition math_id(D,M) in Conds,
%	and a pair of the form [[D],N] is added to LinksIn, where math_id(D,M) is the Nth element
%	in Conds.

make_dref_math_id_pairs([Dref|Tail],Conds,LinksIn,[[[Dref],N]|TmpLinksOut]) :-
	make_dref_math_id_pairs(Tail,Conds,LinksIn,TmpLinksOut),
	nth1_for_math_ids(N,Conds,math_id(Dref,_)).

make_dref_math_id_pairs([],_,Links,Links).

%%	nth1_for_math_ids(?Index,+ConditionList,?MathId)
%
%	MathId is the Index-th element of List, and is of the form math_id(Dref,Mref). Dref is
%	not allowed to be instantiated, but Mref yes.

nth1_for_math_ids(1,[math_id(D1,M)|_],math_id(D2,M)) :-
    D1 == D2.

nth1_for_math_ids(N,[_|Tail],MathId) :-
    \+ var(N),
    N > 0,
    M is N-1,
    nth1_for_math_ids(M,Tail,MathId).

nth1_for_math_ids(N,[_|Tail],MathId) :-
    nth1_for_math_ids(M,Tail,MathId),
    N is M+1.


%%	make_conditional_semantics(+Type,+PRSIn,+LeftPRS,+RightPRS,-PRSOut,+DCGListIn,-DCGListOut)
%
%	This predicate produces the PRS condition representing biconditionals and reversed conditional.
%	These cases need to be handled in a special way as described on the Naproche Wiki page 
%	quantifizierung_bei_bikonditionalen.
%	@param Type can be "biconditional" or "reversed_conditional".

make_conditional_semantics(Type,PRSIn,LeftPRSIn,RightPRSIn,PRSOut,DCGList,DCGList) :-
	introduced_drefs(LeftPRSIn,LeftDrefs),
	drefs_used(LeftDrefs,RightPRSIn,DrefsUsedOnRight),
	\+ DrefsUsedOnRight = [],
	!,
	specify_pullout_drefs(LeftDrefs,DrefsUsedOnRight,PullOutDrefs),
	!,
	pull_out_drefs_and_conds(PullOutDrefs,LeftPRSIn,LeftPRSOut,PulledOutPRS),
	RightPRSIn = accbefore~OldAccBefore,
	LeftPRSOut = accbefore~NewAccBefore,
	correct_accessibles(OldAccBefore,NewAccBefore,RightPRSIn,RightPRSOut),
	new_prs(new_prefix(conditional),PulledOutPRS,ConditionalPRSIn,_,_),
	(
		( Type = biconditional, !, add_condition(_,ConditionalPRSIn,LeftPRSOut <=> RightPRSOut,ConditionalPRSOut,_,_) )
		;
		( Type = reversed_conditional, add_condition(_,ConditionalPRSIn,LeftPRSOut <= RightPRSOut,ConditionalPRSOut,_,_) )
	),
	add_condition(_,PRSIn,PulledOutPRS => ConditionalPRSOut,PRSOut,_,_).

make_conditional_semantics(Type,PRSIn,LeftPRSIn,RightPRSIn,PRSOut,DCGList,DCGList) :-
	RightPRSIn = accbefore~OldAccBefore,
	LeftPRSIn = accbefore~NewAccBefore,
	correct_accessibles(OldAccBefore,NewAccBefore,RightPRSIn,RightPRSOut),
	(
		( Type = biconditional, !, add_condition(_,PRSIn,LeftPRSIn <=> RightPRSOut,PRSOut,_,_) )
		;
		( Type = reversed_conditional, add_condition(_,PRSIn,LeftPRSIn <= RightPRSOut,PRSOut,_,_) )
	).
		

introduced_drefs(PRS,Drefs) :-
	PRS = accbefore~AccBefore..accafter~AccAfter,
	append(AccDifference,AccBefore,AccAfter),
	get_drefs(AccDifference,Drefs).

get_drefs([MathId|Tail],[Dref|DrefTail]) :-
	subsumes(math_id(Dref,_),MathId),
	!,
	get_drefs(Tail,DrefTail).

get_drefs([PluralDrefCond|Tail],[Dref|DrefTail]) :-
	subsumes(plural_dref(Dref,_),PluralDrefCond),
	!,
	get_drefs(Tail,DrefTail).

get_drefs([Dref|Tail],[Dref|DrefTail]) :-
	get_drefs(Tail,DrefTail).

get_drefs([],[]).


drefs_used(Drefs,PRS,DrefsUsedInPRS) :-
	PRS = conds~Conds,
	drefs_used_in_conds(Drefs,Conds,DrefsUsedInPRS).

drefs_used_in_conds(Drefs,[Head|Tail],DrefsUsedInConds) :-
	drefs_used_in_cond(Drefs,Head,DrefsUsedInHead),
	drefs_used_in_conds(Drefs,Tail,DrefsUsedInTail),
	union_without_instantiation(DrefsUsedInHead,DrefsUsedInTail,DrefsUsedInConds).

drefs_used_in_conds(_,[],[]).

drefs_used_in_cond(Drefs,Cond,DrefsUsedInCond) :-
	is_prs(Cond),
	!,
	drefs_used(Drefs,Cond,DrefsUsedInCond).

drefs_used_in_cond(Drefs,Cond,DrefsUsedInCond) :-
    Cond =.. [Functor|Arguments],
    ( Functor = holds; Functor = contradiction; Functor = predicate ),
	!,
	intersection_without_instantiation(Arguments,Drefs,DrefsUsedInCond).

drefs_used_in_cond(Drefs,math_id(Dref,Mref),DrefsUsedInCond) :-
	!,
	mref_drefs(Mref,MrefDrefs),
	intersection_without_instantiation(Drefs,[Dref|MrefDrefs],DrefsUsedInCond).

drefs_used_in_cond(Drefs,Cond,DrefsUsedInCond) :-
    Cond =.. [plural_dref,PluralDref,DrefList],
	!,
	intersection_without_instantiation([PluralDref|DrefList],Drefs,DrefsUsedInCond).

drefs_used_in_cond(Drefs,Cond,DrefsUsedInCond) :-
	Cond =.. [Functor,Dref,PRS],
    ( Functor = the; Functor = plural ),
	!,
	intersection_without_instantiation([Dref],Drefs,DrefsUsed1),
	drefs_used(Drefs,PRS,DrefsUsed2),
	union_without_instantiation(DrefsUsed1,DrefsUsed2,DrefsUsedInCond).

drefs_used_in_cond(Drefs,Cond,DrefsUsedInCond) :-
	Cond =.. [_|Arguments],
	drefs_used_in_conds(Drefs,Arguments,DrefsUsedInCond).

mref_drefs(Mref,MrefDrefs) :-
	subsumes(dref~Dref,Mref),
	!,
	Mref = args~Args,
	mref_list_drefs(Args,ArgsDrefs),
	union_without_instantiation([Dref],ArgsDrefs,MrefDrefs).

mref_drefs(Mref,MrefDrefs) :-
	Mref = args~Args,
	mref_list_drefs(Args,MrefDrefs).

mref_list_drefs([Mref|Tail],MrefListDrefs) :-
	mref_drefs(Mref,MrefDrefs),
	mref_list_drefs(Tail,TailDrefs),
	union_without_instantiation(MrefDrefs,TailDrefs,MrefListDrefs).

mref_list_drefs([],[]).

%%	specify_pullout_drefs(+LeftDrefs,+DrefsUsedOnRight,-PullOutDrefs)
%
%	PullOutDrefs contains all elements of LeftDrefs starting at the first element
%	that also appears in DrefsUsedOnRight.

specify_pullout_drefs([Head|Tail],Right,[Head|Tail]) :-
	member_without_instantiation(Head,Right).

specify_pullout_drefs([_|Tail],Right,Drefs) :-
	specify_pullout_drefs(Tail,Right,Drefs).

%%	pull_out_drefs_and_conds(+Drefs,+PRSIn,-PRSOut,-PulledOutPRS)
%
%	This auxiliary predicate pulls out the drefs listed in Drefs from PRSIn, as well
%	as all drefs and conditions created together with these drefs. PRSOut is what remains of 
%	PRSIn after the pulling out. PulledOutPRS gets all the drefs and conditions that
%	were pulled out.

pull_out_drefs_and_conds(Drefs,PRSIn,PRSOut,PRSPulledOut) :-
	PRSIn = id~Id..accbefore~AccBefore..drefs~DrefsIn..dref_cond_links~Links..conds~Conds..mrefs~Mrefs..accafter~AccAfter..rrefs~Rrefs,
	Id =.. [_,IdArgument],
	get_drefs_pull_out_list(Drefs,Links,DrefsPullOutList),
	subtract_without_instantiation(DrefsIn,DrefsPullOutList,DrefsOut,DrefsPulledOut),
	get_conds_pull_out_list(Drefs,Links,TmpCondsPullOutList),
	% CondsPullOutList needs to be sorted in decreasing order, because separate_conds is only implemented for lists of decreasing order.
	sort(TmpCondsPullOutList,TmpCondsPullOutList2),
	reverse(TmpCondsPullOutList2,CondsPullOutList),
	separate_conds(CondsPullOutList,Conds,CondsOut,CondsPulledOut,MathIdsPulledOut),
	separate_mrefs(MathIdsPulledOut,Mrefs,MrefsOut,MrefsPulledOut),
	subtract_without_instantiation(AccAfter,MathIdsPulledOut,_,AccAfterPulledOut),
	PRSOut = id~Id..accbefore~AccAfterPulledOut..drefs~DrefsOut..conds~CondsOut..mrefs~MrefsOut..accafter~AccAfter..rrefs~Rrefs,
	PRSPulledOut = id~pullout(IdArgument)..accbefore~AccBefore..drefs~DrefsPulledOut..conds~CondsPulledOut..mrefs~MrefsPulledOut..accafter~AccAfterPulledOut..rrefs~[].

%%	get_drefs_pull_out_list(+Drefs,+Links,-PullOutDrefs)
%
%	PullOutDrefs consists of all drefs in Drefs as well as those that appear in a link
%	together with some dref from Drefs.
%	The implementation assumes that the different dref lists appearing in Links are either 
%	identical or disjoint.

get_drefs_pull_out_list(Drefs,[],Drefs).

get_drefs_pull_out_list(Drefs,[[DrefList,_]|LinksTail],PullOutDrefs) :-
	\+ intersection_without_instantiation(Drefs,DrefList,[]),
	union_without_instantiation(Drefs,DrefList,TmpPullOutDrefs),
	get_drefs_pull_out_list(TmpPullOutDrefs,LinksTail,PullOutDrefs).

get_drefs_pull_out_list(Drefs,[_|LinksTail],PullOutDrefs) :-
	get_drefs_pull_out_list(Drefs,LinksTail,PullOutDrefs).


get_conds_pull_out_list(_,X,[]) :-
	var(X).

get_conds_pull_out_list(Drefs,[[DrefList,CondPosition]|LinksTail],[CondPosition|CondsPullOutTail]) :-
	\+ intersection_without_instantiation(Drefs,DrefList,[]),
	!,
	get_conds_pull_out_list(Drefs,LinksTail,CondsPullOutTail).

get_conds_pull_out_list(Drefs,[_|LinksTail],CondsPullOutList) :-
	get_conds_pull_out_list(Drefs,LinksTail,CondsPullOutList).

get_conds_pull_out_list(_,[],[]).

%%	separate_conds(+CondsPullOutList,+Conds,-CondsOut,-CondsPulledOut,-MathIdsPulledOut)
%
%	CondsPullOutList is a list of natural numbers, indicating the positions of conditions
%	(counted from the back of the condition list) to be pulled out. This predicate is only
%	correctly implemented for the case that the numbers in CondsPullOutList are decreasing.

separate_conds(CondsPullOutList,Conds,CondsOut,CondsPulledOut,MathIdsPulledOut) :-
	separate_conds_aux(CondsPullOutList,Conds,CondsOut,CondsPulledOut),
	find_math_ids(CondsPulledOut,MathIdsPulledOut).

separate_conds_aux([Head|Tail],Conds,CondsOut,[Cond|CondsPulledOut]) :-
	nth1_back(Head,Conds,Cond,CondsWithoutCond),
	separate_conds_aux(Tail,CondsWithoutCond,CondsOut,CondsPulledOut).

separate_conds_aux([],Conds,Conds,[]).

find_math_ids([math_id(Dref,Mref)|TailConds],[math_id(Dref,Mref)|TailMathIds]) :-
	!,
	find_math_ids(TailConds,TailMathIds).

find_math_ids([_|TailConds],MathIds) :-
	find_math_ids(TailConds,MathIds).

find_math_ids([],[]).


separate_mrefs(MathIdsPulledOut,[Mref|MrefTail],MrefsOut,[Mref|MrefsPulledOutTail]) :-
	mref_in_math_ids(Mref,MathIdsPulledOut),
	!,
	separate_mrefs(MathIdsPulledOut,MrefTail,MrefsOut,MrefsPulledOutTail).

separate_mrefs(MathIdsPulledOut,[Mref|MrefTail],[Mref|MrefsOutTail],MrefsPulledOut) :-
	separate_mrefs(MathIdsPulledOut,MrefTail,MrefsOutTail,MrefsPulledOut).

separate_mrefs(_,[],[],[]).

mref_in_math_ids(Mref,[math_id(_,Mref)|_]).

mref_in_math_ids(Mref,[_|MathIds]) :-
	mref_in_math_ids(Mref,MathIds).

%%	correct_accessibles(+OldAcc,+NewAcc,+RightPRSIn,-RightPRSOut)
%
%	When  the right PRS of a biconditional or reversed conditional is produced, all things
%	introduced in the left PRS are accessible. After some things have been pulled out from
%	the left PRS, only these pulled out things are accessible in the right PRS. So we need
%	to correct the accessible lists of RightPRS and all its sub-PRSs by replacing OldAcc by
%	NewAcc in them.

correct_accessibles(
	OldAcc,
	NewAcc,
	id~Id..drefs~Drefs..mrefs~Mrefs..conds~CondsIn..rrefs~Rrefs..accbefore~AccBeforeIn..accafter~AccAfterIn..dref_cond_links~Links,
	id~Id..drefs~Drefs..mrefs~Mrefs..conds~CondsOut..rrefs~Rrefs..accbefore~AccBeforeOut..accafter~AccAfterOut..dref_cond_links~Links
) :-
	append(AccBeforeDiff,OldAcc,AccBeforeIn),
	append(AccBeforeDiff,NewAcc,AccBeforeOut),
	append(AccAfterDiff,OldAcc,AccAfterIn),
	append(AccAfterDiff,NewAcc,AccAfterOut),
	change_subprss(correct_accessibles(OldAcc,NewAcc),CondsIn,CondsOut).
%	correct_accessibles_in_conds(OldAcc,NewAcc,CondsIn,CondsOut).

correct_accessibles_in_conds(OldAcc,NewAcc,[HeadIn|TailIn],[HeadOut|TailOut]) :-
	is_prs(HeadIn),
	!,
	correct_accessibles(OldAcc,NewAcc,HeadIn,HeadOut),
	correct_accessibles_in_conds(OldAcc,NewAcc,TailIn,TailOut).

correct_accessibles_in_conds(OldAcc,NewAcc,[Head|TailIn],[Head|TailOut]) :-
	Head =.. [Functor|_],
    ( Functor = holds; Functor = math_id; Functor = contradiction; Functor = predicate; Functor = in; Functor = plural_dref ),
	!,
	correct_accessibles_in_conds(OldAcc,NewAcc,TailIn,TailOut).

correct_accessibles_in_conds(OldAcc,NewAcc,[HeadIn|TailIn],[HeadOut|TailOut]) :-
	HeadIn =.. [Functor|ArgumentsIn],
	correct_accessibles_in_conds(OldAcc,NewAcc,ArgumentsIn,ArgumentsOut),
	HeadOut =.. [Functor|ArgumentsOut],
	correct_accessibles_in_conds(OldAcc,NewAcc,TailIn,TailOut).

correct_accessibles_in_conds(_,_,[],[]).


%%	dissolve_plurals(+PRSIn,-PRSOut)
%
%	This predicate dissolves any plural PRSs and plural drefs that directly (i.e. not in a subordinated way) occur in PRSIn.

dissolve_plurals(PRSIn,PRSOut) :-
	PRSIn = id~Id..drefs~DrefsIn..mrefs~MrefsIn..conds~CondsIn..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~LinksIn,
	dissolve_plurals_in_conds(CondsIn,CondsOut,NewDrefs,NewLinks,0,NewMrefs),
	append(NewDrefs,DrefsIn,DrefsOut),
	append(NewLinks,LinksIn,LinksOut),
	append(NewMrefs,MrefsIn,MrefsOut),
	TmpPRS = id~Id..drefs~DrefsOut..mrefs~MrefsOut..conds~CondsOut..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~LinksOut,
	recreate_accs(AccBefore,TmpPRS,PRSOut),
	!.

%%	dissolve_plurals_in_conds(+CondsIn,-CondsOut,-NewDrefs,-NewLinks,-NumberOfCondsCreated,-NewMrefs)
%
%	This predicate dissolves all plural PRSs in CondsIn. 
%	The handling of dref_cond_links is correct for the case that all linked conditions come before the plural_dref and plural conditions.

dissolve_plurals_in_conds([plural(PluralDref,PluralPRS)|TailIn],CondsOut,NewDrefs,NewLinks,NIn,NewMrefs) :-
	dissolve_plural_cond(PluralDref,PluralPRS,NIn,NewDrefsFromPluralCond,NewConds,NewLinksFromPluralCond,NewMrefsFromPluralCond),
	length(NewConds,N),
	NOut is N + NIn,
	dissolve_plurals_in_conds(TailIn,TmpConds,TmpNewDrefs,TmpNewLinks,NOut,TmpNewMrefs),
	append(NewDrefsFromPluralCond,TmpNewDrefs,NewDrefs),
	append(NewConds,TmpConds,CondsOut),
	append(NewLinksFromPluralCond,TmpNewLinks,NewLinks),
	append(NewMrefsFromPluralCond,TmpNewMrefs,NewMrefs).

dissolve_plurals_in_conds([Head|TailIn],[Head|TailOut],NewDrefs,NewLinks,N,NewMrefs) :-
	M is N+1,
	dissolve_plurals_in_conds(TailIn,TailOut,NewDrefs,NewLinks,M,NewMrefs).

dissolve_plurals_in_conds([],[],[],[],_,[]).

%%	dissolve_plural_cond(+PluralDref,+PluralPRS,+NumberofCondsCreated,-NewDrefs,-NewConds,-NewLinks,-NewMrefs)
%
%	This predicate dissolves a single plural PRS.

dissolve_plural_cond(PluralDref,PluralPRS,N,NewDrefs,NewConds,NewLinks,NewMrefs) :-
	search_for_identical_drefs([PluralDref],PluralPRS,PluralDrefList),
	mark_conds_with_grouped_arguments_and_their_drefs(PluralDrefList,PluralPRS,MarkedPluralPRS),
	mark_superordinated_drefs_and_their_conds([PluralDref],MarkedPluralPRS,MarkedPluralPRS2),
	pull_out(MarkedPluralPRS2,N,RestPRS,NewDrefs,TmpNewConds,NewLinks,NewMrefs),
	RestPRS = conds~RestConds,
	( 
		RestConds = [] 
		-> 
		NewConds = TmpNewConds 
		; 
		(
			% make_plural_implication(PluralDref,RestPRS,PluralImplication),
			append(TmpNewConds,[plural(PluralDref,RestPRS)],NewConds)  
		)
	).

%%	search_for_identical_drefs(+DrefListIn,+PRS,-DrefListOut)
%
%	DrefListOut is the transitive closure of DrefListIn under the relation of identities in the conds list of PRS.

search_for_identical_drefs(DrefListIn,conds~Conds,DrefListOut) :-
	search_for_identities(DrefListIn,Conds,IdenticalDrefList),
	( IdenticalDrefList = [], DrefListOut = DrefListIn
	  ;
	  (
		append(DrefListIn,IdenticalDrefList,NewDrefList),
		search_for_identical_drefs(NewDrefList,conds~Conds,DrefListOut)
	  )
	).

%%	search_for_identities(+DrefListIn,+CondList,-DrefListOut)
%
%	This predicate searches through CondList and lists in DrefListOut all drefs that are identified with a dref in DrefListIn, but
%	are themselves not in DrefListIn.

search_for_identities(DrefListIn,[predicate(Dref1,Dref2,[=])|TailIn],[Dref2|TailOut]) :-
	( member_without_instantiation(Dref1,DrefListIn) -> \+ member_without_instantiation(Dref2,DrefListIn); member_without_instantiation(Dref2,DrefListIn) ),
	search_for_identities(DrefListIn,TailIn,TailOut).

search_for_identities(DrefListIn,[_|TailIn],DrefListOut) :-
	search_for_identities(DrefListIn,TailIn,DrefListOut).

search_for_identities(_,[],[]).

%%	mark_conds_with_grouped_arguments_and_their_drefs(+PluralDrefList,+PluralPRS,-MarkedPluralPRS)
%
%	Every condition in PluralPRS which has arguments grouped in a plural dref from PluralDrefList 
%	(e.g. "distinct(1)" with a plural dref 1) gets marked with a "remove" prefix; the plural dref in
%	this condition is replaced by the head of PluralDrefList. 
%	Additionally, all drefs in PluralPRS which occur in such conditions are marked with "remove".

mark_conds_with_grouped_arguments_and_their_drefs(PluralDrefList,
	id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links,
	id~Id..drefs~MarkedDrefs..mrefs~Mrefs..conds~MarkedConds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links
) :-
	mark_conds_with_grouped_arguments(PluralDrefList,Conds,MarkedConds),
	mark_drefs_of_group_conds(PluralDrefList,MarkedConds,Drefs,MarkedDrefs).

%%	mark_conds_with_grouped_arguments(+PluralDrefList,+ConditionList,-MarkedConditionList)
%
%	Every condition in PluralPRS which has arguments grouped in a plural dref from PluralDrefList
%	(e.g. "distinct(1)" with a plural dref 1) gets marked with a "remove" prefix; the plural dref in
%   this condition is replaced by the head of PluralDrefList. 

mark_conds_with_grouped_arguments(PluralDrefList,[HeadIn|TailIn],[remove(HeadOut)|TailOut]) :-
	cond_with_grouped_arguments(HeadIn,PluralDref),
	member_without_instantiation(PluralDref,PluralDrefList),
	PluralDrefList = [HeadDref|_],
	condition_drefs(HeadIn,acc,HeadInDrefs),
	substitute_dref_in_dref_list(PluralDref,HeadInDrefs,HeadDref,HeadOutDrefs),
	copy_term([HeadInDrefs,HeadIn],[HeadOutDrefs,HeadOut]),
	mark_conds_with_grouped_arguments(PluralDrefList,TailIn,TailOut).

mark_conds_with_grouped_arguments(PluralDrefList,[Head|TailIn],[Head|TailOut]) :-
	mark_conds_with_grouped_arguments(PluralDrefList,TailIn,TailOut).

mark_conds_with_grouped_arguments(_,[],[]).

%%      substitute_dref_in_dref_list(+Dref1,+DrefList,+Dref2,-ModifiedDrefList)
%
%       Substitutes the occurence of Dref1 in DrefList by Dref2. If Dref1 doesn't occur
%       in DrefList, ModifiedDrefList is DrefList. Dref1 may not appear more than once
%       in DrefList.

substitute_dref_in_dref_list(Dref1,[Head|Tail],Dref2,[Dref2|Tail]) :-
        Head == Dref1.

substitute_dref_in_dref_list(Dref1,[Head|TailIn],Dref2,[Head|TailOut]) :-
        substitute_dref_in_dref_list(Dref1,TailIn,Dref2,TailOut).

substitute_dref_in_dref_list(_,[],_,[]).

%%	cond_with_grouped_arguments(+Condition,-Dref)
%
%	This predicate succeeds if Condition is a condition with grouped argument Dref
%	or the negation of such a condition.

cond_with_grouped_arguments(PredicateCond,Dref) :-
	PredicateCond =.. [predicate|Args],
	length(Args,N),
	last(Args,Predicate),
	% Args contains the predicate's name. So if N is the arity of Predicate,
	% then this is a condition with grouped arguments.
	math_lexicon(Predicate,type~relation..arity~N..grouped~M),
	nth1(M,Args,Dref).

cond_with_grouped_arguments(neg(conds~[PredicateCond]),Dref) :-
	cond_with_grouped_arguments(PredicateCond,Dref).

%%	mark_drefs_of_group_conds(+PluralDrefList,+MarkedConds,+Drefs,-MarkedDrefs)
%
%	Every dref in Drefs, which does not occur in PluralDrefList, but does occur in a condition marked
%	with the "remove" prefix in MarkedConds, gets marked with the "remove" prefix.

mark_drefs_of_group_conds(PluralDrefList,MarkedConds,[Head|TailIn],[Head|TailOut]) :-
	member_without_instantiation(Head,PluralDrefList),
	mark_drefs_of_group_conds(PluralDrefList,MarkedConds,TailIn,TailOut).

mark_drefs_of_group_conds(PluralDrefList,MarkedConds,[Head|TailIn],[remove(Head)|TailOut]) :-
	dref_in_marked_condition(Head,MarkedConds),
	mark_drefs_of_group_conds(PluralDrefList,MarkedConds,TailIn,TailOut).

mark_drefs_of_group_conds(PluralDrefList,MarkedConds,[Head|TailIn],[Head|TailOut]) :-
	mark_drefs_of_group_conds(PluralDrefList,MarkedConds,TailIn,TailOut).

mark_drefs_of_group_conds(_,_,[],[]).

%%	dref_in_marked_condition(+Dref,+MarkedConds)
%
%	Succeeds if Dref appears in some condition in MarkedConds that has a "remove" prefix.

dref_in_marked_condition(Dref,[remove(Cond)|_]) :-
	condition_drefs(Cond,cond,Drefs),
	member_without_instantiation(Dref,Drefs).

dref_in_marked_condition(Dref,[_|Tail]) :-
	dref_in_marked_condition(Dref,Tail).

%% 	mark_superordinated_drefs_and_their_conds(DrefList,PartiallyMarkedPRS,MarkedPRS)
%
%	PartiallyMarkedPRS is a PRS with "remove" prefixes on some conditions and drefs. This predicate marks with a "keep"
%	prefix all conditions without "remove" and with drefs from DrefList. If some of these conditions contain drefs that
%	aren't in DrefList and don't have a "remove" prefix, these drefs are marked with "keep" and added to DrefList, and
%	the process is repeated.

mark_superordinated_drefs_and_their_conds(DrefList,
	id~Id..drefs~Drefs..mrefs~Mrefs..rrefs~Rrefs..conds~Conds..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links,
	id~Id..drefs~MarkedDrefs..mrefs~Mrefs..rrefs~Rrefs..conds~MarkedConds..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links
) :-
	mark_superordinated_conds(DrefList,Conds,TmpMarkedConds),
	( 
		(Conds = TmpMarkedConds, !, MarkedConds = Conds, MarkedDrefs = Drefs)
		;
		mark_superordinated_drefs(DrefList,Drefs,TmpMarkedConds,TmpMarkedDrefs,TmpDrefList),
		mark_superordinated_drefs_and_their_conds(TmpDrefList,drefs~TmpMarkedDrefs..conds~TmpMarkedConds,drefs~MarkedDrefs..conds~MarkedConds)
	).

%%	mark_superordinated_conds(+DrefList,+ConditionList,-MarkedConds)
%
%	This predicate marks with a "keep" prefix all conditions in ConditionList, that aren't marked in any way and that
%	have drefs contained in DrefList.

mark_superordinated_conds(DrefList,[Head|TailIn],[keep(Head)|TailOut]) :-
	\+ Head = remove(_),
	\+ Head = keep(_),
	dref_agreement(Head,DrefList),
	mark_superordinated_conds(DrefList,TailIn,TailOut).

mark_superordinated_conds(DrefList,[Head|TailIn],[Head|TailOut]) :-
	mark_superordinated_conds(DrefList,TailIn,TailOut).

mark_superordinated_conds(_,[],[]).

%%	dref_agreement(+Condition,+DrefList)
%
%	This predicate succeeds if some dref used in Condition appears in DrefList.

dref_agreement(Condition,DrefList) :-
	condition_drefs(Condition,cond,ConditionDrefs),
	!,
	\+ intersection_without_instantiation(ConditionDrefs,DrefList,[]).

%%	condition_drefs(+Condition,+TypeOfDrefs,-ConditionDrefs)
%
%	This predicate lists all drefs appearing in Condition.
%	If TypeOfDrefs is cond, it lists only drefs used in conditions
%	of sub-PRSs. If TypeOfDref is acc, it also lists drefs used in
%	accessibles lists of sub-PRSs. In that case, Condition may also
%	be a Dref, in which case it will be listed too.

condition_drefs(Dref,acc,[Dref]) :-
	var(Dref),
	!.

condition_drefs(predicate(Dref,_),_,[Dref]) :-
	!.

condition_drefs(predicate(Dref1,Dref2,_),_,[Dref1,Dref2]) :-
	!.

condition_drefs(holds(Dref),_,[Dref]) :-
	!.

condition_drefs(in(Dref1,Dref2),_,[Dref1,Dref2]) :-
	!.

condition_drefs(plural_dref(Dref,DrefList),_,[Dref|DrefList]) :-
	!.

condition_drefs(contradiction,_,[]) :-
	!.

condition_drefs(math_id(Dref,Formula),_,Drefs) :-
	formula_drefs(Formula,FormulaDrefs),
	union_without_instantiation([Dref],FormulaDrefs,Drefs),
	!.

condition_drefs(PRS,cond,Drefs) :-
	is_prs(PRS),
	PRS = conds~Conds,
	conds_drefs(Conds,cond,Drefs),
	!.

condition_drefs(PRS,acc,Drefs) :-
	is_prs(PRS),
	PRS = conds~Conds..accbefore~AccBefore..accafter~AccAfter,
	conds_drefs(Conds,acc,Drefs1),
	conds_drefs(AccBefore,acc,Drefs2),
	conds_drefs(AccAfter,acc,Drefs3),
	union_without_instantiation(Drefs1,Drefs2,Drefs1_2),
	union_without_instantiation(Drefs1_2,Drefs3,Drefs),
	!.

condition_drefs(ComplexCondition,Type,Drefs) :-
	ComplexCondition =.. [_|PRSList],
	prs_list_drefs(PRSList,Type,Drefs),
	!.

%%	conds_drefs(+ConditionList,+TypeOfDrefs,-Drefs)
%
%	This predicate lists all drefs appearing in any of the conditions in ConditionList.
%	If TypeOfDrefs is cond, it lists only drefs used in conditions
%	of sub-PRSs. If TypeOfDref is acc, it also lists drefs used in
%	accessibles lists of sub-PRSs.

conds_drefs([Head|Tail],Type,Drefs) :-
	condition_drefs(Head,Type,HeadDrefs),
	conds_drefs(Tail,Type,TailDrefs),
	union_without_instantiation(HeadDrefs,TailDrefs,Drefs).

conds_drefs([],_,[]).

%%	prs_list_drefs(+PRSList,+TypeOfDrefs,-Drefs)
%
%	This predicate lists all drefs appearing in any PRS in PRSList.
%	If TypeOfDrefs is cond, it lists only drefs used in conditions
%	of sub-PRSs. If TypeOfDref is acc, it also lists drefs used in
%	accessibles lists of sub-PRSs.
%	Instead of a PRS list, one can also enter a list of condition
%	arguments, of which only some are PRSs. Then all those arguments
%	which aren't PRSs are ignored.

prs_list_drefs([Arg|Tail],Type,Drefs) :-
	\+ is_prs(Arg),
	prs_list_drefs(Tail,Type,Drefs).

prs_list_drefs([conds~Conds|Tail],cond,Drefs) :-
	conds_drefs(Conds,cond,HeadDrefs),
	prs_list_drefs(Tail,cond,TailDrefs),
	union_without_instantiation(HeadDrefs,TailDrefs,Drefs).

prs_list_drefs([conds~Conds..accbefore~AccBefore..accafter~AccAfter|Tail],acc,Drefs) :-
	conds_drefs(Conds,acc,HeadDrefs1),
	conds_drefs(AccBefore,acc,HeadDrefs2),
	conds_drefs(AccAfter,acc,HeadDrefs3),
	prs_list_drefs(Tail,acc,TailDrefs),
	union_without_instantiation(HeadDrefs1,HeadDrefs2,HeadDrefs1_2),
	union_without_instantiation(HeadDrefs1_2,HeadDrefs3,HeadDrefs),
	union_without_instantiation(HeadDrefs,TailDrefs,Drefs).

prs_list_drefs([],_,[]).

%%	formula_drefs(+FormulaTree,-Drefs)
%
%	This predicate lists all drefs attached to a variable in FormulaTree (which can be the presentation of an fo_formula or an fo_term).

formula_drefs(type~relation..args~Args,Drefs) :-
	formula_list_drefs(Args,Drefs).
	
formula_drefs(FormulaTree,[Dref]) :-
	FormulaTree = type~variable,
	subsumes(dref~Dref,FormulaTree).

formula_drefs(type~variable,[]).

formula_drefs(type~constant,[]).

formula_drefs(type~function..args~Args,Drefs) :-
	formula_list_drefs(Args,Drefs).

formula_drefs(type~logical_symbol..args~Args,Drefs) :-
	formula_list_drefs(Args,Drefs).

formula_drefs(type~quantifier..args~[_|Formula],Drefs) :-
	formula_drefs(Formula,Drefs).

%%	formula_list_drefs(+FormulaTreeList,-Drefs)
%
%	This predicate lists all drefs attached to a variable in any of the formula tree in FormulaTree.

formula_list_drefs([Head|Tail],Drefs) :-
	formula_drefs(Head,HeadDrefs),
	formula_list_drefs(Tail,TailDrefs),
	union_without_instantiation(HeadDrefs,TailDrefs,Drefs).

formula_list_drefs([],[]).

%%	mark_superordinated_drefs(+DrefList,+Drefs,+MarkedConds,-MarkedDrefs,-ExtendedDrefList)
%
%	This predicate looks at all drefs in conditions that appear in MarkedConds with a "keep" prefix. If one of these
%	drefs is not in DrefList but in Drefs (without a "remove" prefix), then it gets marked with a "keep"
%	prefix and added to ExtendedDrefList.

mark_superordinated_drefs(DrefList,Drefs,[keep(Head)|Tail],MarkedDrefs,ExtendedDrefList) :-
	condition_drefs(Head,cond,ConditionDrefs),
	subtract_without_instantiation(ConditionDrefs,DrefList,PotentiallyMarkedDrefs,_),
	mark_superordinated_drefs_aux(PotentiallyMarkedDrefs,Drefs,TmpMarkedDrefs,ExtraDrefs),
	append(ExtraDrefs,DrefList,TmpExtendedDrefList),
	mark_superordinated_drefs(TmpExtendedDrefList,TmpMarkedDrefs,Tail,MarkedDrefs,ExtendedDrefList).

mark_superordinated_drefs(DrefList,Drefs,[_|Tail],MarkedDrefs,ExtendedDrefList) :-
	mark_superordinated_drefs(DrefList,Drefs,Tail,MarkedDrefs,ExtendedDrefList).
	
mark_superordinated_drefs(DrefList,Drefs,[],Drefs,DrefList).

%%	mark_superordinated_drefs_aux(+DrefList,+Drefs,-MarkedDrefs,-ExtraDrefs)
%
%	This predicate looks at all drefs in DrefList. If one of these drefs is in Drefs (without a
%	"remove" prefix), then it gets marked with a "keep" prefix and added to ExtraDrefs. So in the end,
%	ExtraDrefs is a list of all drefs that appear marked with "keep" in MarkedDrefs.

mark_superordinated_drefs_aux([Head|Tail],Drefs,MarkedDrefs,[Head|ExtraDrefTail]) :-
	member_without_instantiation(Head,Drefs),
	mark_dref(Head,Drefs,TmpMarkedDrefs),
	mark_superordinated_drefs_aux(Tail,TmpMarkedDrefs,MarkedDrefs,ExtraDrefTail).

mark_superordinated_drefs_aux([_|Tail],Drefs,MarkedDrefs,ExtraDrefTail) :-
	mark_superordinated_drefs_aux(Tail,Drefs,MarkedDrefs,ExtraDrefTail).

mark_superordinated_drefs_aux([],Drefs,Drefs,[]).
	
%%	mark_dref(+Dref,+Drefs,-MarkedDrefs)
%
%	This predicate marks the occurence of Dref in Drefs with a "keep" prefix.

mark_dref(Dref1,[Dref2|Tail],[keep(Dref1)|Tail]) :-
	Dref1 == Dref2.

mark_dref(Dref,[Head|TailIn],[Head|TailOut]) :-
	mark_dref(Dref,TailIn,TailOut).

%%	pull_out(+MarkedPRS,+NumberOfCondsCreated,-RestPRS,-NewDrefs,-NewConds,-NewLinks,-NewMrefs)
%
%	This predicate pulls out all conditions and drefs from MarkedPRS that aren't prefixed with "keep", and
%	allows to add the pulled out material to the super-PRS. RestPRS is what remains after the pulling out
%	process. NewDrefs, NewConds, NewLinks and NewMrefs are what has to be added to the super-PRS.
%	It is assumed that any dref_conds_links in MarkedPRS are either between drefs and conditions that are
%	to be pulled out, or between drefs and conditions that are to be kept (actually, the resolve_plural
%	algorithm that uses this predicate has the stronger property that no dref to be kept is used in a 
%	condition to be pulled out and no condition to be kept contains a dref to be pulled out).

pull_out(
	id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links,
	N,
	id~Id..drefs~RestDrefs..mrefs~RestMrefs..conds~RestConds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~RestLinks,
	NewDrefs,NewConds,NewLinks,NewMrefs) :-
	single_pull_out(Drefs,RestDrefs,NewDrefs),
	single_pull_out(Conds,RestConds,NewConds),
	adapt_mrefs(NewConds,Mrefs,RestMrefs,NewMrefs),
	cond_number_correction_function(Conds,RestConds,1,CorrectionFunction),
	adapt_links(CorrectionFunction,Links,N,RestLinks,NewLinks).

%%	single_pull_out(+List,-RestList,-NewList)
%
%	List is a list of items (e.g. drefs or conditions), partially prefixed with "keep" and partially prefixed with "remove". 
%	All items with the "keep" prefix are listed in RestList, all others in NewList, whereby all prefixes are dropped.

single_pull_out([Head|Tail],[Item|RestTail],NewList) :-
	subsumes(keep(Item),Head),
	single_pull_out(Tail,RestTail,NewList).

single_pull_out([Head|Tail],RestList,[Item|NewTail]) :-
	subsumes(remove(Item),Head),
	single_pull_out(Tail,RestList,NewTail).

single_pull_out([Head|Tail],RestList,[Head|NewTail]) :-
	single_pull_out(Tail,RestList,NewTail).

single_pull_out([],[],[]).

%%	adapt_mrefs(+Conds,+Mrefs,-RestMrefs,-NewMrefs)
%
%	All mrefs in Mrefs that appear in math_id condition in Conds are placed into NewMrefs. 
%	The others are placed into RestMrefs.

adapt_mrefs(Conds,[Head|Tail],RestMrefs,[Head|NewTail]) :-
	member(math_id(_,Head),Conds),
	adapt_mrefs(Conds,Tail,RestMrefs,NewTail).

adapt_mrefs(Conds,[Head|Tail],[Head|RestTail],NewMrefs) :-
	adapt_mrefs(Conds,Tail,RestTail,NewMrefs).

adapt_mrefs(_,[],[],[]).

%%	adapt_accessibles(+NewDrefs,+NewConds,-NewAccessibles)
%
%	For any dref Dref in NewDrefs we add something to NewAccssibles: If a math_id or plural_dref
%	condition with Dref appears in NewConds, this condition will be added, else Dref itself. 

adapt_accessibles([Head|Tail],Conds,[math_id(Head,Mref)|AccTail]) :-
	search_math_id(math_id(Head,Mref),Conds),
	adapt_accessibles(Tail,Conds,AccTail).

adapt_accessibles([Head|Tail],Conds,[plural_dref(Head,DrefList)|AccTail]) :-
	search_plural_dref(plural_dref(Head,DrefList),Conds),
	adapt_accessibles(Tail,Conds,AccTail).

adapt_accessibles([Head|Tail],Conds,[Head|AccTail]) :-
	adapt_accessibles(Tail,Conds,AccTail).

adapt_accessibles([],_,[]).

%%	cond_number_correction_funtion(+Conds,+RestConds,+Index,-CorrectionFunction)
%
%	For every condition Cond in RestConds, we add a pair [N,M] to CorrectionFunction,
%	where N is the position of Cond in Conds and M is the position of Cond in RestConds.
%	(So RestConds is assumed to be a subset of Conds.) Index is 1 to start with, and
%	records how much of RestConds has been parsed.

cond_number_correction_function(Conds,[Head|Tail],M,[[N,M]|FunctionTail]) :-
	nth1_without_instantiation(N,Conds,keep(Head)),
	M1 is M+1,
	cond_number_correction_function(Conds,Tail,M1,FunctionTail).

cond_number_correction_function(_,[],_,[]).

%%	adapt_links(+CorrectionFunction,+Links,+NumberOfCondsCreated,-RestLinks,-NewLinks)
%
%	For every dref_cond_link [DrefList,CondNumber] in Links, one link is created and placed either into 
%	RestLinks (if CondNumber is in the domain of CorrectionFunction) or into NewLink (otherwise). The 
%	condition numbers for the links in RestLinks have to be adapted according to the CorrectionFunction.
%	The condition numbers for the links in NewLinks are created on top of NumberOfCondsCreated.

adapt_links(CorrectionFunction,[[DrefList,CondNumber]|Tail],N,[[DrefList,AdaptedCondNumber]|RestTail],NewLinks) :-
	member([CondNumber,AdaptedCondNumber],CorrectionFunction),
	adapt_links(CorrectionFunction,Tail,N,RestTail,NewLinks).

adapt_links(CorrectionFunction,[[DrefList,CondNumber]|Tail],N,RestLinks,[[DrefList,AdaptedCondNumber]|NewTail]) :-
	AdaptedCondNumber is CondNumber + N,
	M is N+1,
	adapt_links(CorrectionFunction,Tail,M,RestLinks,NewTail).

adapt_links(_,[],_,[],[]). 

%%	recreate_accs(+AccBefore,+PRSIn,-PRSOut)
%
%	All accessible lists in the PRS and its sub-PRSs are recreated according to the dref and conds lists
%	of the PRS, with AccBefore as new accbefore list.
%	This is currently only correctly implemented for PRSs at or below the sentence level.

recreate_accs(AccBefore,
	id~Id..drefs~Drefs..mrefs~Mrefs..conds~CondsIn..rrefs~Rrefs..dref_cond_links~Links,
	id~Id..drefs~Drefs..mrefs~Mrefs..conds~CondsOut..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links
) :-
	recreate_accafter(AccBefore,Drefs,CondsIn,TmpAccAfter),
	recreate_accs_in_subprss(TmpAccAfter,CondsIn,CondsOut,AccAfter).

%%	recreate_accafter(+AccBefore,+Drefs,+Conds,-AccAfter)
%
%	AccAfter is made of AccBefore and additionally one element for every dref in Dref.
%	If the concerning dref is linked to an mref or dref list by a math_id or plural_dref
%	condition in Conds, this math_id or plural_dref condition is added to AccAfter. 
%	Else the dref itself is added to AccAfter.

recreate_accafter(AccBefore,[Head|Tail],Conds,[math_id(Head,Mref)|AccTail]) :-
	search_math_id(math_id(Head,Mref),Conds),
	recreate_accafter(AccBefore,Tail,Conds,AccTail).

recreate_accafter(AccBefore,[Head|Tail],Conds,[plural_dref(Head,DrefList)|AccTail]) :-
	search_plural_dref(plural_dref(Head,DrefList),Conds),
	recreate_accafter(AccBefore,Tail,Conds,AccTail).

recreate_accafter(AccBefore,[Head|Tail],Conds,[Head|AccTail]) :-
	recreate_accafter(AccBefore,Tail,Conds,AccTail).

recreate_accafter(AccBefore,[],_,AccBefore).

%%	recreate_accs_in_subprss(+AccBefore,+CondsIn,-CondsOut,-AccAfter)
%
%	In all sub-PRSs in CondsIn the accessible lists are recreated with AccBefore
%	as accbefore. AccAfter is the accessible list at the end of the conditions.

recreate_accs_in_subprss(AccBefore,[Head|TailIn],[Head|TailOut],AccAfter) :-
	Head =.. [Functor|_],
	( Functor = holds; Functor = math_id; Functor = contradiction; Functor = predicate; Functor = plural_dref; Functor = in ),
	recreate_accs_in_subprss(AccBefore,TailIn,TailOut,AccAfter).

recreate_accs_in_subprss(AccBefore,[HeadIn|TailIn],[HeadOut|TailOut],AccAfter) :-
	is_prs(HeadIn),
	recreate_accs_in_subprss(AccBefore,TailIn,TailOut,TmpAcc),
	recreate_accs(TmpAcc,HeadIn,HeadOut),
	HeadOut = accafter~AccAfter.

recreate_accs_in_subprss(AccBefore,[PRS1In => PRS2In|TailIn],[PRS1Out => PRS2Out|TailOut],AccAfter) :-
	recreate_accs_in_subprss(AccBefore,TailIn,TailOut,AccAfter),
	recreate_accs(AccAfter,PRS1In,PRS1Out),
	PRS1Out = accafter~Acc,
	recreate_accs(Acc,PRS2In,PRS2Out).

recreate_accs_in_subprss(AccBefore,[HeadIn|TailIn],[HeadOut|TailOut],AccAfter) :-
	HeadIn =.. [plural,Dref,PRSIn],
	recreate_accs_in_subprss(AccBefore,TailIn,TailOut,AccAfter),
	recreate_accs(AccBefore,PRSIn,PRSOut),
	HeadOut =.. [plural,Dref,PRSOut].

recreate_accs_in_subprss(AccBefore,[HeadIn|TailIn],[HeadOut|TailOut],[Dref|AccAfter]) :-
	HeadIn =.. [the,Dref,PRSIn],
	recreate_accs_in_subprss(AccBefore,TailIn,TailOut,AccAfter),
	recreate_accs(AccAfter,PRSIn,TmpPRS),
	TmpPRS = id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~TmpAccAfter..dref_cond_links~Links,
	( search_math_id(math_id(Dref,Mref),Conds) -> AccAfterOut = [math_id(Dref,Mref)|TmpAccAfter] ; AccAfterOut = TmpAccAfter ),
	PRSOut = id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfterOut..dref_cond_links~Links,
	HeadOut =.. [the,Dref,PRSOut].

recreate_accs_in_subprss(AccBefore,[HeadIn|TailIn],[HeadOut|TailOut],AccAfter) :-
	HeadIn =.. [Functor|ArgsIn],
	recreate_accs_in_subprss(AccBefore,TailIn,TailOut,AccAfter),
	maplist(recreate_accs(AccAfter),ArgsIn,ArgsOut),
	HeadOut =.. [Functor|ArgsOut].

recreate_accs_in_subprss(Acc,[],[],Acc).


%%	add_accessible(+Features,+PRSIn,+Accessible,-PRSOut)
%
%	If the add_dref feature in Features has value "yes", this predicate adds Accessible to the 
%	accessible lists of the PRS and all its sub-PRSs. Else PRSOut is PRSIn.

add_accessible(add_dref~yes,PRSIn,Acc,PRSOut) :-
	PRSIn = accbefore~AccBefore,
	correct_accessibles(AccBefore,[Acc|AccBefore],PRSIn,PRSOut).

add_accessible(_,PRS,_,PRS).


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


%%	update_var_types(+SymbolData,+Noun,+VarTypesIn,-VarTypesOut)
%
%	This predicate updates the list of links between symbol collections and tyoe nouns.
%	@param VarTypesIn and VarTypesOut are lists of pairs consisting of a symbol collection
%	feature structure and a noun.

update_var_types([HeadSD|TailSD],Noun,VarTypesIn,VarTypesOut) :-
	update_var_types(TailSD,Noun,[[HeadSD,Noun]|VarTypesIn],VarTypesOut).

update_var_types([],_,VarTypesIn,VarTypesIn).
