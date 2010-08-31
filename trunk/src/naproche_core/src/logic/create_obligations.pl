:-module(create_obligations,[create_obligations/3]).

:- use_module(library(pldoc)).
:- use_module(library(ugraphs)).

:- ensure_loaded(naproche(gulp4swi)).
:- use_module(naproche(premises)).
:- use_module(naproche(discharge_obligations)).
:- use_module(naproche(prs)).
:- use_module(naproche(error_logger)).
:- use_module(naproche(fo_grammar)).
:- use_module(naproche(math_lexicon)).
:- use_module(naproche(translation_tptp)).
:- use_module(naproche(graph)).

% assumption marker
:- op(901, xfx, user:(=>)).

% implication marker
:- op(901, xfx, user:(==>)).

% definition marker
:- op(901, xfx, user:(:=)).

% disjunction marker
:- op(901, xfx, user:(v)).

% reverse implication marker
:- op(901, xfx, user:(<=)).

% equivalence marker
:- op(901, xfx, user:(<=>)).

% function marker
:- op(1000,xfx, user:(::)).
	

/**	<module> High level proof checking predicate
 *
 * 	This module provides predicates that create proof obligations for ATPs.
 *
 */	

%%	create_obligations(+PRS,-Graph:list,-Obligations:list)
%
%	Parses a PRS and creates a list of all things that need to be checked (Obligations)
%	Graph is the Graph of the PRS.

create_obligations(PRS,Graph,Obligations) :-
	PRS = id~Id..accafter~Accafter..conds~Conds,
	check_conditions(Id,Accafter,Conds,0,Id,[],Graph,[],Obligations,[],_PremisesOut,check).


%%	check_prs(+PRS:prs,+Accafter,+GraphIn,-GraphOut,+ObligationsIn:list,-ObligationsOut:list,+PremisesBegin:list(DOBSOD),-PremisesEnd:list(DOBSOD),
%%	+CheckTrigger:(check | nocheck)) is det.
%
%	True if all necessary obligations could be created, this should always be the case.
%
%	@param PRS is the PRS to be checked.
%	@param GraphIn is the Graph before this PRS.
%	@param GraphOut is GraphIn+the Graph of the PRS
%	@param ObligationsIn is the list of Obligations before checking the PRS
%	@param ObligationsOut is the list of ObligationsIn+ all new Obligations that were created through the prs.
%	@param PremisesEnd is the Graph after this PRS.
%	@param PremisesBegin is the list of premises accessible from before this PRS.
%	@param PremisesEnd is the list containing PremisesBegin and every formula in this PRS.
%	@param Check trigger determins whether proof obilgation should be created or not.


check_prs(PRS,_Accafter,GraphIn,GraphOut,ObIn,ObOut,PremisesIn,PremisesOut,CheckTrigger) :-
	% Case that PRS is an induction PRS
	PRS = id~Id..conds~Conds..drefs~Drefs..accbefore~AccbeforePRS..accafter~AccafterPRS..rrefs~Rrefs,
	member('induction',Rrefs),	

	% The PRS must not contain any variables
	get_free_var(AccafterPRS,Drefs,[]),
	
	% get the premises of the PRS and throw error message if they are empty
	check_conditions(Id,AccafterPRS,Conds,0,Id,GraphIn,GraphPRS,ObIn,ObPRS,PremisesIn,PremisesInAndPRS,nocheck),
	sort_premises_for_the(Id,AccbeforePRS,PremisesIn,PremisesInAndPRS,GraphPRS,GraphTmp,PremisesInAndThe,PremisesPRS,CheckTrigger),
	error_if_empty(PremisesPRS,Id,'Induction PRS is empty'),	
	
	% PremisesPRS has to be one universally quantified formula
	PremisesPRS = [UnivQuantifiedFormula],
	(
	(UnivQuantifiedFormula = name~'!',!)
	;
	(
	term_to_atom(Id,AtomId),
	add_error_message(logic,'check_prs',AtomId,'Wrong formula for induction PRS (must be universally quantified).'),
	!,
	fail
	)),

	% Make Induction Formulas
	make_induction_formulas(Id,PremisesPRS,Formula1,FormulaSucc),	
	
	% Check Formulas if CheckTrigger = check 
	create_obligation_if_trigger(PremisesInAndThe,Formula1,Id,CheckTrigger,ObPRS,ObTmp),
	create_obligation_if_trigger(PremisesInAndThe,FormulaSucc,Id,CheckTrigger,ObTmp,ObOut),
	
	% Update Graph
	Formula1 = [id~StartId],
	FormulaSucc = [id~StepId],
	UnivQuantifiedFormula = id~FormulaId,
	add_edges(GraphTmp,[Id-FormulaId,FormulaId-Id,FormulaId-StartId,FormulaId-StepId,StartId-Id,StepId-Id],GraphTmp2),
	add_rrefs(GraphTmp2,Rrefs,Id,GraphOut),
	
	% update PremisesBegin
	PremisesOut = [UnivQuantifiedFormula|PremisesInAndThe],!.

% Error case
check_prs(PRS,_Accafter,_GraphIn,_GraphOut,_ObIn,_ObOut,_PremisesBegin,_PremisesEnd,_CheckTrigger) :-
	% Case that PRS is an induction PRS
	PRS = id~Id..rrefs~Rrefs,
	member('induction',Rrefs),	
	!,
	term_to_atom(Id,AtomId),
	add_error_message(logic,'check_prs',AtomId,'Could not check induction PRS'),
        fail.

check_prs(PRS,_Accafter,GraphIn,GraphOut,ObIn,ObOut,PremisesBegin,PremisesEnd,CheckTrigger) :-
	% If PRS is not a theorem and has no variables in Mrefs just check the conditions
	PRS = id~Id..conds~Conds..drefs~Drefs..accafter~AccafterPRS..rrefs~Rrefs,
	get_free_var(AccafterPRS, Drefs,[]),

	% Update Graph
	add_rrefs(GraphIn,Rrefs,Id,TmpGraphOut),

	check_conditions(Id,AccafterPRS,Conds,0,Id,TmpGraphOut,GraphOut,ObIn,ObOut,PremisesBegin,PremisesEnd,CheckTrigger),
	!.

check_prs(PRS,Accafter,GraphIn,GraphOut,ObIn,ObOut,PremisesBegin,PremisesEnd,CheckTrigger) :-
	% If PRS is not a theorem but has variables in Mrefs then check_sentence_prs
    PRS = id~Id..drefs~Drefs..accafter~AccafterPRS..rrefs~Rrefs,
    
	\+ (Id = theorem(_); Id = lemma(_);Id=axiom(_)),
	\+ get_free_var(AccafterPRS, Drefs,[]),

	% Update Graph
	add_rrefs(GraphIn,Rrefs,Id,TmpGraphOut),

	check_sentence_prs(PRS,Accafter,TmpGraphOut,GraphOut,ObIn,ObOut,PremisesBegin,PremisesEnd,CheckTrigger),
	!.

% Error case
check_prs(Wrong_PRS,_,_, _, _, _,_,_,_) :-
	Wrong_PRS = id~Id,
	term_to_atom(Id,AtomId),
	add_error_message(logic,'check_prs',AtomId,'Could not check PRS'),
        fail.


% -------------------------- Check Sentence PRS ---------------------------------------


%%	check_sentence_prs(+PRS:prs,+GraphIn,-GraphOut,+ObligationsIn,-ObligationsOut,+PremisesBegin:list(DOBSOD),-PremisesEnd:list(DOBSOD),
%%	+CheckTrigger:(check | nocheck)) is det.
%	
%	Gets the formula image of PRS,
%	conjuncts the formulas to one formula Formula,
%	existentially quantifies over the variables in Mrefs,
%	checks the quantified formula if CheckTrigger=check,
%	and adds Formula to the Premises.
%
%	@author Sep 09, dk


check_sentence_prs(PRS,Accafter,GraphIn,GraphOut,ObIn,ObOut,PremisesIn,PremisesOut,CheckTrigger) :- 
	% Get the premises
	PRS = id~Id..conds~Conds..accbefore~AccbeforePRS..accafter~AccafterPRS,
	get_all_new_drefs(PRS,Drefs),
	get_free_var(AccafterPRS,Drefs,VariablesPRS),
	get_free_var(Accafter,Drefs,VariablesAll),
	subtract(VariablesPRS,VariablesAll,Variables),
	!,

	check_conditions(Id,AccafterPRS,Conds,0,Id,GraphIn,GraphPRS,ObIn,ObPRS,PremisesIn,PremisesTmp,nocheck),
	
	sort_premises_for_the(Id,AccbeforePRS,PremisesIn,PremisesTmp,GraphPRS,GraphTmp,PremisesInAndThe,PremisesPRS,CheckTrigger),

	% Conjunct the premises and quantify existentially over variables in Mrefs
	quantify_existentially(Id,VariablesPRS,PremisesPRS,ObFormula),	
	quantify_existentially(Id,Variables,PremisesPRS,QNewFormula),	
	create_obligation_if_trigger(PremisesInAndThe,ObFormula,Id,CheckTrigger,ObPRS,ObOut),
	
	( PremisesPRS = [] -> 
	(
	GraphOut = GraphTmp,
	PremisesOut = PremisesInAndThe
	);
	( 
	
	% Update Graph
	% Dont add edges to the Formulas from which QNewFormula was derived since they were created with nocheck anyways.
	QNewFormula = [id~FormulaId],
	ObFormula = [id~ObFormulaId],
	add_formula_link(FormulaId,PremisesPRS,GraphTmp,GraphTmp2),
	add_edges(GraphTmp2,[FormulaId-Id,Id-FormulaId,ObFormulaId-Id,Id-ObFormulaId],GraphOut),

	( CheckTrigger = check ->
		(
		append(PremisesPRS,PremisesInAndThe,PremisesOut)
		);
		(
		QNewFormula = [QFormula],
		PremisesOut = [QFormula | PremisesInAndThe]
		)
	)
	)).


% ----------------------------------- check_neg_prs -------------------------------------------------------------------------
	

%%	check_neg_prs(+NEG_PRS:neg(prs),+GraphIn,-GraphOut,+ObligationsIn,-ObligationsOut,+PremisesBegin:list(DOBSOD),-PremisesEnd:list(DOBSOD),
%%	+CheckTrigger:(check | nocheck)) is det.
%
%	Depending on CheckTrigger, creates proof obligations for a negated PRS
%
%	@param NEG_PRS is a negated PRS.
%	@param PremisesBegin is the list of premises from which we try to prove the PRS.
%	@param PremisesEnd is the list containing PremisesBegin and every formula we proved through the PRS.
%	@param Check trigger gives us the option to either try to prove every formula we encounter ( check ) or
%	 to just go through the structure of the proof without running a prover (nocheck).

check_neg_prs(neg(PRS),Accafter,GraphIn,GraphOut,ObIn,ObOut,PremisesIn,PremisesOut,CheckTrigger) :-
	!,
	PRS = id~Id..accbefore~AccbeforePRS,

	% Get the premises of the PRS
	check_prs(PRS,Accafter,GraphIn,GraphPRS,ObIn,ObPRS,PremisesIn,PremisesInAndPRS,nocheck),
	sort_premises_for_the(Id,AccbeforePRS,PremisesIn,PremisesInAndPRS,GraphPRS,GraphTmp,PremisesInAndThe,PremisesPRS,CheckTrigger),

	(PremisesPRS = [] ->
		(
		add_warning_message('Empty negated PRS',Id,'','There is an empty negated PRS, this was probably not intentional'),
		GraphTmp=GraphOut,ObPRS=ObOut,PremisesInAndPRS = PremisesOut);
		(

		% Conjunct the premises, quantify existentially over Mrefs and negate
		make_conjunction_list(PremisesPRS,Formula),
		negate_formulas(Id,Formula,NegatedFormula),

		% Check the negated formula if CheckTrigger=check
		create_obligation_if_trigger(PremisesInAndThe,NegatedFormula,Id,CheckTrigger,ObPRS,ObOut),
	
		% Update Graph
		% Dont add edges to the Formulas since they were created with nocheck anyways.
		NegatedFormula = [id~NegFormulaId],
		add_edges(GraphTmp,[NegFormulaId-Id,Id-NegFormulaId],GraphOut),

		% Append the negated Premises
		NegatedFormula = [NegFormula],
		PremisesOut = [NegFormula | PremisesInAndThe]
		)
	).


% ----------------------------------- check_conditions -------------------------------------------------------------------------


%%	check_conditions(+Id:atom,+Accbefore:list,+Conditions:list
%%	+Number:int,+TopNode,+GraphIn,-GraphOut,
%%	+ObligationsIn,-ObligationsOut,
%%	+PremisesBegin:list(DOBSOD),-PremisesEnd:list(DOBSOD),
%%	+CheckTrigger:(check|nocheck)) is det.
%
%	True if every Element of the List is logically valid, or if the list is empty
%
%	check_conditions checks the Elements of the List one ofter the other. 
%
%	@param Id is the ID of the PRS we are checking
%	@param Accbefore is the list of accessible Math_IDs of the PRS
%	@param Conditions is the list of conditions which we try to prove.
%	@param Number is the Number of the condition of PRS Id
%	@param TopNode is the Vertice which corresponds to the condition befor this condition
%	@param GraphIn Is the Graph befor parsing this cond
%	@param GraphOut is the Graph after parsing this cond
%	@param PremisesBegin is the list of premises from which we try to prove the PRS.
%	@param PremisesEnd is the list containing PremisesBegin and every formula we proved through the PRS.
%	@param Check trigger determins whether proof obligations are created or not


% ------------------------- Empty List ----------------------------------------
% Empty list is valid.
% PremisesBegin = PremisesEnd
check_conditions(Id,_Accafter,[],_N,TopNode,GraphIn,GraphOut,ObIn,ObIn,PremisesBegin,PremisesBegin,_) :- 
	!,
	( (Id = TopNode;Id = conseq(_)) ->
		GraphIn=GraphOut;
		add_edges(GraphIn,[Id-TopNode],GraphOut)
	).


% ------------------------- PRS ----------------------------------------
% X is a PRS
% Check PRS then proceed with the updated Premises.

% If prs is of the type conjunct1 or comma_conjunct1, then take the current PRS +  the next condition (which should be conjunct2), conjunct the premises
% and quantify existentially over the Mrefs of the first PRS. An example is:
%
% "$x$ is even, then $x$ is prime." gives a pair of PRSs, conjunct1 and conjunct2, but the Formula Image is 
% exists x (even(x) and prime(x)).
check_conditions(Id,AccafterAll,[PRS1|Rest],N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesIn,PremisesOut,CheckTrigger) :-
	is_prs(PRS1),
	( PRS1 = id~conjunct1(_) ; PRS1 = id~comma_conjunct1(_) ),
	!,

	Rest = [PRS2 | Rest2 ],

	PRS1 = id~PRSId1..accbefore~AccbeforePRS1..accafter~Accafter1..conds~Conds1,
	PRS2 = id~PRSId2..accbefore~AccbeforePRS2..accafter~Accafter2..conds~Conds2,
	
	get_all_new_drefs(PRS1,DrefsPRS1),
	get_all_new_drefs(PRS2,DrefsPRS2),
	append(DrefsPRS1,DrefsPRS2,Drefs),

	get_free_var(Accafter2,Drefs,FreeVarCond),

	(FreeVarCond = [] ->
	(
	% If there are no new variables, deal with it normally, if there are new variables, conjunct and quantify existentially
	check_prs(PRS1,AccafterAll,GraphIn,GraphPRS1,ObIn,ObTmp1,PremisesIn,PremisesInAnd1,CheckTrigger),
	check_prs(PRS2,AccafterAll,GraphPRS1,GraphPRS2,ObTmp1,ObTmp,PremisesInAnd1,PremisesTmp,CheckTrigger),

	add_edges(GraphPRS2,[PRSId1-TopNode,PRSId2-PRSId1],GraphTmp),
	NewTopNode = PRSId2

	);
	(
	check_conditions(PRSId1,Accafter1,Conds1,0,PRSId1,GraphIn,GraphPRS1,ObIn,ObTmp1,PremisesIn,PremisesInAnd1,nocheck),
	check_conditions(PRSId2,Accafter2,Conds2,0,PRSId2,GraphPRS1,GraphPRS2,ObTmp1,ObTmp2,PremisesInAnd1,PremisesInAnd2,nocheck),

	sort_premises_for_the(Id,AccbeforePRS1,PremisesIn,PremisesInAnd1,GraphPRS2,GraphTmp1,PremisesIn1,Premises1,CheckTrigger),
	append(Tmp2,PremisesInAnd1,PremisesInAnd2),
	sort_premises_for_the(Id,AccbeforePRS2,[],Tmp2,GraphTmp1,GraphTmp2,PremisesIn2,Premises2,CheckTrigger),
	append(Premises2,Premises1,NewPremisesTmp),

	% We have to find the free variables over which we'll quantify
	% Only take the free var that are not accessile after
	get_free_var(AccafterAll,Drefs,FreeVarAll),
	subtract(FreeVarCond,FreeVarAll,FreeVar),

	make_conjunction_list(NewPremisesTmp,ConjPremises),
	quantify_existentially(and(PRSId1,PRSId2),FreeVar,ConjPremises,[NewFormula]),
	quantify_existentially(and(PRSId1,PRSId2),FreeVarAll,ConjPremises,ObFormula),
	
	append(PremisesIn2,PremisesIn1,PremisesObligation),
	create_obligation_if_trigger(PremisesObligation,ObFormula,Id,CheckTrigger,ObTmp2,ObTmp),

	!,
	PremisesTmp = [NewFormula | PremisesObligation],
	% Update Graph
	ObFormula = [id~ObId],
	NewFormula = id~FId,
	add_edges(GraphTmp2,[PRSId1-FId,FId-PRSId1,FId-PRSId2,PRSId2-FId,FId-TopNode,ObId-FId,FId-ObId],GraphTmp),
	NewTopNode = FId
	)),

	M is N+2,
	% Use the updated Premises for the remaining check.
	check_conditions(Id,AccafterAll,Rest2,M,NewTopNode,GraphTmp,GraphOut,ObTmp,ObOut,PremisesTmp,PremisesOut,CheckTrigger).

% Other PRSs are treated in the usual way
check_conditions(Id,Accafter,[X|Rest],N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesBegin,PremisesEnd,CheckTrigger) :-
	is_prs(X),
	!,
	check_prs(X,Accafter,GraphIn,PRSGraph,ObIn,ObTmp,PremisesBegin,XPremisesEnd,CheckTrigger),

	% Update Graph
	X = id~PRSId,
	add_edges(PRSGraph,[PRSId-TopNode],TmpGraph),

	M is N+1,

	% Use the updated Premises for the remaining check.
	check_conditions(Id,Accafter,Rest,M,PRSId,TmpGraph,GraphOut,ObTmp,ObOut,XPremisesEnd,PremisesEnd,CheckTrigger).


% ------------------------- math_id(_,_) ----------------------------------------
% X = math_id(_,_)
% Check the rest since the MathIDs are already updated in Accafter
check_conditions(Id,Accafter,[math_id(_,_)|Rest],N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesBegin,PremisesEnd,CheckTrigger) :-
	!,
	M is N+1,
	% For now, don't change the graph.. 
	check_conditions(Id,Accafter,Rest,M,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesBegin,PremisesEnd,CheckTrigger).


% --------------------------- holds(_) --------------------------------------------
% X = holds(Dref)
check_conditions(Id,Accafter,[ holds(Dref) |Rest],N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesBegin,PremisesEnd,CheckTrigger) :-
	
	% Find the Formula corresponding to Dref. 
	% If it can't be found in Accafter throw an error
	Z = math_id(Dref,FormulaFOL),
	member(Z,Accafter), 
	
	(((FormulaFOL = type~quantifier),!);
	 ((FormulaFOL = type~relation),!);
         ((FormulaFOL = type~logical_symbol),!)),!,

	simplify_formula_conjunction(FormulaFOL,Id,Dref,0,_,TopNode,GraphIn,TmpGraph,ObIn,ObTmp,PremisesBegin,NewPremisesBegin,CheckTrigger),
		
	M is N+1,
	!,
	check_conditions(Id,Accafter,Rest,M,holds(Id,Dref,N),TmpGraph,GraphOut,ObTmp,ObOut,NewPremisesBegin,PremisesEnd,CheckTrigger).


%Error case: Argument for holds is not a Formula
check_conditions(_Id,Accafter,[ holds(X) |_Rest],_N,_TopNode,_GraphIn,_GraphOut,_ObIn,_ObOut,_PremisesBegin,_PremisesEnd,_CheckTrigger) :-
	Z = math_id(X,_FormulaFOL),
	member(Z,Accafter), 
	!,
	
	term_to_atom(X,AtomX),
	add_error_message(logic,check_conditions,AtomX,'Argument for holds is not a Formula'),
	fail.


%Error case: could not find Formula corresponding to X
check_conditions(_Id,_Accafter,[ holds(X) |_Rest],_N,_Topdnode,_GraphIn,_GraphOut,_ObIn,_ObOut,_PremisesBegin,_PremisesEnd,_CheckTrigger) :-
	!,
	term_to_atom(X,AtomX),
	add_error_message(logic,check_conditions,AtomX,'Could not find corresponding Formula'),
	fail.

% --------------------------- Predicate -------------------------------------------
check_conditions(Id,Accafter,[Condition|Rest],N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesBegin,PremisesEnd,CheckTrigger) :-
	Condition =.. [predicate|PredicateArguments],!,
	predicate_aux(Accafter,PredicateArguments,[],LexPredicate),

	% Set the formula id
	LexPredicate = id~pred(Id,N),
	%LexPredicate = id~pred(Id,N)..type~relation..args~[]..name~'$false'..arity~0,
	
	% Check the formula if CheckTrigger = check
	% otherwise just append it
	create_obligation_if_trigger(PremisesBegin,[LexPredicate],Id,CheckTrigger,ObIn,ObTmp),

	NewPremisesBegin = [LexPredicate | PremisesBegin],	
	
	% Update Graph
	M is N+1,
	add_edges(GraphIn,[pred(Id,N)-TopNode],TmpGraph),
	!,
	% Check the rest of the conditions with NewPremisesBegin
 	check_conditions(Id,Accafter,Rest,M,pred(Id,N),TmpGraph,GraphOut,ObTmp,ObOut,NewPremisesBegin,PremisesEnd,CheckTrigger).


% ------------------------- Theorems ----------------------------------------
% X = theorem(_,Goal,Proof)
check_conditions(Id,Accafter,[ theorem(_,Goal,Proof) |Rest],N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesIn,PremisesOut,CheckTrigger) :-
	!,
	Proof = id~ProofId,
		
	% Create the obligations for the proof of the theorem.
	% Assumptions for Theorem must be made again!
	check_prs(Proof,Accafter,GraphIn,ProofGraph,ObIn,ObTmp1,PremisesIn,ProofPremisesEnd,CheckTrigger),
	
	!,
	% Check the actual Theorem
	check_prs(Goal,Accafter,ProofGraph,ThmGraph,ObTmp1,ObTmp2,ProofPremisesEnd,PremisesThm,CheckTrigger),
	!,

	append(Thm,ProofPremisesEnd,PremisesThm),

	% Update Graph
	add_edges(ThmGraph,[ProofId-theorem(Id,N),theorem(Id,N)-ProofId,theorem(Id,N)-TopNode],Tmp1),
	add_formula_link(theorem(Id,N),Thm,Tmp1,GraphTmp),
		
	% Set New Premises 
	append(Thm, PremisesIn, NewPremisesIn),

	M is N+1,
	!,
	check_conditions(Id,Accafter,Rest,M,theorem(Id,N),GraphTmp,GraphOut,ObTmp2,ObOut,NewPremisesIn,PremisesOut,CheckTrigger).


% ------------------------- Definition ----------------------------------------
% X = A := B
% A Definition is just one more premise. Therefore we update the premises and proceed.
check_conditions(Id,Accafter,[ (A:=B) |Rest],N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesIn,PremisesOut,CheckTrigger) :-
	!,

 	A = drefs~DrefsA..accbefore~AccbeforePRSA..accafter~AccafterA..id~IdA..conds~CondsA,
	B = drefs~DrefsB..accafter~AccafterB..id~IdB..conds~CondsB,

	% Extract the list of Premises from A and B
	check_conditions(IdA,AccafterA,CondsA,0,IdA,[],_GraphA,[],_ObOut,[],PremisesA,nocheck),
	check_conditions(IdB,AccafterB,CondsB,0,IdB,GraphIn,GraphB,ObIn,ObB,PremisesIn,PremisesInAndB,nocheck),
	sort_premises_for_the(Id,AccbeforePRSA,PremisesIn,PremisesInAndB,GraphB,GraphTmp,PremisesInAndThe,PremisesB,CheckTrigger),
	!,
	
	% Error case:  check_conditions fails if A or B are empty (i.e. the conditions of A or B contain no 'holds' and therefore ListA or ListB are empty)
	error_if_empty(PremisesA,IdA,'Definiendum PRS is empty'),
	error_if_empty(PremisesB,IdB,'Definiens PRS is empty'),
	
	get_free_var(AccafterA,DrefsA,FreeVarA),
	get_free_var(AccafterB,DrefsB,FreeVarB),

	% Update the premise
	% Need to use IdB since RRefs link the the right-hand-side PRS of a definition
	update_definitions(PremisesInAndThe,cond(Id,N),FreeVarA,FreeVarB,PremisesA,PremisesB,FormulaId,NewPremisesBegin),

	% Update Graph
	M is N+1,
	% Link IdB to cond(Id,N) since RRefs refer to B.
	add_edges(GraphTmp,[FormulaId-TopNode,IdB-FormulaId,FormulaId-IdB],GraphTmp2),

 	!,
 	check_conditions(Id,Accafter,Rest,M,FormulaId,GraphTmp2,GraphOut,ObB,ObOut,NewPremisesBegin,PremisesOut,CheckTrigger).
	

% --------------------------- Equivalence --------------------------------------------
check_conditions(Id,Accafter,[ (A<=>B) | Rest],N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesIn,PremisesOut,CheckTrigger) :-
	!,
	% Result should be Forall [Drefs in A ind B]: exists [Drefs only A] A <=> exists [Drefs only B] B
	A = id~IdA..accbefore~AccbeforePRSA,
	B = id~IdB..accbefore~AccbeforePRSB,

	% Get premises from A
	% Nocheck since A has to follow from B (which is verified below)
	check_prs(A,Accafter,GraphIn,GraphA,ObIn,ObA,PremisesIn,PremisesInAndA,nocheck),
	sort_premises_for_the(Id,AccbeforePRSA,PremisesIn,PremisesInAndA,GraphA,GraphTmp,PremisesInAndThe,PremisesA,CheckTrigger),
		
	% Get premises from B
	% Nocheck since B has to follow from A (which is verified below)
	check_prs(B,Accafter,GraphTmp,GraphB,ObA,ObB,PremisesInAndA,PremisesInAndB,nocheck),
	append(BTmp,PremisesInAndA,PremisesInAndB),
	sort_premises_for_the(Id,AccbeforePRSB,[],BTmp,GraphB,GraphTmp2,TheB,PremisesB,CheckTrigger),
	append(TheB,PremisesInAndThe,POutTmp),
	
	% Error case: Equivalence fails if A is empty 
	error_if_empty(PremisesA,IdA,'Equivalence PRS A is empty'),
	error_if_empty(PremisesB,IdB,'Equivalence PRS B is empty'),
	
	% NewFormula = (Conjuncted PremisesA <=> Conjuncted PremisesB)
	% We don't need to quantify because check_prs quantifies A and B existentially, and all potential universal quantification is dealt with at PRS level.
	make_equivalence_formula(cond(Id,N),PremisesA,PremisesB,NewFormula),	
	create_obligation_if_trigger(POutTmp,NewFormula,IdA,CheckTrigger,ObB,ObTmp),
	!,
	
	NewFormula =[NP],
	PremisesTmp = [NP | POutTmp],

	% Update Graph
	NewFormula = [id~FormulaId],
	M is N+1,
	add_edges(GraphTmp2,[FormulaId-TopNode,IdA-FormulaId,FormulaId-IdA,IdB-FormulaId,FormulaId-IdB],GraphTmp3),

 	!,
 	check_conditions(Id,Accafter,Rest,M,FormulaId,GraphTmp3,GraphOut,ObTmp,ObOut,PremisesTmp,PremisesOut,CheckTrigger).


% ----------------------------- Reverse Implication ----------------------------------------
check_conditions(Id,Accafter,[ (B<=A) | Rest ],N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesIn,PremisesOut,CheckTrigger) :-
	% The names of A and B are swapped so that they match the names in premises.pl
	!,
	A = id~IdA..accbefore~AccbeforePRSA,
	B = id~IdB..accbefore~AccbeforePRSB,
	
	% Get premises from A
	% Nocheck since A has to follow from B (which is verified below)
	check_prs(A,Accafter,GraphIn,GraphA,ObIn,ObA,PremisesIn,PremisesInAndA,nocheck),
	sort_premises_for_the(Id,AccbeforePRSA,PremisesIn,PremisesInAndA,GraphA,GraphTmp,PremisesInAndThe,PremisesA,CheckTrigger),
		
	% Get premises from B
	% Nocheck since B has to follow from A (which is verified below)
	check_prs(B,Accafter,GraphTmp,GraphB,ObA,ObB,PremisesInAndA,PremisesInAndB,CheckTrigger),
	append(BTmp,PremisesInAndA,PremisesInAndB),
	sort_premises_for_the(Id,AccbeforePRSB,[],BTmp,GraphB,GraphTmp2,TheB,PremisesB,CheckTrigger),
	append(TheB,PremisesInAndThe,POutTmp),

	error_if_empty(PremisesB,IdB,'Succedent PRS is empty'),
	error_if_empty(PremisesA,IdA,'Antecedent PRS is empty'),

	% NewFormula is A => B
	make_implication_formula(Id,PremisesA,PremisesB,NewFormula),

	% Append EndFormula
	NewFormula = [NP],
	PremisesTmp = [NP | POutTmp],
	
	% Update Graph
	NP = id~EndId,
	add_edges(GraphTmp2,[IdA-EndId,EndId-IdA,IdB-EndId,EndId-IdB],UpdateGraph),
	add_edges(UpdateGraph,[EndId-TopNode],TmpGraph),
	
	% Check the rest of the conditions with NewPremisesBegin
	M is N+1,
 	check_conditions(Id,Accafter,Rest,M,EndId,TmpGraph,GraphOut,ObB,ObOut,PremisesTmp,PremisesOut,CheckTrigger).


%---------------------------- Disjunction --------------------------------------------
% X = A v B
% A and B can both be either a PRS or a negated PRS.
% Finds the formulas in PrsA and in PrsB and disjuncts them.
% Then proceeds to check the new premises.
check_conditions(Id,Accafter,[(A v B)|Rest],N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesIn,PremisesOut,CheckTrigger) :-
	!,
	check_prs(A,Accafter, GraphIn, GraphA, ObIn, ObA ,PremisesIn, PremisesInAndA, nocheck),
	check_prs(B,Accafter, GraphA, GraphB, ObA, ObB, PremisesInAndA, PremisesInAndB, nocheck),
	
	A = id~IdA..accbefore~AccbeforePRSA,
	B = id~IdB..accbefore~AccbeforePRSB,

	sort_premises_for_the(Id,AccbeforePRSA,PremisesIn,PremisesInAndA,GraphB,GraphTmp,APremisesIn,PremisesA,CheckTrigger),
	append(BTmp,PremisesInAndA,PremisesInAndB),
	sort_premises_for_the(Id,AccbeforePRSB,[],BTmp,GraphTmp,GraphTmp2,BPremisesIn,PremisesB,CheckTrigger),
	append(BPremisesIn,APremisesIn,NewPremisesTmp),
	
	update_disjunction(Id,PremisesA,PremisesB,NewFormula),

	% Check the Premises if CheckTrigger = check
	% otherwise just append them
	create_obligation_if_trigger(NewPremisesTmp,NewFormula,Id,CheckTrigger,ObB,ObTmp),
	NewFormula = [NP],
	NewPremisesIn = [ NP | NewPremisesTmp ],

	% Update Graph
	M is N+1,
	NP = id~FormulaId,
	add_edges(GraphTmp2,[IdA-FormulaId,IdB-FormulaId,FormulaId-IdA,FormulaId-IdB,FormulaId-TopNode],GraphTmp3),

 	!,
 	check_conditions(Id,Accafter,Rest,M,FormulaId,GraphTmp3,GraphOut,ObTmp,ObOut,NewPremisesIn,PremisesOut,CheckTrigger).


% ------------------------- Assumption ----------------------------------------
% X = A ==> B
% A and B both can be either a PRS or a negated PRS.
% This results in a premises update:
% For all premises X from B we add
% ! [Variables in MrefsA] : Fol_A -> X
% to the list of premises available

check_conditions(Id,Accafter,[ (A==>B) |Rest],N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesIn,PremisesOut,CheckTrigger) :-
	!,
	% Get PremisesA
	A = accafter~AccafterA..accbefore~AccbeforePRSA..id~IdA..conds~CondsA,
	B = id~IdB..conds~CondsB,

	% Search for all Drefs
	get_all_new_drefs(A,DrefsA),
	get_free_var(AccafterA,DrefsA,FreeVarA),

	check_conditions(IdA,AccafterA,CondsA,0,IdA,GraphIn,GraphA,ObIn,ObA,PremisesIn,PremisesInAndA,nocheck),
	sort_premises_for_the(Id,AccbeforePRSA,PremisesIn,PremisesInAndA,GraphA,GraphTmp,PremisesStart,PremisesA,CheckTrigger),

	% If the assumptions ends with a contradiction, we mark the PremisesA with contradiction~yes.
	% An Obligation with contra~yes does not give a warning if the axioms are contradictory.
	( (\+ IdA=axiom(_)), search_for_contradiction(CondsB)->
		mark_contradiction(PremisesA); true
	),

	(CheckTrigger = nocheck ->
		assumption_cond_nocheck(cond(Id,N),Accafter,B,FreeVarA,PremisesA,GraphTmp,GraphTmp2,ObA,ObTmp,PremisesStart,NewPremisesIn)
		;
		assumption_cond_check(cond(Id,N),Accafter,IdA,B,FreeVarA,PremisesA,AccafterA,GraphTmp,GraphTmp2,ObA,ObTmp,PremisesStart,NewPremisesIn)
	),

	% Update Graph
	add_edges(GraphTmp2,[cond(Id,N)-TopNode,IdB-cond(Id,N),IdA-cond(Id,N),cond(Id,N)-IdB,cond(Id,N)-IdA],GraphTmp3),
	M is N+1,
	!,
 	check_conditions(Id,Accafter,Rest,M,cond(Id,N),GraphTmp3,GraphOut,ObTmp,ObOut,NewPremisesIn,PremisesOut,CheckTrigger).


% ------------------------- Implication & for all/exists -----------------------------------
% X = A => B
% A and B both can be either a PRS or a negated PRS.
% Find the Formulas in PrsA and adds them as Premises. Then proceeds to check PrsB with the updated premises

check_conditions(Id,Accafter,[ (A=>B) | Rest ],N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesBegin,PremisesEnd,CheckTrigger) :-
	check_conditions(Id,Accafter,[ (A==>B) | Rest ],N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesBegin,PremisesEnd,CheckTrigger).


% ------------------------- Negation ----------------------------------------
% X = neg(PRS)
% Takes all premises from PRS and negates them.
% Check occurs after the negation.
check_conditions(Id,Accafter,[neg(PRS) | Rest ],N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesBegin,PremisesEnd,CheckTrigger) :-
	!,
	
	check_neg_prs(neg(PRS),Accafter,GraphIn,UpdateGraph,ObIn,ObTmp,PremisesBegin,NewPremisesBegin,CheckTrigger),

	% Update Graph
	M is N+1,
	PRS = id~PRSId,
	add_edges(UpdateGraph,[PRSId-TopNode],TmpGraph),

 	!,
 	check_conditions(Id,Accafter,Rest,M,PRSId,TmpGraph,GraphOut,ObTmp,ObOut,NewPremisesBegin,PremisesEnd,CheckTrigger).


% -------------------------- Contradiction ------------------------------------------
check_conditions(Id,Accafter,[contradiction|Rest],N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesBegin,PremisesEnd,CheckTrigger) :-
	!,
	
	% Check whether we can conclude $false from the premises so far.
	create_obligation_if_trigger(PremisesBegin,[type~relation..arity~0..name~'$false'..id~cont(Id,N)],Id,CheckTrigger,ObIn,ObTmp),

	% Add [contradiction] to the premises
	NewPremisesBegin = [ type~relation..arity~0..name~'$false'..id~cont(Id,N) | PremisesBegin ],

	% Update Graph
	M is N+1,
	add_edges(GraphIn,[cont(Id,N)-TopNode],TmpGraph),

	% Check the rest of the conditions with NewPremisesBegin
 	check_conditions(Id,Accafter,Rest,M,cont(Id,N),TmpGraph,GraphOut,ObTmp,ObOut,NewPremisesBegin,PremisesEnd,CheckTrigger).


% ------------------------- Function ----------------------------------------------
check_conditions(Id,Accafter,[(_::A=>B)|Rest],N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesIn,PremisesOut,CheckTrigger) :-
	!,
	% Get PremisesA
	A = accafter~AccafterA..accbefore~AccbeforePRSA..id~IdA..conds~CondsA,
	B = id~IdB..conds~CondsB,

	% Search for all Drefs
	get_all_new_drefs(A,DrefsA),
	get_free_var(AccafterA,DrefsA,FreeVarA),

	check_conditions(IdA,AccafterA,CondsA,0,IdA,GraphIn,GraphA,ObIn,ObA,PremisesIn,PremisesInAndA,nocheck),
	sort_premises_for_the(Id,AccbeforePRSA,PremisesIn,PremisesInAndA,GraphA,GraphTmp,PremisesStart,PremisesA,CheckTrigger),

	% If the assumptions ends with a contradiction, we mark the PremisesA with contradiction~yes.
	% An Obligation with contra~yes does not give a warning if the axioms are contradictory.
	( (\+ IdA=axiom(_)), search_for_contradiction(CondsB)->
		mark_contradiction(PremisesA); true
	),

	assumption_cond_nocheck(cond(Id,N),Accafter,B,FreeVarA,PremisesA,GraphTmp,GraphTmp2,ObA,ObTmp,PremisesStart,NewPremisesIn),

	% Update Graph
	add_edges(GraphTmp2,[cond(Id,N)-TopNode,IdB-cond(Id,N),IdA-cond(Id,N),cond(Id,N)-IdB,cond(Id,N)-IdA],GraphTmp3),
	M is N+1,
	!,
 	check_conditions(Id,Accafter,Rest,M,cond(Id,N),GraphTmp3,GraphOut,ObTmp,ObOut,NewPremisesIn,PremisesOut,CheckTrigger).

/*
check_conditions(Id,Accafter,[(_::_A=>B)|Rest],N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesIn,PremisesOut,CheckTrigger) :-
	!,
	B = id~IdB..drefs~DrefsB..accbefore~AccbeforePRSB..accafter~AccafterB..conds~CondsB,
	get_free_var(AccafterB,DrefsB,VariablesB),
	
	% Get formula image of B and conjunct
	check_conditions(IdB,AccafterB,CondsB,0,cond(Id,N),GraphIn,GraphB,[],_ObOut,PremisesIn,PremisesInAndB,nocheck),
	sort_premises_for_the(IdB,AccbeforePRSB,PremisesIn,PremisesInAndB,GraphB,GraphTmp,PremisesInAndThe,PremisesB,nocheck),
	
	( PremisesB = [] ->
		(
		add_error_message(logic,check_conditions,Id,'Empty Function Definition'),	
		fail
		);
		(
		make_conjunction_list(PremisesB,FunctionDef),

		% Quantify universally over the variables in MrefsB and append the new formula
		% to PremisesBegin
		quantify_universally(cond(Id,N),VariablesB,FunctionDef,NewFormula),
		NewFormula = [NP],
		NewPremisesBegin = [ NP | PremisesInAndThe],

		% Update Graph
		M is N+1,
		NewFormula = [id~FormulaId],
		add_edges(GraphTmp,[cond(Id,N)-TopNode,FormulaId-cond(Id,N),cond(Id,N)-FormulaId],TmpGraph),

		% Check the rest of the conditions with NewPremisesBegin
	 	check_conditions(Id,Accafter,Rest,M,cond(Id,N),TmpGraph,GraphOut,ObIn,ObOut,NewPremisesBegin,PremisesOut,CheckTrigger)
		)
	).
*/

%---------------------------- Exclusive Disjunction ----------------------------------
% Exactly one of A,B,.. holds
% X = ><([A,B,..])
check_conditions(Id,Accafter,[><([A|Rest])|OtherConds],N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesIn,PremisesOut,CheckTrigger) :-
	!,
	% Create edge to TopNode
	add_edges(GraphIn,[cond(Id,N)-TopNode],TopGraph),	

	check_exclusive_formulas(cond(Id,N),Accafter,[A|Rest],TopGraph,FormGraph,ObIn,ObTmp,PremisesIn,ExPremises,CheckTrigger),
	
	make_disjunction(Id,Accafter,[A|Rest],[],DisPremises,CheckTrigger),
	create_obligation_if_trigger(PremisesIn,DisPremises,Id,CheckTrigger,ObTmp,ObTmp2),

	% Add Formula Edge
	DisPremises = [id~FormulaId],
	add_edges(FormGraph,[cond(Id,N)-FormulaId,FormulaId-cond(Id,N)],TmpGraph),

	DisPremises = [DisFormula],
	NewPremisesBegin = [DisFormula|ExPremises],

	% Update Condition Counter
	M is N+1,

	!,
	check_conditions(Id,Accafter,OtherConds,M,cond(Id,N),TmpGraph,GraphOut,ObTmp2,ObOut,NewPremisesBegin,PremisesOut,CheckTrigger).


%---------------------------- Inclusive Disjunction  ----------------------------------
% At most of of A,B,.. holds
% X = <>([A,B,..])
check_conditions(Id,Accafter,[<>(PRSs)|OtherConds],N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesBegin,PremisesEnd,CheckTrigger) :-
	!,
	% Create edge to TopNode
	add_edges(GraphIn,[cond(Id,N)-TopNode],TopGraph),	
	
	check_exclusive_formulas(cond(Id,N),Accafter,PRSs,TopGraph,TmpGraph,ObIn,ObTmp,PremisesBegin,ExPremises,CheckTrigger),
	
	% Update Condition Counter
	M is N+1,
	
	!,
	check_conditions(Id,Accafter,OtherConds,M,cond(Id,N),TmpGraph,GraphOut,ObTmp,ObOut,ExPremises,PremisesEnd,CheckTrigger).


% ------------------------------- plural_dref condition --------------------------------------
check_conditions(Id,Accafter,[plural_dref(_Dref,_PluralList)|OtherConds],N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesIn,PremisesOut,CheckTrigger) :-
	% Update Condition Counter
	M is N+1,
	!,
	check_conditions(Id,Accafter,OtherConds,M,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesIn,PremisesOut,CheckTrigger).

% ------------------------------- static condition --------------------------------------
% Always quantify existentially over all new Drefs
check_conditions(Id,Accafter,[static(PRS)|Conds],N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesIn,PremisesOut,CheckTrigger) :-
	is_prs(PRS),
	!,
	PRS = id~IdPRS..conds~CondsPRS..accbefore~AccbeforePRS..accafter~AccafterPRS,
	get_all_new_drefs(PRS,Drefs),
	get_free_var(AccafterPRS,Drefs,VariablesPRS),
	
	check_conditions(IdPRS,AccafterPRS,CondsPRS,0,IdPRS,GraphIn,GraphPRS,ObIn,ObPRS,PremisesIn,PremisesTmp,nocheck),
	
	sort_premises_for_the(IdPRS,AccbeforePRS,PremisesIn,PremisesTmp,GraphPRS,GraphTmp,PremisesInAndThe,PremisesPRS,CheckTrigger),

	% Conjunct the premises and quantify existentially over variables in Mrefs
	quantify_existentially(IdPRS,VariablesPRS,PremisesPRS,ObFormula),	
	create_obligation_if_trigger(PremisesInAndThe,ObFormula,IdPRS,CheckTrigger,ObPRS,ObTmp),
	
	( PremisesPRS = [] -> 
	(
	GraphTmp3 = GraphTmp,
	PremisesTmp2 = PremisesInAndThe
	);
	( 
	% Update Graph
	% Dont add edges to the Formulas from which QNewFormula was derived since they were created with nocheck anyways.
	ObFormula = [id~ObFormulaId],
	add_formula_link(ObFormulaId,PremisesPRS,GraphTmp,GraphTmp2),
	add_edges(GraphTmp2,[ObFormulaId-IdPRS,IdPRS-ObFormulaId,IdPRS-TopNode],GraphTmp3),
	ObFormula = [QFormula],
	PremisesTmp2 = [QFormula | PremisesInAndThe]
	)),
	M is N+1,
	% Use the updated Premises for the remaining check.
	!,
	check_conditions(Id,Accafter,Conds,M,IdPRS,GraphTmp3,GraphOut,ObTmp,ObOut,PremisesTmp2,PremisesOut,CheckTrigger).

% ------------------------------- the conditions --------------------------------------
% Either proves the existence of a set;or existence and uniqueness of something else.
check_conditions(Id,Accafter,[the(Dref,PRS)|OtherConds],N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesBegin,PremisesEnd,CheckTrigger) :-
	% case 1: Proving the existence of a set
	% The seperation scheme is used implicitly. The existence of a superset becomes an obligation.
	PRS = id~TheId..conds~[predicate(Dref,set),A<=>B],
	B = conds~[predicate(DrefA,Dref,in)],
	!,

	% A contains the predicate that defines the set.
	A = id~IdA..accafter~AccafterA,
	check_prs(A,Accafter,[],_GraphA,[],_ObA,[],PremisesAQuant,nocheck),
	get_all_new_drefs(A,DrefsA),
	get_free_var(AccafterA,DrefsA,VariablesA),
	subtract(VariablesA,[type~variable..dref~DrefA],VarWithoutDrefA),
	% We want the unquantified version of PremisesA
	PremisesAQuant = [args~[_,PremisesA]],

	% Create the formula: exists z s.t. set(z) and forall x PRSA(x) => x in z
	quantify_existentially(IdA,VarWithoutDrefA,[PremisesA],FormulaA),
	make_implication_formula(TheId,FormulaA,[type~relation..name~in..id~theid..arity~2..args~[type~variable..dref~DrefA,type~variable..dref~the..name~the]],ImpFormula),
	quantify_universally(theu(TheId,N),[type~variable..dref~DrefA],ImpFormula,QFormula),
	quantify_existentially(thee(TheId,N),[type~variable..dref~the],[type~relation..name~set..id~set(theid)..arity~1..args~[type~variable..dref~the..name~the]|QFormula],EFormula),

	
	% This obligation must always be checked, hence CheckTrigger=check
	create_obligation_if_trigger(PremisesBegin,EFormula,TheId,check,ObIn,ObTmp),

	% Update Condition Counter
	M is N+1,

	% Create New Premises	
	make_equivalence_formula(TheId,FormulaA,
		[type~relation..name~in..arity~1..id~imp(the)..args~[type~variable..dref~DrefA,type~variable..dref~Dref..name~thevar..the~yes]],EQFormula),
	quantify_universally(theu(TheId,N),[type~variable..dref~DrefA],EQFormula,FinalFormulaList),
	
	% Update Graph
	FinalFormulaList = [FinalFormula],
	EFormula = [Formula],
	FinalFormula = id~FFId..the~yes,
	Formula = id~FId..the~yes,
	NewPremises = [FinalFormula,Formula,type~relation..name~set..id~TheId..arity~1..args~[type~variable..dref~Dref..the~yes..id~set(TheId)]|PremisesBegin],
	add_edges(GraphIn,[TheId-TopNode,FId-TheId,TheId-FId,FFId-TheId,TheId-FFId,set(TheId)-TheId,TheId-set(TheId)],GraphTmp),	

	!,
	check_conditions(Id,Accafter,OtherConds,M,TheId,GraphTmp,GraphOut,ObTmp,ObOut,NewPremises,PremisesEnd,CheckTrigger).
	

check_conditions(Id,Accafter,[the(Dref,PRS)|OtherConds],N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesBegin,PremisesEnd,CheckTrigger) :-
	% case 2: Existence and uniqueness
	!,
	check_prs(PRS,Accafter,GraphIn,GraphPRS,[],_ObA,[],PremisesPRS,nocheck),
	PRS = id~PRSId..accafter~AccafterPRS,
	get_all_new_drefs(PRS,DrefsPRS),
	get_free_var(AccafterPRS,DrefsPRS,VariablesPRS),

	quantify_existentially(PRSId,VariablesPRS,PremisesPRS,[ConFormula]),
	replace_in_formula(dref~Dref,ConFormula,type~variable..name~thevar..dref~Dref..the~yes,SaveFormula),
	replace_in_formula(dref~Dref,ConFormula,type~variable..name~thex..dref~thex,XFormula),
	% We need to skolemize SaveFormula later, therefore it gets a the~yes marker
	SaveFormula = the~yes,

	% We need to check two things. 1. Existence 
	TmpVariablesPRS = [type~variable..dref~Dref],
	quantify_existentially(thee(PRSId,N),TmpVariablesPRS,[ConFormula],[EFormula]),
	% 2. Uniqueness: forall x PRS(x) => Dref=x
	make_implication_formula(PRSId,[XFormula],
		[type~relation..name~'='..arity~2..id~the(Id)..args~[type~variable..name~thex..dref~thex,type~variable..dref~Dref..name~thevar..the~yes]],ImpFormula),
	quantify_universally(theu(PRSId,N),[type~variable..name~thex..dref~thex],ImpFormula,[QFormula]),

	% These obligation must always be checked, hence CheckTrigger=check
	create_obligation_if_trigger(PremisesBegin,[EFormula],PRSId,check,ObIn,ObTmp1),
	create_obligation_if_trigger([ConFormula|PremisesBegin],[QFormula],PRSId,check,ObTmp1,ObTmp),

	% Update Condition Counter
	M is N+1,

	% Create New Premises	
	% We 'skolemize' i.e. don't quantify existentially over the Dref in the 'the' condition
	NewPremises = [QFormula,SaveFormula|PremisesBegin],

	% Update Graph
	EFormula = id~ExId..the~yes,
	QFormula = id~QuId..the~yes,
	ConFormula = id~ConId..the~yes,
	add_edges(GraphPRS,[PRSId-TopNode,ExId-ConId,ConId-ExId,ConId-PRSId,PRSId-ConId,QuId-PRSId,PRSId-QuId],TmpGraph),	

	!,
	check_conditions(Id,Accafter,OtherConds,M,PRSId,TmpGraph,GraphOut,ObTmp,ObOut,NewPremises,PremisesEnd,CheckTrigger).

% ------------------------- Everything else ----------------------------------------
% Error Case
check_conditions(_Id,_Accafter,[X|_Rest],_N,_TopNode,_GraphIn,_GraphOut,_ObIn,_ObOut,_PremisesBegin,_PremisesEnd,_CheckTrigger) :-
        term_to_atom(X,AtomX),
	add_error_message(logic,check_conditions,AtomX,'Unknown condition'),	
	fail.



%%	predicate_aux(+Acc,+List,+Args,-Formula)
%
%	Auxiliary predicate for the predicate PRS condition
% 	Creates a first order formula from the predicate list

predicate_aux(_Acc,[Predicate],Arguments,LexPredicate) :-
	% Get the DOBSOD representation of Predicate
	math_lexicon(Predicate,LexPredicate),!,
	reverse(Arguments,RevArgs),	
	% the new formula is LexPredicate with argument Variable
	LexPredicate = args~RevArgs.

predicate_aux(Acc,[Dref|Rest],ArgSoFar,LexPredicate) :-
	Z = math_id(Dref,Var),
	( (member(Z,Acc),!,(Var = type~constant;Var=type~function)) ->
		Args = [Var|ArgSoFar];
		Args = [type~variable..dref~Dref|ArgSoFar]
	), 
	!,
	predicate_aux(Acc,Rest,Args,LexPredicate).

	

%%	check_exclusive_formulas(+Id,+Accafter,+PRss:list(PRS),+GraphIn,-GraphOut,+ObIn,-ObOut,+PremisesIn:list(DOBSOD),-NewPremisesOut:list(DOBSOD),+CheckTrigger)
%
%	check_exclusive_formulas checks whether exclusive cases really exclude one another.

check_exclusive_formulas(Id,Accafter,PRSs,GraphIn,GraphOut,ObIn,ObOut,PremisesIn,PremisesOut,CheckTrigger) :-
	get_conjuncted_list(Id,Accafter,PRSs,GraphIn,GraphTmp,ObIn,ObTmp,PremisesIn,PremisesTmp,NegConjFormulas,CheckTrigger),
	make_and_check_disjunctions(NegConjFormulas,Id,0,_,GraphTmp,GraphOut,ObTmp,ObOut,PremisesTmp,PremisesOut,CheckTrigger).

%%	get_conjuncted_list(+Id,+Accafter,+PRSs:list(PRS),+GraphIn,-GraphOut,+ObIn,-ObOut,+PremisesIn,-PremisesOut,-ConjFormulas,+CheckTrigger)
%
%	PremisesOut is PremisesIn + the negated conjuncted formulas image of each PRS in PRS

get_conjuncted_list(Id,Accafter,[PRS|PRSs],GraphIn,GraphOut,ObIn,ObOut,PremisesIn,PremisesOut,[NotQPremisesPRS|ConjFormulasTmp],CheckTrigger) :-
	check_prs(PRS,Accafter,GraphIn,GraphTmp,ObIn,ObTmp,PremisesIn,PremisesTmp,nocheck),
	PRS = id~PRSId..accbefore~Accbefore,
	% Take care of "the" formulas
	sort_premises_for_the(Id,Accbefore,PremisesIn,PremisesTmp,GraphTmp,GraphTmp2,PremisesInAndThe,PremisesPRS,CheckTrigger),
	make_conjunction_list(PremisesPRS,PremisesPRSCon),
	negate_formulas(PRSId,PremisesPRSCon,NotQPremisesPRS),
	%Update Graph
	add_edges(GraphTmp2,[PRSId-Id,Id-PRSId],GraphTmp3),
	!,
	get_conjuncted_list(Id,Accafter,PRSs,GraphTmp3,GraphOut,ObTmp,ObOut,PremisesInAndThe,PremisesOut,ConjFormulasTmp,CheckTrigger).
get_conjuncted_list(_Id,_Accafter,[],GraphIn,GraphIn,ObIn,ObIn,PremisesIn,PremisesIn,[],_CheckTrigger).


%%	make_and_check_disjunctions(+Formulas:list,+Id,+NIn,-NOut,+GraphIn,-GraphOut,+ObIn,-ObOut,+PremisesIn,-PremisesOut,+CheckTrigger)
%
%	Takes each pair of elemnets of Formulas disjuncts them, creates an obligation and saved the disjunction in PremisesOut

make_and_check_disjunctions([Formula1,Formula2|Formulas],Id,NIn,NOut,GraphIn,GraphOut,ObIn,ObOut,PremisesIn,PremisesOut,CheckTrigger) :-
	update_disjunction(ex(Id,NIn),Formula1,Formula2,DisjunctionList),
	% Check the Premises if CheckTrigger = check
	% otherwise just append them
	create_obligation_if_trigger(PremisesIn,DisjunctionList,Id,CheckTrigger,ObIn,ObTmp),
	DisjunctionList = [NP],
	PremisesTmp = [ NP | PremisesIn],	
	% Update Graph
	DisjunctionList = [id~FormulaId],
	add_edges(GraphIn,[Id-FormulaId,FormulaId-Id],GraphTmp),
	M is NIn + 1,
	!,
	% Recursion
	make_and_check_disjunctions([Formula1|Formulas],Id,M,MOut,GraphTmp,GraphTmp2,ObTmp,ObTmp2,PremisesTmp,PremisesTmp2,CheckTrigger),
	make_and_check_disjunctions([Formula2|Formulas],Id,MOut,NOut,GraphTmp2,GraphOut,ObTmp2,ObOut,PremisesTmp2,PremisesOut,CheckTrigger).
make_and_check_disjunctions([_Formula],_Id,NIn,NIn,GraphIn,GraphIn,ObIn,ObIn,PremisesIn,PremisesIn,_CheckTrigger). 
make_and_check_disjunctions([],_Id,NIn,NIn,GraphIn,GraphIn,ObIn,ObIn,PremisesIn,PremisesIn,_CheckTrigger). 
	

%%	make_disjunction(+Id,+List:list(PRS),+OldDisjunction:list(DOBSOD),-NewDisjunction:list(DOBSOD),+CheckTrigger)
%
%	make_disjunction takes a list of case-PRSes, extracts the set of premises they contain and disjuncts all the sets.

make_disjunction(_Id,_Accafter,[],Dis,Dis,_) :- !.

make_disjunction(Id,Accafter,[A|Rest],OldDisjunction,NewDisjunction,CheckTrigger):-
	% Get Premises from A
	check_prs(A,Accafter,[],_,[],_Ob,[],PremisesTmp,nocheck),
	!,
	A = accbefore~AccbeforePRSA,
	sort_premises_for_the(Id,AccbeforePRSA,[],PremisesTmp,[],_Graph,_APremisesIn,PremisesA,CheckTrigger),
	make_conjunction_list(PremisesA,PremisesACon),
	
	% Add them to the current disjunction
	update_disjunction(Id,OldDisjunction,PremisesACon,TempDisjunction),
	!,
	make_disjunction(updis(Id),Accafter,Rest,TempDisjunction,NewDisjunction,CheckTrigger).


%%	error_if_empty(+List:list(DOBSOD),+Id:atom,+ErrorMessage:string)
%
% 	Throws error message ErrorMessage if List is empty

error_if_empty([], Id, ErrorMessage):-
	!,
	term_to_atom(Id,AtomId),
	add_error_message(logic,check_conditions,AtomId,ErrorMessage),
	fail.

error_if_empty(_, _, _).


%% 	create_obligation_if_trigger(+Premises:list(DOBSOD),+ToBeChecked:list(DOBSOD),+Id:atom,
%%	+CheckTrigger:(check | nocheck),+ObIn,-ObOut) is det.
%
%	Creates proof obligations for ToBeChecked with Premises if CheckTrigger = check

create_obligation_if_trigger(_,_,_,nocheck,ObIn,ObIn) :- !.

create_obligation_if_trigger(Premises,ToBeChecked,Id,check,ObIn,[[Id,ToBeChecked,Premises]|ObIn]) :- !.


%% 	simplify_formula_conjunction(+Formula,+Id,+Dref,+N,-M,+TopNode,+GraphIn,-GraphOut,+ObIn,-ObOut,+PremisesBegin,-NewPremisesBegin,+CheckTrigger)
% 
%	create a proof obligation for Formula, this includes giving the formula an id and updating the graph.
%	If formula is a conjunction, split it and call simplify_formula_conjunction on each side of the conjunction.

simplify_formula_conjunction(name~'&'..args~[F1,F2],Id,Dref,N,M,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesBegin,NewPremisesBegin,CheckTrigger) :-
	!,
	simplify_formula_conjunction(F1,Id,Dref,N,M,TopNode,GraphIn,GraphTmp,ObIn,ObTmp,PremisesBegin,NewPremisesBeginTmp,CheckTrigger),
	!,
	simplify_formula_conjunction(F2,Id,Dref,M,_,TopNode,GraphTmp,GraphOut,ObTmp,ObOut,NewPremisesBeginTmp,NewPremisesBegin,CheckTrigger).

simplify_formula_conjunction(Formula,Id,Dref,N,M,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesBegin,NewPremisesBegin,CheckTrigger) :-
	!,
	% Set Formula Id
	% Copy term since the same formula, with the same dref might have been used in another prs, and therefore already have an ID.
	copy_term(Formula,CopyF),
	CopyF = id~holds(Id,Dref,N),

	create_obligation_if_trigger(PremisesBegin,[CopyF],Id,CheckTrigger,ObIn,ObOut),

	% Update premises
	NewPremisesBegin = [CopyF | PremisesBegin],

	% Update Graph,
	add_edges(GraphIn,[holds(Id,Dref,N)-TopNode],GraphOut),
	
	M is N+1,
	!.


%%	get_free_var(+Accafter,+Drefs,?FreeVar:list)
%
%	Takes the drefs and accafter of a PRS and returns the list of all variables in drefs

get_free_var(_Acc,[],[]) :- !.

get_free_var(Acc,[X|Rest],NewVar) :-
	member(math_id(X,Y),Acc),
	!,
	( Y = type~variable ->
	  NewVar = [Y|Var];
	  NewVar = Var
	),
	get_free_var(Acc,Rest,Var).
get_free_var(Acc,[Dref|Rest],NewVar) :-
	member(Dref,Acc),
	!,
	X = type~variable..dref~Dref,
	NewVar = [X|Var],
	get_free_var(Acc,Rest,Var).

get_free_var(Acc,[_Dref|Rest],Var) :-
	!,
	get_free_var(Acc,Rest,Var).

%%	get_all_new_drefs(PRS,DrefsPRS:list)
%
%	DrefsPRS is a list of all the Drefs that are introduced in the PRS or subPRSs or the PRS.
%	Note that only bare PRSs are consider subPRSs here.

get_all_new_drefs(PRS,DrefsPRS) :-
	is_prs(PRS),
	PRS = drefs~Drefs..conds~Conds, 
	get_all_new_drefs_conds(Conds,DrefsConds),
	append(Drefs,DrefsConds,DrefsPRS).

get_all_new_drefs_conds([],[]).

get_all_new_drefs_conds([PRS|Rest],Drefs) :-
	is_prs(PRS),
	!,
	get_all_new_drefs(PRS,DrefsPRS),
	get_all_new_drefs_conds(Rest,DrefsRest),
	append(DrefsPRS,DrefsRest,Drefs).

get_all_new_drefs_conds([_|Rest],Drefs) :-
	get_all_new_drefs_conds(Rest,Drefs).


%% 	assumption_cond(+Id,+IdA,+IdB,VariablesA,PremisesA,N,TopNode,GraphIn,GraphOut,ObIn,ObOut,PremisesBegin,PremisesEnd,CheckTrigger)
%
%	Predicate for the specific cases of assumptions

%nocheck case
assumption_cond_nocheck(Id,Accafter,B,VariablesA,PremisesA,GraphIn,GraphOut,ObIn,ObOut,PremisesIn,PremisesOut) :-
	!,
	append(PremisesA,PremisesIn,PremisesStart),
	check_prs(B,Accafter,GraphIn,GraphB,ObIn,ObOut,PremisesStart,PremisesInAndB,nocheck),
	append(PremisesBUnSort,PremisesStart,PremisesInAndB),
	B = accbefore~AccbeforePRSB..id~IdB,
	sort_premises_for_the(IdB,AccbeforePRSB,[],PremisesBUnSort,GraphB,GraphTmp,PremisesThe,PremisesB,nocheck),
	append(PremisesThe,PremisesIn,PremisesInAndThe),

	( PremisesB = [] ->
		PremisesOut = PremisesInAndThe, 
		GraphOut = GraphB
		;
		% In the nocheck chase, we don't skolemize,
		% Instead, existential quantification is used
		( PremisesB = [type~quantifier..name~'?'] ->
			make_implication_formula(Id,PremisesA,PremisesB,ImpFormula)
			;
			B = accafter~AccafterB,
			get_all_new_drefs(B,DrefsB),
			get_free_var(AccafterB,DrefsB,FreeVarB),
			quantify_existentially(Id,FreeVarB,PremisesB,QPremisesB),
			make_implication_formula(Id,PremisesA,QPremisesB,ImpFormula)
		),
		quantify_universally(Id,VariablesA,ImpFormula,QFormulaList),

		QFormulaList = [QFormula],
		QFormula = id~QId,

		PremisesOut = [QFormula | PremisesInAndThe],
		add_edges(GraphTmp,[Id-QId],GraphOut)
	).

% check case
% Variables A and Premises A empty, just add the obligations for B
assumption_cond_check(_Id,Accafter,_IdA,B,[],[],_AccafterA,GraphIn,GraphOut,ObIn,ObOut,PremisesBegin,PremisesEnd) :-
	check_prs(B,Accafter,GraphIn,GraphOut,ObIn,ObOut,PremisesBegin,PremisesEnd,check).
	
% Variables A not empty, but PremisesA empty, quantify universally over VariablesA
assumption_cond_check(Id,Accafter,IdA,B,VariablesA,[],AccafterA,GraphIn,GraphOut,ObIn,ObOut,PremisesBegin,PremisesEnd) :-
	!,
	% Check B just with PremisesBegin (because A is empty)
	% If CheckTrigger = nocheck then don't check B, else do check it
	check_prs(B,Accafter, GraphIn, BGraph,ObIn,ObOut,PremisesBegin,PremisesBeginAndB,check),

	% Two cases:
	% 1 : The user made a proof by contradiction
	% 2 : We deal with a normal assumption	
	% % The last entry in BPremisesEnd determines that
	( PremisesBeginAndB = [type~relation..arity~0..name~'$false' | _ ] ->
		(
		% Contradiction case
		% Add 'false' to PremisesBegin
		PremisesEnd = [type~relation..arity~0..name~'$false'..id~cont(Id) | PremisesBegin],
		add_edges(BGraph,[cont(Id)-Id,Id-cont(Id)],GraphOut)
		)
		;
		(
		% For all Formulas X in PremisesB add 
		% ! [Variables in MrefsA] : X
		% to our Premises storage
		append(PremisesB,PremisesBegin,PremisesBeginAndB),

		( IdA = axiom(_) ->
			(
			update_assumption(Id,PremisesBegin,VariablesA,AccafterA,[],PremisesB,[],_UpdateGraph,PremisesEnd),
			BGraph = GraphOut
			);
			update_assumption(Id,PremisesBegin,VariablesA,AccafterA,[],PremisesB,BGraph,GraphOut,PremisesEnd)
		)
		)
	).


% VariablesA and PremisesA not empty, quantify+conjunct
assumption_cond_check(Id,Accafter,IdA,B,VariablesA,PremisesA,AccafterA,GraphIn,GraphOut,ObIn,ObOut,PremisesBegin,PremisesEnd) :-
	add_formula_link(IdA,PremisesA,GraphIn,GraphA),
	% Check B with all the Premises from A included
	% The PremisesBegin values are Premises_tmpa, 
	% which we got from check_prs(A,..)
	% The PremisesEnd value will later be used for all-quantification.
	
	append(PremisesA,PremisesBegin,PremisesBeginAndA),
	
	% If CheckTrigger = nocheck then don't check B, else do check it
	check_prs(B,Accafter,GraphA,BGraph,ObIn,ObOut,PremisesBeginAndA,PremisesBeginAndAAndB,check),
	!,

	% Two cases:
	% 1 : The user made a proof by contradiction
	% 2 : We deal with a normal assumption	
	% % The last entry in BPremisesEnd determines that
	( PremisesBeginAndAAndB = [type~relation..arity~0..name~'$false'..id~FId | _] ->
		(
		% Contradiction case
		%
		% Negate Assumption, quantify universally over variables in MrefsA and append
		% the formula to PremisesBegin
		negate_formulas(IdA,PremisesA,NegatedA),
		quantify_universally(Id,VariablesA,NegatedA,QNegatedA),
		QNegatedA = [NP],
		PremisesEnd = [NP | PremisesBegin],
				
		QNegatedA = [id~FormulaId],
		add_edges(BGraph,[Id-FormulaId,FormulaId-Id,FormulaId-FId],GraphOut)
		)
		;
		(
		% For all Formulas X in Premises_tmpb / Premises_tmp_a add 
		% a new over all free variables in A quantified Statement of the form
		% ! [Variables in MrefsA] : Fol_A -> X
		% to our Premises storage
		append(PremisesB,PremisesBeginAndA,PremisesBeginAndAAndB),
		( IdA = axiom(_) ->
			(
			update_assumption(Id,PremisesBegin,VariablesA,AccafterA,PremisesA,PremisesB,[],_UpdateGraph,PremisesEnd),
			BGraph = GraphOut
			);
			update_assumption(Id,PremisesBegin,VariablesA,AccafterA,PremisesA,PremisesB,BGraph,GraphOut,PremisesEnd)
		)
		)
	).
	


%%	mark_contradiction(Premises:list)
%
%	Marks all elements of the list with contradiction~yes
	
mark_contradiction([]).

mark_contradiction([contradiction~yes|Tail]) :-
	mark_contradiction(Tail).

%%	search_for_contradiction(+Conds:list)
%
%	succeeds if the last element of Conds is a PRS and the last Condition of the PRS is contradiction

search_for_contradiction(Conds) :-
	append(_,[conds~PRSConds],Conds),
	append(_,[contradiction],PRSConds).


