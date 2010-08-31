:-module(premises,[
	quantify_existentially/4,
	quantify_universally/4,
	update_definitions/8,
	update_assumption/9,
	update_disjunction/4,
	make_implication_formula/4,
	make_induction_formulas/4,
	make_equivalence_formula/4,
	make_conjunction/2,
	make_conjunction_list/2,
	negate_formulas/3,
	sort_premises_for_the/9,
	replace_in_formula/4,
	replace_in_formula_list/4,
	freevar_list/2
	]).


:- use_module(library(pldoc)).
:- use_module(library(ugraphs)).
:- use_module(naproche(utils)).
:- use_module(naproche(graph)).
:- use_module(naproche(gulp4swi)).

reset_counter :-
	nb_setval(premises_counter,0).

/**	<module> Premises update predicates
 *
 * 	This module provides predicates to update the premises after some high level structure change in a prs.
 * 	e.g. closing assumptions, new definition, negation
*/

%% make_induction_formulas(+Id,+ListFormula:list(DOBSOD),-FormulaStart:list(DOBSOD),-FormulaStep:list(DOBSOD))
%
%	Let ListFormula be [!X: (Conjunction(X) => Conseq(X))]
%	Conjunction(X) must contain naturalnumber(X) as one of its conjuncts. Let RestConnjunction(X) be the result of
%	deleting naturalnumber(X) from Conjunction(X), and let Formula(X) be (RestConjunction(X) => Conseq(X)).
%	Then FormulaStart is [Formula(1)] and FormulaStep is [!X: ((naturalnumber(X) &  Formula(X)) => Formula(Succ(X)))]
%
%	Example:
%	(Example still without the naturalnumber part!)
%	==
%	premises:make_induction_formulas(
%		[type~quantifier..name~!..arity~2..args~[
%			[type~variable..name~x..arity~0],
%			type~relation..name~ (=)..arity~2..args~[
%       			type~variable..name~x..arity~0,
%       			type~variable..name~x..arity~0
%	                        ]
%	                ]],
%		Formula1,
%		FormulaSucc),
%
%	        Formula1 = type~relation..arity~2..name~ (=)..args~['1', '1'],
%	        FormulaSucc = [type~quantifier..name~!..args~[
%	        		[type~variable..arity~0..name~x],
%	        		type~logical_symbol..arity~2..name~ (=>)..args~[
%	                        	type~relation..arity~2..name~ (=)..args~[type~variable..arity~0..name~x, type~variable..arity~0..name~x],
%	                                type~relation..arity~2..name~ (=)..args~[
%	                                	type~function..arity~1..name~succ..args~[type~variable..arity~0..name~x],
%	                                        type~function..arity~1..name~succ..args~[type~variable..arity~0..name~x]
%	                                ]]]].
%	==

make_induction_formulas(Id,ListFormula,FormulaStart,FormulaStep) :-
	ListFormula = [QuantifiedFormula],
	
	% QuantifiedFormula is !X: (Conjunction(X) => Conseq(X))
	QuantifiedFormula = args~[ListVar,Implication],
	ListVar=[Var|Rest],
	(
		Rest = []
		;
		( add_error_message(logic,make_induction_formulas,Id,'More than one universally quantified variable in induction'), !, fail)
	),
	!,

/*
	% if there is more than one variable universally quantified, we use the first for the induction
	% and quantify universally over the rest within the formula
	quantify_universally(Id,Rest,[Formula],[QFormula]),
*/
	
	(
		Implication = type~logical_symbol..name~'=>'..args~[Conjunction,Conseq]
		;
		( add_error_message(logic,make_induction_formulas,Id,'Induction conclusion not limited to natural numbers.'), !, fail )
	),

	% Conjunction must contain a conjunct of the form naturalnumber(X)
	make_conjunction(ConjunctList,Conjunction),
	(
		member(type~relation..name~naturalnumber..args~[Var],ConjunctList),!
		;
		( add_error_message(logic,make_induction_formulas,Id,'Induction conclusion not limited to natural numbers.'), !, fail )
	),

	% Formula(X) is (RestConjunction(X) => Conseq(X)), where RestConjunction(X) is the result of deleting naturalnumber(X) from Conjunction(X).
	delete(ConjunctList,type~relation..name~naturalnumber..args~[Var],RestConjunctList),
	make_implication_formula(Id,RestConjunctList,[Conseq],[Formula]),

	% make formula for induction start
	replace_in_formula(Var,Formula,type~constant..name~'1'..arity~0,FormulaStartNotList),
	FormulaStart=[FormulaStartNotList],

	% make formula for induction step
	VarSucc=type~function..arity~1..name~succ..args~[Var],
	replace_in_formula(Var,Formula,VarSucc,FormulaSucc),
	make_implication_formula(Id,[type~relation..id~nat(Id)..name~naturalnumber..args~[Var],Formula],[FormulaSucc],NotQuantifFormulaStep),
	quantify_universally(ind(Id),[Var],NotQuantifFormulaStep,FormulaStep).


%%	replace_in_formula_list(+Old:list,+Formula,+New:list,-NewFormula)
%
%	replaces all elements of Old with the corresponding elements of New in Formula.
%	e.g. the first element of Old with the first element of New etc

replace_in_formula_list([],Formula,[],Formula) :- !.
replace_in_formula_list([Old|Olds],OldFormula,[New|News],NewFormula) :-
	replace_in_formula(Old,OldFormula,New,FormulaTmp),
	!,
	replace_in_formula_list(Olds,FormulaTmp,News,NewFormula).


%% replace_in_formula(+X:DOBSOD,+Formula:DOBSOD,+ReplaceBy:DOBSOD,-NewPhi:DOBSOD)
% 
% 	Replaces X (type~variable) in Formula by ReplaceBy
%
% 	Example:
% 	==
%	
%       premises:replace_in_formula(
%       type~variable..name~x..arity~0,
%       type~quantifier..name~!..arity~2..args~[
%       	[type~variable..name~x..arity~0],
%               type~relation..name~ (=)..arity~2..args~[
%               	type~variable..name~x..arity~0,
%                       type~variable..name~x..arity~0
%                       ]
%               ],
%       type~constant..name~c..arity~0,
%       X),
%	X =     type~quantifier..arity~2..name~!..args~[
%			[type~constant..arity~0..name~c],
%	                type~relation..arity~2..name~ (=)..args~[
%	                type~constant..arity~0..name~c,
%	                type~constant..arity~0..name~c]].
%
% 	==

replace_in_formula(X,Formula,ReplaceBy,NewFormula) :-
	% Case that Formula is quantified
	Formula = type~quantifier..name~Name..arity~Arity..args~[ListVar,FormulaIntern]..id~Id,
	!,
	replace_variables(X,ListVar,ReplaceBy,NewListVar),
	replace_in_formula(X,FormulaIntern,ReplaceBy,NewFormulaIntern),
	NewFormula = type~quantifier..name~Name..args~[NewListVar,NewFormulaIntern]..arity~Arity..id~replace(Id).

replace_in_formula(X,Formula,ReplaceBy,NewFormula) :-
	% Case that Formula is a relation
	Formula = type~relation..name~Name..arity~Arity..args~Args..id~Id,
	!,
	replace_variables(X,Args,ReplaceBy,NewArgs),
	NewFormula = type~relation..name~Name..args~NewArgs..name~Name..arity~Arity..id~replace(Id).


replace_in_formula(X,Formula,ReplaceBy,NewFormula) :-
	% Case that Formula is a binary logical symbol
	Formula = type~logical_symbol..name~Name..arity~Arity..args~[Arg1,Arg2]..id~Id,
	!,
	replace_in_formula(X,Arg1,ReplaceBy,NewArg1),
	replace_in_formula(X,Arg2,ReplaceBy,NewArg2),
	NewFormula = type~logical_symbol..name~Name..arity~Arity..args~[NewArg1,NewArg2]..id~replace(Id).

replace_in_formula(X,Formula,ReplaceBy,NewFormula) :-
	% Case that Formula is a unary logical symbol
	Formula = type~logical_symbol..name~Name..arity~Arity..args~[Arg]..id~Id,
	!,
	replace_in_formula(X,Arg,ReplaceBy,NewArg),
	NewFormula = type~logical_symbol..name~Name..arity~Arity..args~[NewArg]..id~replace(Id).

%% replace_variables(+Old:DOBSOD,+Args:list(DOBSOD),+New:DOBSOD,-NewArgs:list(DOBSOD))
% 
% 	Replaces every occurence of Old in Args by New
%
% 	Example:
%	==
%	premises:replace_variables(
%		type~variable..name~x,					% we replace x
%		[							% in this formula
%			type~variable..name~x,
%			type~function..name~f..arity~2..args~[
%				type~variable..name~x,
%				type~constant..name~c],
%			type~variable..name~y
%		],
%		type~variable..name~z,NewFormula), 			% by y
%	NewFormula = [
%		type~variable..name~z, 
%		type~function..arity~2..name~f..args~[
%			type~variable..name~z, 
%			type~constant..name~c], 
%		type~variable..name~y].
%	==

replace_variables(Old,[Old|Args],New,[New|NewArgs]) :-
	!,
	replace_variables(Old,Args,New,NewArgs).

replace_variables(Old,[Arg|Args],New,[Arg|NewArgs]) :-
	Arg = type~variable,
	!,
	replace_variables(Old,Args,New,NewArgs).

replace_variables(Old,[Arg|Args],New,[Arg|NewArgs]) :-
	Arg = type~constant,
	!,
	replace_variables(Old,Args,New,NewArgs).

replace_variables(Old,[Arg|Args],New,[NewArg|NewArgs]) :- 
	Arg = type~function..name~Name..arity~Arity..args~FArgs,
	!,
	NewArg = type~function..name~Name..arity~Arity..args~NewFArgs,
	replace_variables(Old,FArgs,New,NewFArgs),
	replace_variables(Old,Args,New,NewArgs).

replace_variables(_Old,[],_New,[]).

% -------------------------------------------------------------------------------------------------------------------

%%	update_definitions(+Premises:list(DOBSOD),+Id:term,+FreeVarsA:list:DOBSOD,+FreeVarB:list:DOBSOD,+ListA:list(DOBSOD),+ListB:list(DOBSOD),
%%	-FormulaId,-NewPremises:list(DOBSOD)).
%
%   Adds 
%   ! [Variables in MrefsA] : ConjunctA <=> ? [Variables in MrefsB] ConjunctB
%   to the list of Premises, 
%   where ConjunctA is the conjunct of all elements of ListA
%
%	Sep 2009 dk

update_definitions(Premises,_,_,_,[],_,_,Premises) :- 
	add_error_message(logic,update_definition,'0','Definiendum PRS is empty'),
	!,
	fail.

update_definitions(Premises,_,_,_,_,[],_,Premises) :- 
	add_error_message(logic,update_definition,'0','Definiens PRS is empty'),
	!,
	fail.

update_definitions(Premises,Id,VariablesA,VariablesB,ListA,ListB,FormulaId,NewPremises) :-
	make_conjunction(ListA,A),
	
	quantify_existentially(Id,VariablesB,ListB,[B]),
	
	New = type~logical_symbol ..name~'<=>' ..arity~2 ..args~[A,B]..id~def(Id,0),!,
	
	% If VariablesA is empty, append New. Else quantify over VariablesA
	( VariablesA = [] ->
		NewDef = New
		;
		NewDef = type~quantifier ..name~'!' ..arity~2 ..args~[VariablesA,New]..id~def(Id,1)
	),

	%Update Graph
	NewDef= id~FormulaId,

	NewPremises = [NewDef | Premises].

%-------------------------------------------------------------------------------------  
   	
%%	update_assumption(+Id,+Premises:list(DOBSOD),+VarA:list(DOBSOD),+AccessiblesA,+ListA:list(DOBSOD),+ListB:list(DOBSOD),+GraphIn,-GraphOut,-NewPremises:list(DOBSOD)).	
%
%	For each formula B in ListB
%	! [VariablesA] : ConjunctA => B
%	is added to the Premises,
% 	where ConjunctA is the conjunct of all formulas in ListA,
%   	VariablesA is the list of variables in MrefsA. 
%	NewPremises is all Premises with all the new formulas appended.	
%
%	Sep 09 dk


update_assumption(Id,Premises,VariablesA,AccA,ListA,ListB,GraphIn,GraphOut,NewPremises) :- 
% to avoid using make_conjunction and variables every time when update_assumption is called
% Case that A is empty
	ListA = [],
	!,

	update_assumption_intern_A_empty(VariablesA,AccA,[],Id,0,ListB,GraphIn,TmpGraph,Temp),
	add_formula_link(Id,Temp,TmpGraph,GraphOut),

	append(Temp,Premises,NewPremises).

update_assumption(Id,Premises,VariablesA,AccA,ListA,ListB,GraphIn,GraphOut,NewPremises) :- 
% to avoid using make_conjunction and variables every time when update_assumption is called
% Case that A is not empty
	make_conjunction(ListA,A),
	!,

	% SkolemVariables are empty at the beginning
	update_assumption_intern(VariablesA,AccA,Id, 0,[], A, ListB, GraphIn,TmpGraph,Temp),
	add_formula_link(Id,Temp,TmpGraph,GraphOut),

	append(Temp,Premises,NewPremises).

update_assumption_intern(_VarA,_AccA,_Id,_N,_SkolemVar,_A,[],GraphIn,GraphIn,[]) :-
	% case that ListB is empty (update_assumption is finished)
	!.	

update_assumption_intern(VariablesA,AccA,Id, N, SkolemVariables, A, ListB,GraphIn,GraphOut,NewPremises) :-
	ListB = [H|T],
	% Get the free variables from H by extracting free ones from H and then
	% subtracting those that have occurred previously, i.e. in A or in the premises.
	freevar(H,VariablesH,SkVarH),
	subtract_set(VariablesH,VariablesA,FreeVariablesTmp),
	substract_accessibles(FreeVariablesTmp,AccA,FreeVariables),

	% Assign skolemized variables to FreeVariables
	create_skolem_variables(SkolemVariables, VariablesA, FreeVariables, NewSkolemVariables),
	%Already existing skolem functions need to be updated to have the free variables of A as argument as well
	update_skolem_variables(SkVarH,VariablesA,UpdatedSkolemVariables),

	% Skolemize H
	skolemize(H,FreeVariables,NewSkolemVariables,SkolemFormulaTmp),
	replace_in_formula_list(SkVarH,SkolemFormulaTmp,UpdatedSkolemVariables,SkolemH),

	( VariablesA = [] ->
		Newassump = type~logical_symbol ..name~'=>' ..arity~2 ..args~[A,SkolemH]..id~ass(Id,N);
		Newassump = type~quantifier ..name~'!' ..arity~2 ..args~[VariablesA,type~logical_symbol ..name~'=>' ..arity~2 ..args~[A,SkolemH]..id~imp(Id,N)]..id~ass(Id,N)
	),!,
	
	% Update Graph
	M is N+1,
	H = id~OldId,
	Newassump = id~NewId,
	add_edges(GraphIn,[NewId-OldId],TmpGraph),

   	update_assumption_intern(VariablesA,AccA,Id,M,NewSkolemVariables,A,T,TmpGraph,GraphOut,Temp),
	NewPremises = [Newassump|Temp].

update_assumption_intern_A_empty(_VarA,_AccA,_SkolemVar,_Id,_N,[],GraphIn,GraphIn,[]) :-
	% case that ListB is empty (update_assumption is finished)
	!.	

update_assumption_intern_A_empty(VariablesA,AccA,SkolemVariables,Id,N,ListB, GraphIn,GraphOut,NewPremises) :-
	ListB = [H|T],
	% Get the free variables from H
	freevar(H,VariablesH,SkVarH),
	subtract_set(VariablesH,VariablesA,FreeVariablesTmp),
	substract_accessibles(FreeVariablesTmp,AccA,FreeVariables),
	
	% Assign skolemized variables to FreeVariables
	create_skolem_variables(SkolemVariables, VariablesA, FreeVariables, NewSkolemVariables),
	%Already existing skolem functions need to be updated to have the free variables of A as argument as well
	update_skolem_variables(SkVarH,VariablesA,UpdatedSkolemVariables),
   	
	% Skolemize H
	skolemize(H,FreeVariables,NewSkolemVariables,SkolemFormulaTmp),
	replace_in_formula_list(SkVarH,SkolemFormulaTmp,UpdatedSkolemVariables,SkolemH),

	Newassump = type~quantifier ..name~'!' ..arity~2 ..args~[VariablesA,SkolemH]..id~ass(Id,N),

	% Update Graph
	M is N+1,
	H = id~OldId,
	Newassump = id~NewId,
	add_edges(GraphIn,[NewId-OldId],TmpGraph),
   	
   	update_assumption_intern_A_empty(VariablesA,AccA,NewSkolemVariables,Id,M,T,TmpGraph,GraphOut,Temp),
	NewPremises = [Newassump|Temp].

update_skolem_variables([SkVar|SkVars],Vars,[NewSkVar|NewSkVars]) :-
	SkVar = name~Name..args~Args,
	append(Args,Vars,NewArgs),
	NewSkVar = name~Name..args~NewArgs..type~function,
	!,
	update_skolem_variables(SkVars,Vars,NewSkVars).
update_skolem_variables([],_,[]).

substract_accessibles([Var|Rest],AccA,FreeVar) :-
	member(math_id(_,Var),AccA),
	!,
	substract_accessibles(Rest,AccA,FreeVar).
substract_accessibles([Var|Rest],AccA,FreeVar) :-
	Var = dref~Dref,
	member(Dref,AccA),
	!,
	substract_accessibles(Rest,AccA,FreeVar).
substract_accessibles([Var|Rest],AccA,[Var|FreeVar]) :-
	substract_accessibles(Rest,AccA,FreeVar).
substract_accessibles([],_AccA,[]).


%% 	skolemize(+Formula,+Variables:list,+SkolemVariables:list,+SkolemizedFormula)
%	Replaces each variable in Variables in Formula by the corresponding skolemized variable found in SkolemizedVariables
%	
%	Example:
%
%	==
%
%	skolemize(type~relation..name~r..arity~2..args~[ 								% Formula
%			type~variable..name~x..arity~0,									
%			type~variable..name~y..arity~0],
%		[type~variable..name~x..arity~0,type~variable..name~y..arity~0],					% Variables
%		[													% SkolemVariables
%			skolem_id(type~variable..name~x..arity~0,type~function..name~skolem..arity~2..args~[1,type~variable..name~z..arity~0]),
%			skolem_id(type~variable..name~y..arity~0,type~function..name~skolem..arity~2..args~[2,type~variable..name~z..arity~0])
%		],
% 		type~relation..arity~2..name~r..args~[									% Skolemized Formula
% 			type~function..arity~2..name~skolem..args~[1, type~variable..name~z..arity~0], 
% 			type~function..arity~2..name~skolem..args~[2, type~variable..name~z..arity~0]]). 
%
%	==


skolemize(Formula,[],_SkolemVariables,Formula) :- !.

skolemize(Formula,Variables,SkolemVariables,SkolemizedFormula) :-
	Variables = [Variable|Rest],
	SkolemId = skolem_id(Variable,SkolemVariable),
	member(SkolemId,SkolemVariables),
	replace_in_formula(Variable,Formula,SkolemVariable,FormulaTmp),
	skolemize(FormulaTmp,Rest,SkolemVariables,SkolemizedFormula).


%% 	create_skolem_variables(+SkolemVariables,+VariablesA,+AdditionalVariables,-NewSkolemVariables)
%
%	For each variable in AdditionalVariables that has no SkolemId yet
%	we add a new Skolem-Id to SkolemVariables
%
%	for example, if x is a new variable, we add: 
%
%	skolem_id(x,type~function..name~skolem..args~[VariablesA])


create_skolem_variables(SkolemVariables, _VariablesA, [], SkolemVariables) :-!.

create_skolem_variables(SkolemVariables, VariablesA, AdditionalVariables, NewSkolemVariables) :-
	% case that Variable already appears in SkolemVariables
	AdditionalVariables = [Variable|Rest],
	SkolemId = skolem_id(Variable,_),
	member(SkolemId,SkolemVariables),
	!,
	create_skolem_variables(SkolemVariables, VariablesA, Rest, NewSkolemVariables).

create_skolem_variables(SkolemVariables, VariablesA, AdditionalVariables, NewSkolemVariables) :-
	% get number of variables in A for SkolemArity
	length(VariablesA,SkolemArity),

	% case that Variable does not yet appear in SkolemVariables
	% we add a new skolem_id
	AdditionalVariables = [Variable|Rest],

	% Use the Dref of the variable for the SkolemId
	Variable = dref~Dref,
	
	( var(Dref) -> 
		(
		nb_getval(premises_counter,N),!,
		atom_concat(skolem,N,SkName),
		succ(N,M),
		nb_setval(premises_counter,M)
		);
		atom_concat(skolem,Dref,SkName)
	),

	% add a new skolem_id for Variable
	SkolemFunction = type~function..name~SkName..arity~SkolemArity..args~VariablesA,

	SkolemVarTmp = [skolem_id(Variable,SkolemFunction) | SkolemVariables],

	create_skolem_variables(SkolemVarTmp,VariablesA,Rest,NewSkolemVariables).




% ------------------------------------------------------------------------------------------------------------------------------

%%	make_equivalence_formula(+Id,+PremisesA:list(DOBSOD),+PremisesB:list(DOBSOD),-NewFormula:list(DOBSOD)).	
%	
%	Equivalence: A <=> B
%	
%	NewFormula is 
%		A <=> B
% 	where A is the conjunct of all formulas in PremisesA,
% 	B is the conjunct of all formulas in PremisesB.
%
%	@author Feb 18 2010 dk

make_equivalence_formula(_Id,PremisesA, [], A) :-
	!,
	make_conjunction_list(PremisesA,A).
make_equivalence_formula(_Id,[], PremisesB, B) :-
	!,
	make_conjunction_list(PremisesB,B).
make_equivalence_formula(Id,PremisesA, PremisesB, NewFormula) :-
	make_conjunction(PremisesA,A),
	make_conjunction(PremisesB,B),
	NewFormula = [type~logical_symbol ..name~'<=>' ..arity~2 ..args~[A,B] ..id~eq(Id)].

% -----------------------------------------------------------------------------------------------------------------------------


%%	make_implication_formula(+Id:Term,+PremisesA:list(DOBSOD),+PremisesB:list(DOBSOD),-NewFormula:list(DOBSOD)).	
%	
%	Implies: A => B	
%	NewFormula is 
%		A => B
% 	where A is the conjunct of all formulas in PremisesA,
% 	B is the conjunct of all formulas in PremisesB,
%
%   @author Feb 18 2010 dk
   
make_implication_formula(_Id, _PremisesA, [], []) :- !.
make_implication_formula(_Id, [], PremisesB, NewFormula) :-
	!,
	make_conjunction_list(PremisesB,NewFormula). 
make_implication_formula(Id, PremisesA, PremisesB, NewFormula) :-
	make_conjunction(PremisesA,A),
	make_conjunction(PremisesB,B),
	NewFormula = [type~logical_symbol ..name~'=>' ..arity~2 ..args~[A,B]..id~imp(Id)].


% -----------------------------------------------------------------------------------------------------------------------------


%%	quantify_existentially(+Id,+Variables:list(DOBSOD),+Premises:list(DOBSOD),-NewFormula:list(DOBSOD)).	
%	
%	NewFormula is 
%		? [Variables] Formula
% 	where Formula is the conjunct of all formulas in Premises,
%   	and Variables is the list of variables in Mrefs. 
%	If one of the variables in Variables is in an equals relation in the Premises, it is substituted and deleted.
%
%	June 10 dk

quantify_existentially(_Id,_Variables,[],[]) :- !.
quantify_existentially(_Id,[],Premises,[Formula]) :- 
	!,
	make_conjunction(Premises,Formula).	
%Ignore non-free existing Variables
quantify_existentially(Id,[Variable|Variables],Premises,NewFormula) :-
	freevar_list(Premises,FreeVar),
	\+ member(Variable, FreeVar),!,
	quantify_existentially(Id,Variables,Premises,NewFormula).

% Case that we have 'x=x' as formula
quantify_existentially(Id,[Variable|Variables],Premises,NewFormula) :- 
	is_equal_to(Variable, Premises, Variable, Equality),!,
	delete_in_formulas(Premises, Equality, NewPremises),
	( appears_again(Variable, NewPremises) ->
		quantify_existentially(Id,[Variable|Variables],NewPremises,NewFormulaTmp);
		quantify_existentially(Id,Variables,NewPremises,NewFormulaTmp)
	),
	( NewFormulaTmp = [] ->
			NewFormula = [type~relation..name~'$true'..id~qe(Id)];
			NewFormula = NewFormulaTmp
	).
quantify_existentially(Id,[Variable|Variables],Premises,NewFormula) :- 
	is_equal_to(Variable, Premises, EqualTo, Equality),!,
	delete_in_formulas(Premises, Equality, TmpPremises),
	substitute(Variable, EqualTo, TmpPremises, SubstitutePremises),
	quantify_existentially(Id,Variables,SubstitutePremises,NewFormulaTmp),
	( NewFormulaTmp = [] ->
			NewFormula = [type~relation..name~'$true'..id~qe(Id)];
			NewFormula = NewFormulaTmp
	).
quantify_existentially(Id,Variables,Premises,NewFormula) :- 
	make_conjunction(Premises,Formula),	
	NewFormula = [type~quantifier ..name~'?' ..args~[
		Variables,
		Formula]..id~qe(Id)].

/*quantify_existentially(Id,Variables,Premises,NewFormula) :-
	make_conjunction(Premises,Formula),
	quantify_existentially_intern(Id,Variables,Formula,NewFormula).

quantify_existentially_intern(_Id,[],Formula,[Formula]) :- !.
quantify_existentially_intern(Id,Variables,Formula,NewFormula) :-
	NewFormula = [type~quantifier ..name~'?' ..args~[
		Variables,
		Formula]..id~qe(Id)].
*/

% -----------------------------------------------------------------------------------------------------------------------------


%%	quantify_universally(+Id,+FreeVar:list(DOBSOD),+Premises:list(DOBSOD),-NewFormula:list(DOBSOD)).	
%	
%	NewFormula is 
%		! [Variables] Formula
% 	where Formula is the conjunct of all formulas in Premises,
%   	and Variables is the list of variables in Mrefs. 
%	If we have only one premise that is an implication and one of the variables in Variables is in an equals relation in the Premises, and the variable is not
%	free in the other side of the Equality, then it is substituted and deleted.
%
%	Jun 10 dk

quantify_universally(_Id,_Variables,[],[]) :- !.
quantify_universally(_Id,[],Premises,[Formula]) :- 
	!,
	make_conjunction(Premises,Formula).
%Ignore non-free existing Variables
quantify_universally(Id,[Variable|Variables],Premises,NewFormula) :-
	freevar_list(Premises,FreeVar),
	\+ member(Variable, FreeVar),!,
	quantify_universally(Id,Variables,Premises,NewFormula).
% Implication Case
quantify_universally(Id,[Variable|Variables],[Premise],NewFormula) :-
	Premise = type~logical_symbol..name~'=>'..args~[LHS,_RHS],
	is_equal_to(Variable, [LHS], EqualTo, Equality),
	Equality = name~'='..args~[LHSE,RHSE],	
	( LHSE = Variable, freevar(RHSE,FreeVarRHSE), \+ member(Variable,FreeVarRHSE) ;
	  RHSE = Variable, freevar(LHSE,FreeVarLHSE), \+ member(Variable,FreeVarLHSE)),!,
	delete_in_formulas([Premise],Equality,TmpPremises),
	substitute(Variable, EqualTo, TmpPremises, SubstitutePremises),
	quantify_universally(Id,Variables,SubstitutePremises,NewFormulaTmp),
	( NewFormulaTmp = [] ->
			NewFormula = [type~relation..name~'$true'..id~qu(Id)];
			NewFormula = NewFormulaTmp
	).
	
quantify_universally(Id,[Variable|Variables],[Premise],NewFormula) :-
	Premise = type~logical_symbol..name~'=>'..args~[_LHS,_RHS],!,
	quantify_universally(Id,Variables,[Premise],NewFormulaTmp),
	( NewFormulaTmp = [type~quantifier..name~'!'] ->
		(
		NewFormulaTmp = [args~[BoundedVariables,Formula]..id~NewId],
		NewFormula = [type~quantifier..name~'!'..args~[[Variable|BoundedVariables],Formula]..id~NewId]
		);
		(
		NewFormulaTmp = [NewFormulaTmpList],
		NewFormulaTmpList = id~OldId,
		NewFormula = [type~quantifier ..name~'!' ..arity~2 ..args~[
			[Variable],	NewFormulaTmpList]..id~qu(Id,OldId)]
		)
	).

/*	
quantify_universally(Id,[Variable|Variables],Premises,NewFormula) :- 
	is_equal_to(Variable, Premises, EqualTo, Equality),!,
	delete_in_formulas(Premises,Equality, TmpPremises),
	substitute(Variable, EqualTo, TmpPremises, SubstitutePremises),
	quantify_universally(Id,Variables,SubstitutePremises,NewFormulaTmp),
	( NewFormulaTmp = [] ->
			NewFormula = [type~relation..name~'$true'..id~qu(Id)];
			NewFormula = NewFormulaTmp
	).
*/
quantify_universally(Id,Variables,Premises,NewFormula) :- 
	make_conjunction(Premises,Formula),	
	Formula = id~OldId,
	NewFormula = [type~quantifier ..name~'!' ..arity~2 ..args~[
		Variables,
		Formula]..id~qu(Id,OldId)].


/*
%%	quantify_universally_simple(Id,Var,Premises,NewFormula)
%
% Quantification without equation substitution.

quantify_universally_simple(_Id,_Var,[],[]) :- !.
quantify_universally_simple(Id,Variables,Premises,NewFormula) :-
	make_conjunction(Premises,Formula),
	quantify_universally_intern(Id,Variables,Formula,NewFormula).

quantify_universally_intern(_Id,[],Formula,[Formula]) :- !.
quantify_universally_intern(Id,Variables,Formula,NewFormula) :-
	Formula = id~OldId,
	NewFormula = [type~quantifier ..name~'!' ..arity~2 ..args~[
		Variables,
		Formula]..id~qu(Id,OldId)].
*/

% ---------------------------------------------------------------------------------------------------------------------------------

%%	appears_again(Variable, Premises:list)
%
%	Succeeds if Variable appears in one of the premises

appears_again(Variable,[Premise|_Premises]) :-
	freevar(Premise, FreeVar),
	member(Variable, FreeVar),!.
appears_again(Variable, [_Premise|Premises]) :-
	appears_again(Variable, Premises).

%%	is_equal_to(+Variable, +Premises, -EqualTo, -Formula)
%
%	Succeeds if Variable = EqualTo or EqualTo=Variable is a formula in the Premises.
%   Formula is the equation formula

is_equal_to(Variable, [Premise|_Premises], EqualTo, Premise) :-
	Premise = type~relation..name~'='..args~[Variable, EqualTo]. 
is_equal_to(Variable, [Premise|_Premises], EqualTo, Premise) :-
	Premise = type~relation..name~'='..args~[EqualTo, Variable].
is_equal_to(Variable, [Premise|_Premises], EqualTo, EqualFormula) :-
	Premise = type~logical_symbol..name~'&'..args~Formulas,
	is_equal_to(Variable, Formulas, EqualTo, EqualFormula).
is_equal_to(Variable, [Premise|_Premises], EqualTo, EqualFormula) :-
	Premise = type~logical_symbol..name~'=>'..args~[Ante,_Succ],
	is_equal_to(Variable, [Ante], EqualTo, EqualFormula).
is_equal_to(Variable, [Premise|_Premises], EqualTo, EqualFormula) :-
	Premise = type~logical_symbol..name~'<=>'..args~Formulas,
	is_equal_to(Variable, Formulas, EqualTo, EqualFormula).
is_equal_to(Variable, [_Premise|Premises], EqualTo, EqualFormula) :-
	is_equal_to(Variable, Premises, EqualTo, EqualFormula).

%%	delete_in_formulas(+ListIn:DOBSODS,+Formula:DOBSOD,-ListOut:DOBSODS)
%
%	delete Formula in ListIn, if ListIn contains a formula which is equal to Formula, contains Formula as a argument of a conjunction or quivalence; 
%	or contains an implication where formula is a subformula of the Antecedence.

delete_in_formulas([],_Formula,[]).
delete_in_formulas([Formula|Premises],Formula,PremisesOut) :-
	!,
	delete_in_formulas(Premises,Formula,PremisesOut).
delete_in_formulas([Premise|Premises],Formula,PremisesOut) :-
	Premise = type~logical_symbol..name~'&'..args~Args..id~Id,
	!,
	delete_in_formulas(Args, Formula, NewArgs),
	( NewArgs = [] -> 
		delete_in_formulas(Premises,Formula,PremisesOut);
		( NewArgs = [OneFormula] ->
			PremiseOut = OneFormula;
			PremiseOut = type~logical_symbol..name~'&'..args~NewArgs..id~Id
		),
		delete_in_formulas(Premises,Formula,TmpPremisesOut),
		PremisesOut = [PremiseOut|TmpPremisesOut]
	).
delete_in_formulas([Premise|Premises],Formula,PremisesOut) :-
	Premise = type~logical_symbol..name~'=>'..args~[Ante,Succ]..id~Id,
	!,
	delete_in_formulas([Ante], Formula, NewAnteList),
	( NewAnteList = [] -> 
		PremiseOut = Succ;
		NewAnteList = [NewAnte],
		PremiseOut = type~logical_symbol..name~'=>'..args~[NewAnte,Succ]..id~Id
	),
	delete_in_formulas(Premises,Formula,TmpPremisesOut),
	PremisesOut = [PremiseOut|TmpPremisesOut].
delete_in_formulas([Premise|Premises],Formula,PremisesOut) :-
	Premise = type~logical_symbol..name~'<=>'..args~[Arg1,Arg2]..id~Id,
	!,
	delete_in_formulas([Arg1], Formula, NewArg1List),
	delete_in_formulas([Arg2], Formula, NewArg2List),
	( (NewArg1List = [], NewArg2List = []) ->
		delete_in_formulas(Premises, Formula, PremisesOut)
		;
		( NewArg1List = [] ->
			NewArg2List = [PremiseOut];
			( NewArg2List = [] -> 
				NewArg1List = [PremiseOut];
				NewArg1List = [NewArg1],
				NewArg2:ist = [NewArg2],
				PremiseOut = type~logical_symbol..name~'<=>'..args~[NewArg1,NewArg2]..id~Id
			)
		),
		delete_in_formulas(Premises,Formula,TmpPremisesOut),
		PremisesOut = [PremiseOut|TmpPremisesOut]
	).
delete_in_formulas([Premise|Premises], Formula, [Premise|PremisesOut]) :-
	delete_in_formulas(Premises, Formula, PremisesOut).

%%	substitute(+Variable, +Substitute, +Premises, -SubstitutePremises)
%
%	Substitues all occurences of Variable with Substitute
substitute(_Variable, _Substitute, [], []) :- !.
substitute(Variable, Substitute, [Premise|Premises], [SubstitutePremise|SubstitutePremises]) :-
	replace_in_formula(Variable, Premise, Substitute, SubstitutePremise),
	substitute(Variable, Substitute, Premises, SubstitutePremises).


% ------------------------------------------------------------------------------------------------------------------------------

%%	update_disjunction(+Id,+ListA:list(DOBSOD),+ListB:list(DOBSOD),-ListFormulas:list(DOBSOD)).	
%
%	Conjuncts ListA and ListB and disjuncts the result.	
%
%   @author Daniel Kuehlwein

% If one of the lists is empty, there is no need for disjunction
update_disjunction(_Id,[],ListB,ListB) :- !.
update_disjunction(_Id,ListA,[],ListA) :- !.
update_disjunction(Id,ListA,ListB,Out) :-
	make_conjunction(ListA,AOut),
	make_conjunction(ListB,BOut),
	Out = [type~logical_symbol ..name~'|' ..arity~2  ..args~[AOut,BOut]..id~dis(Id)].

% ------------------------------------------------------------------------------------------------------------------------------

%%	make_conjunction(?ConjunctList:list(DOBSOD),-Conjunction:DOBSOD)
%
%	Conjunction is a conjunction of the formulae in ConjunctList. This predicate
%	can be used either for conjuncting formulae or for splitting up a conjunction
%	into a list of formulae.
%	
%	Example:
%
%	==
%	make_conjunction([type~variable ..name~x,type~variable ..name~y],                              % Input list of DOBSODs
%                    type~logical_symbol..name~ & .. arity~2..args~ 
%                    [type~variable ..name~x,type~variable ..name~y]                               % Output conjunct DOBSOD
%					).
%
%	==


make_conjunction([H|T],Out) :-
 	Out = type~logical_symbol ..name~'&' ..arity~2 ..args~[H,Temp]..id~and(HId,TId),
	H = id~HId, 
	Temp = id~TId,
 	make_conjunction(T,Temp),
	!.
make_conjunction([X],X) :- !.
make_conjunction([],[]) :- 
	add_error_message(logic,make_conjunction,'0','Empty Conjunction'),
	fail.	


% ------------------------------------------------------------------------------------------------------------------------------

%%	make_conjunction_list(+In:list(DOBSOD),-Out:list(DOBSOD))
%
%	Like make_conjunction, but the output is a list

make_conjunction_list(In,[Out]):-
	make_conjunction(In,Out).
 
%-------------------------------------------------------------------------------------

%% 	negate_formulas(+Id:Term,+Formulas:list(DOBSOD),-NegFormulas:list(DOBSOD)).
%
%  Takes a list of DOBSODs, conjuncts them via '&' and negates the formula.
%  Gives out a list because checker.pl always handles premises (like PRS_premises, Negated_Premises) in form of lists
%
%	Sep 09 dk

negate_formulas(_Id,[],[]):-
	!.

negate_formulas(Id,Formulas,NegFormulas) :- 
	make_conjunction(Formulas,Temp),
	NegFormulas = [type~logical_symbol.. name~'~' ..arity~1 ..args~[Temp]..id~neg(Id)].

%-------------------------------------------------------------------------------------

%% freevar(+A:DOBSOD, -FreeA:list(DOBSOD), -SkolemVar:list).	
%
% Gives the list of free variables in the DOBSOD A.
% Also calculates the Skolemized Variables in A.
%
% Example :
%
% ==
% freevar(type~logical_symbol ..name~'=>' ..arity~2 ..args~                                             
%         [type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~x],
%		   type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~y]
%		  ],                                                                                       % Input DOBSOD
%		  [type~variable..name~x, type~variable..name~y]                                           % List of free variables
%		 ).
% ==

freevar(A,[],[]) :- 
	A = type~constant,!.

freevar(A,[A],[]) :- 
	A = type~variable,!.

freevar(A,[],[A]) :- 
	A = type~function..name~Name,
	atom_concat(skolem,_,Name),!.

freevar(A,FreeA,SkA) :- 
	A = type~function..args~ARGS,!,
	freevar_list(ARGS,FreeA,SkA).

freevar(A,FreeA,SkA) :- 
	A = type~relation..args~ARGS,!,
	freevar_list(ARGS,FreeA,SkA).

freevar(A,FreeA,SkA) :- 
	A = type~logical_symbol..args~ARGS,!,
	freevar_list(ARGS,FreeA,SkA).

freevar(A,FreeA,SkA) :- 
	A= type~quantifier..args~[BoundList,Formula],!,
	freevar_list(BoundList,Bound,SkList),
	freevar(Formula,Free,SkATmp),
	append(SkATmp,SkList,SkA),
	subtract(Free,Bound,FreeA).


%% freevar(+A:DOBSOD, -FreeA:list(DOBSOD)).	
%
% Gives the list of free variables in the DOBSOD A.
%
% Example :
%
% ==
% freevar(type~logical_symbol ..name~'=>' ..arity~2 ..args~                                             
%         [type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~x],
%		   type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~y]
%		  ],                                                                                       % Input DOBSOD
%		  [type~variable..name~x, type~variable..name~y]                                           % List of free variables
%		 ).
% ==

freevar(A,[]) :- 
	A = type~constant,!.

freevar(A,[A]) :- 
	A = type~variable,!.

freevar(A,FreeA) :- 
	A = type~function..args~ARGS,!,
	freevar_list(ARGS,FreeA).

freevar(A,FreeA) :- 
	A = type~relation..args~ARGS,!,
	freevar_list(ARGS,FreeA).

freevar(A,FreeA) :- 
	A = type~logical_symbol..args~ARGS,!,
	freevar_list(ARGS,FreeA).

freevar(A,FreeA) :- 
	A= type~quantifier..args~[BoundList,Formula],!,
	freevar_list(BoundList,Bound),
	freevar(Formula,Free),
	subtract(Free,Bound,FreeA).

% --------------------------------------------------------------------------------------------

%%	freevar_list(+List:list(DOBSOD),-Output:list(DOBSOD),:SkVar:List)
%
%	Gets the free Variables from all the elements in the input list.
%	Also calculates all skolemized variables and stores them in SkVar

freevar_list([],[],[]).
freevar_list([X|Rest],Output,SkVar) :-
	freevar(X,FreeX,SkX),
	freevar_list(Rest,Tmp,SkTmp),
	union(FreeX,Tmp,Output),
	union(SkX,SkTmp,SkVar).


%%	freevar_list(+List:list(DOBSOD),-Output:list(DOBSOD))
%
%	Gets the free Variables from all the elements in the input list.

freevar_list([],[]).
freevar_list([X|Rest],Output) :-
	freevar(X,FreeX),
	freevar_list(Rest,Tmp),
	union(FreeX,Tmp,Output).

%%	equal(+Arg1,+Arg2,-Formula)
%
%	Formula := Arg1 = Arg2
%
%	@author Feb 17 2010 dk

equal(Arg1,Arg2,type~relation..name~'='..arity~2..args~[Arg1,Arg2]).
	
%%	make_disjunction(+List:list,-Disjunction:Tree)	
%
%	Disjunct alle the elements of List

make_disjunction([Formula|Formulas],Disjunction) :-
	Disjunction = type~logical_symbol..name~'|'..arity~2..args~[Formula,DisjunctionTmp],
	make_disjunction(Formulas,DisjunctionTmp).
make_disjunction([Formula],Formula).

%%	make_var_from_dref(+Dref,-Var)
%
%	Creates the to a Dref corresponding Variable
%	Var = type~variable..dref~Dref
%
%	@author Feb 17 2010 dk

make_var_from_dref(Dref,type~variable..dref~Dref).


%%  sort_premises_for_the(+Id,+AccIn,+PremisesIn:list,+PremisesInAndA:list,+GraphIn,-GraphOut,-PremisesInAndThe:list,-PremisesA:list,+CheckTrigger)
%
%   Considers all elements of PremisesInAndA. All elements of PremisesIn with  the~yes are sorted into PremisesInAndThe,
%   the others are put into PremisesA. If there are Premises preceeding the~yes formulas, then the the~yes formulas need to be quantified and skolemized.
%	Variables that appear in AccIn are bound and therefore we do not quantify over them.
%
%   ==
%
%   PremisesInAndA = [X,the~yes,Y,PremisesIn]
%   PremisesA = [X,Y]
%   PremisesInAndThe = [forall y Y -> skolemize(the~yes) | Premises In]
%
%   ==
%
%   @param PremisesInAndA = [X|PremisesIn]


sort_premises_for_the(_Id,_AccIn,PremisesIn,PremisesInAndA,GraphIn,GraphIn,PremisesInAndThe,PremisesA,nocheck) :-
	!,
	sort_premises_for_the(PremisesIn,PremisesInAndA,PremisesInAndThe,PremisesA).

sort_premises_for_the(Id,AccIn,PremisesIn,PremisesInAndA,GraphIn,GraphOut,PremisesInAndThe,PremisesA,check) :-
    sort_premises_for_the(Id,AccIn,PremisesIn,PremisesInAndA,GraphIn,GraphOut,PremisesInAndThe,PremisesA,_SortCounter,_FreeVar,_SkVar,_SkolemVars).

% Nocheck Case
sort_premises_for_the(PremisesIn,PremisesIn,PremisesIn,[]) :- !.

sort_premises_for_the(_PremisesIn,[],_PremisesInAndThe,_PremisesA) :-
	!,
    add_error_message(logic,'sort_premises_for_the',0,'Wrong Input. PremisesInAndA != [_|PremisesIn]'),
    fail.

sort_premises_for_the(PremisesIn,[Formula|Rest],[ImpFormula|PremisesInAndTheTmp],AOut) :-
    subsumes_chk(the~yes,Formula),
	!,
	sort_premises_for_the(PremisesIn,Rest,PremisesInAndTheTmp,AOut),
    make_implication_formula(0,AOut,[Formula],[ImpFormula]),
	ImpFormula = the~yes.
	
sort_premises_for_the(PremisesIn,[Formula|Rest],PremisesInAndTheTmp,[Formula|PremisesATmp]) :-
	!,
	sort_premises_for_the(PremisesIn,Rest,PremisesInAndTheTmp,PremisesATmp).

% Check Case
sort_premises_for_the(_Id,_AccIn,PremisesIn,PremisesIn,GraphIn,GraphIn,PremisesIn,[],0,[],[],[]) :- !.

sort_premises_for_the(Id,_AccIn,_PIn,[],_GIn,_GOut,_PInAndThe,_PA,_SortCounter,_FreeVar,_Skvar,_SkolemVar) :-
	!,
    add_error_message(logic,'sort_premises_for_the',Id,'Wrong Input. PremisesInAndA != [_|PremisesIn]'),
    fail.

sort_premises_for_the(Id,AccIn,PremisesIn,[Formula|Rest],GraphIn,GraphOut,[QFormula|POut],AOut,SortCounter,FreeVar,SkolemVarsOrg,SkolemVars) :-
    subsumes_chk(the~yes,Formula),
    !,
    sort_premises_for_the(Id,AccIn,PremisesIn,Rest,GraphIn,GraphTmp,POut,AOut,SortCounterTmp,FreeVarTmp,SkolemVarsOrgTmp,SkolemVarTmp),

	%Only skolemize the variables that are new
    freevar(Formula,VarsTheTmp,_OldSkVar),
	sort_vars_for_the(VarsTheTmp,VarsThe,FreeVarForm),
	union(FreeVarForm,FreeVarTmp,FreeVarTmp2),
	delete_accessibles(AccIn,FreeVarTmp2,FreeVar),
	subtract(VarsThe,FreeVarTmp,VarsTmp),
    create_skolem_variables(SkolemVarTmp,FreeVar,VarsTmp,SkolemVars),
	%Store all the variables that were skolemized in SkolemVarsOrg
	append(VarsTmp,SkolemVarsOrgTmp,SkolemVarsOrg),

    SortCounter is SortCounterTmp+1,
    make_implication_formula(sort(SortCounter),AOut,[Formula],[ImpFormula]),

	%Already existing skolem functions need to be updated to have the free variables of A as argument as well
%	update_skolem_variables(OldSkVar,FreeVar,NewSkVar),
%	replace_in_formula_list(OldSkVar,QFormula,NewSkVar,SkolemFormulaTmp),
    skolemize(ImpFormula,VarsThe,SkolemVars,SkolemFormula),
    quantify_universally(sort(SortCounter),FreeVar,[SkolemFormula],[QFormula]),
	% Only change the graph if something happened and id != 0
	( ( QFormula = Formula; Id = 0) ->
		GraphTmp = GraphOut;
	 	(
	 	QFormula = id~FId,
		add_edges(GraphTmp,[Id-FId],GraphOut)
		)
	).

% No 'the' formula case: calculate the free variables and skolemize existing 'the' variables
sort_premises_for_the(Id,AccIn,PremisesIn,[Formula|Rest],GraphIn,GraphOut,POut,[SkolemFormula|AOut],SortCounter,FreeVar,SkolemVarsOrg,SkolemVars) :-
    sort_premises_for_the(Id,AccIn,PremisesIn,Rest,GraphIn,GraphTmp,POut,AOut,SortCounter,FreeVarTmp,SkolemVarsOrg,SkolemVars),
	% Get the free var of the formula
    freevar(Formula,Var,_OldSkVar),
	% Subtract the variables that were already skolemized
	subtract(Var,SkolemVarsOrg,NewVars),
	% Calculate the new free variables
    union(NewVars,FreeVarTmp,FreeVar),
	%skolemize the Formula
	intersection(Var,SkolemVarsOrg,SkVars),
	% If SkVars=[], then we need not do anything
	( (SkVars = []) ->
		(
		GraphTmp=GraphOut,
		SkolemFormula = Formula
		)
		;
    	(
		%Already existing skolem functions need to be updated to have the free variables of A as argument as well
%		update_skolem_variables(OldSkVar,FreeVar,NewSkVar),
%		replace_in_formula_list(OldSkVar,Formula,NewSkVar,SkolemFormulaTmp),
		skolemize(Formula,SkVars,SkolemVars,SkolemFormula),
	 	SkolemFormula = id~FId,
		add_edges(GraphTmp,[Id-FId],GraphOut)
		)
	).


sort_vars_for_the([Var|Rest],[Var|TheVar],FreeVar) :-
	subsumes_chk(the~yes,Var),
	!,
	sort_vars_for_the(Rest,TheVar,FreeVar).
sort_vars_for_the([Var|Rest],TheVar,[Var|FreeVar]) :-
	!,
	sort_vars_for_the(Rest,TheVar,FreeVar).
sort_vars_for_the([],[],[]).


%%	delete_accessibles(+Acc,+VarIn:list(Tree),-VarOut:list(Tree))
%
%	Deletes the elements of VarIn that can also be found in Acc.

delete_accessibles(Acc,[Var|Vars],VarOut) :-
	Var = dref~Dref,
	( member(Dref,Acc); member(math_id(Dref,_),Acc); member(plural_dref(Dref,_),Acc) ),!,
	delete_accessibles(Acc,Vars,VarOut).
delete_accessibles(Acc,[Var|Vars],[Var|VarOut]) :-
	!,
	delete_accessibles(Acc,Vars,VarOut).
delete_accessibles(_,[],[]).



