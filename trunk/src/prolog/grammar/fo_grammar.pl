:- module(fo_grammar,[
	fo_function_symbol/3,
	fo_variable/3,
	fo_term/4,
	fo_formula/4,
	fo_chained_formula/5,
	fo_term_without/5,
	fo_free_predicate_symbol/4,
	fo_free_function/5,
	fo_succ/4,
	fo_induction_start/7,
	fo_induction_step/7,
	fo_var_list/4
	]).

:- use_module(library(pldoc)).
:- use_module(naproche(gulp4swi)).
:- use_module(naproche(error_logger)).
:- use_module(naproche(math_lexicon)).
:- use_module(naproche(utils)).


/** <module> Grammar Rules for First-Order Formulas and Terms
 *
 * This module describes all the grammar rules that are used to
 * parse first-order constructs. It is mainly used in the PRS construction
 * process. The input contains no white spaces!
 *
 * @author Daniel Kuehlwein
 * @author Mona Rahn
 * @author Marcos Cramer
 */



% ---------------- Terminal Symbols ---------------------
% Terminals
left_bracket -->
	['('],!.
right_bracket -->
	[')'],!.
comma -->
	[','],!.

% Variable
fo_variable(Tree) -->
	fo_complex_variable(Tree).

fo_variable(Tree) -->
	fo_simple_variable(Tree).

% Simple Variable
fo_simple_variable(Tree) -->
	[Symbol],
	{
	Tree=type~variable..dref~_,
	math_lexicon([Symbol],Tree)
	}.

% Complex Variable
fo_complex_variable(Tree) -->
	fo_simple_variable(STree),
	['_'],
	url:digits(D),
	{
	STree = name~SName,
	atomic_list_concat([SName|D],Name),
	change_feature(STree,name,Name,Tree)
	}.	

% Constant
fo_constant(Tree) -->
	{
	Tree=type~constant,
	math_lexicon(Symbol,Tree)
	},
	Symbol,!.

% Function
fo_function_symbol(Tree) -->
	{
	Tree=type~function,
	math_lexicon(Symbol,Tree)
	},
	Symbol,!.

% Relation
fo_relation(Tree) -->
	{
	Tree=type~relation,
	math_lexicon(Symbol,Tree)
	},
	Symbol,!.

% Logic Symbols
fo_logical_symbol(Tree) -->
	{
	Tree=type~logical_symbol,
	math_lexicon(Symbol,Tree)
	},
	Symbol,!.

% Quantifier
fo_quantifier(Tree) -->
	{
	Tree=type~quantifier,
	math_lexicon(Symbol,Tree)
	},
	Symbol,!.

% ---------------------- Functions --------------------------

%%	fo_simple_function(-Tree,-FreeVar)
%
%	Accepts all function of the form f(...).
fo_simple_function(Tree,FreeVar) -->
	{
	Tree = args~List
	},
	fo_function_symbol(Tree),
	left_bracket,
	fo_term_list(List,FreeVar),
	right_bracket.

%%	fo_term_list(-List,-FreeVar)
%
%	Succeeds if the input is a of the form Term1,Term2,...,TermN.
%	and List is [Term1,...,TermN].

fo_term_list([Arg],FreeVar) -->
	fo_term_intern(Arg,FreeVar).

fo_term_list([Tree|Tail],FreeVar) -->
	fo_term_intern(Tree,TreeVar),
	comma,
	fo_term_list(Tail,FreeVarList),
	{ union_in_right_order(TreeVar,FreeVarList,FreeVar) }.


%%	fo_succ_function(-Tree,-FreeVar)
%
%	Let t be a term and succ be a function symbol. Accepts all functions of the form tsucc.

fo_succ_function(Tree,[VariableTree]) -->
	fo_variable(VariableTree),
	!,
	fo_function_symbol(Tree),
	{
	Tree = name~succ..args~[VariableTree]
	}.
	
fo_succ_function(Tree,[]) -->
	fo_constant(ConstantTree),
	!,
	fo_function_symbol(Tree),
	{
	Tree = name~succ..args~[ConstantTree]
	}.
	
fo_succ_function(Tree,FreeVar) -->
	fo_simple_function(TermTree,FreeVar),
	!,
	fo_function_symbol(Tree),
	{
	Tree = name~succ..args~[TermTree]
	}.
	
fo_succ_function(Tree,FreeVar) -->
	left_bracket,
	fo_succ_function(TermTree,FreeVar),
	right_bracket,
	!,
	fo_function_symbol(Tree),
	{
	Tree = name~succ..args~[TermTree]
	}.
	
fo_succ_function(Tree,FreeVar) -->
	left_bracket,
	fo_middle_function(TermTree,FreeVar),
	right_bracket,
	!,
	fo_function_symbol(Tree),
	{
	Tree = name~succ..args~[TermTree]
	}.
	

%%	fo_middle_function(-Tree,-FreeVar)
%
%	Let * be a funtion symbol
%	Accepts all function of the form LHS * RHS

fo_middle_function(Tree,FreeVar) -->
	fo_middle_function_mul(Tree,FreeVar).
fo_middle_function(Tree,FreeVar) -->
	fo_middle_function_nonmul(Tree,FreeVar).

fo_middle_function_mul(Tree,FreeVar) -->
	{ Tree = name~mul..arity~2..args~[Arg1,Arg2] },
	fo_middle_function_LHS(Arg1,FreeVar1),	
	fo_function_symbol(Tree),
	fo_middle_function_LHS(Arg2,FreeVar2),	
	{ 
	union_in_right_order(FreeVar1,FreeVar2,FreeVar)
	}.

fo_middle_function_nonmul(Tree,FreeVar) -->
	{ Tree = arity~2..args~[Arg1,Arg2] },
	fo_middle_function_mul(Arg1,FreeVar1),	
	fo_function_symbol(Tree),
	fo_term_intern(Arg2,FreeVar2),
	{ 
	union_in_right_order(FreeVar1,FreeVar2,FreeVar)
	}.
fo_middle_function_nonmul(Tree,FreeVar) -->
	{ Tree = arity~2..args~[Arg1,Arg2] },
	fo_middle_function_LHS(Arg1,FreeVar1),	
	fo_function_symbol(Tree),
	fo_term_intern(Arg2,FreeVar2),
	{ 
	union_in_right_order(FreeVar1,FreeVar2,FreeVar)
	}.

%%	fo_middle_function_LHS(-Tree,-FreeVar)
%
%	Accepts all that is allowed to appear as an Argument of the LHS of 
%	a middle function.
%	e.g. Terms or (middle_function).

fo_middle_function_LHS(Tree,FreeVar) -->
	left_bracket,
	fo_middle_function(Tree,FreeVar),
	right_bracket.
fo_middle_function_LHS(Tree,FreeVar) -->
	left_bracket,
	fo_middle_function_LHS(Tree,FreeVar),
	right_bracket.
fo_middle_function_LHS(Tree,FreeVar) -->
	fo_succ_function(Tree,FreeVar).
fo_middle_function_LHS(Tree,FreeVar) -->
	fo_simple_function(Tree,FreeVar).
fo_middle_function_LHS(Tree,[Tree]) -->
	fo_variable(Tree).
fo_middle_function_LHS(Tree,[]) -->
	fo_constant(Tree).

%%	fo_function(-Tree,-FreeVar)
%
%	Accepts all functions.
%	@param FreeVar is a list of trees

fo_function(Tree,FreeVar) -->
	fo_simple_function(Tree,FreeVar).
fo_function(Tree,FreeVar) -->
	fo_succ_function(Tree,FreeVar).
fo_function(Tree,FreeVar) -->
	fo_middle_function(Tree,FreeVar).

% ---------------------- Terms ------------------------------

fo_term(List,Freevar) --> 
	fo_term_intern(List,Freevar),
	!.
/*
% Error case
% Concats the first five atoms where the term is expected and gives them out as one atom
fo_term(_,_) -->
	([X1],!;{X1=''}),
	([X2],!;{X2=''}),
	([X3],!;{X3=''}),
	([X4],!;{X4=''}),
	([X5],!;{X5=''}),
	{
	concat_atom([X1, X2, X3, X4, X5],X),
	add_error_message_once(syntax,fo_term, X,'Term expected'),
	fail
	}.
*/
fo_term_intern(Tree,FreeVar) -->
	fo_variable(Tree),
	{FreeVar=[Tree]}.

fo_term_intern(Tree,[]) -->
	fo_constant(Tree).	

fo_term_intern(Tree,FreeVar) -->
	fo_function(Tree,FreeVar).

fo_term_intern(Tree,FreeVar) -->
	left_bracket,
	fo_term_intern(Tree,FreeVar),
	right_bracket.


% ------------------- Formulas -------------------------------
% In order to prevent infinite recursion, we differentiate between 
% normal formulas and simple formulas. Normal formulas allow connectives
% to be used without bracketing (left bracketing is assumed). A simple
% formula is either a formula without connectives, or a formula with
% brackets around it.

fo_formula(Tree,FreeVar) -->
	fo_formula_intern(Tree,FreeVar),
	!.

fo_formula(Tree,FreeVar) -->
	fo_chained_formula(_,Tree,FreeVar),
	!.
/*
% Error case
% Concats the first five atoms where the formula is expected and gives them out as one atom
fo_formula(_,_) -->
	([X1],!;{X1=''}),
	([X2],!;{X2=''}),
	([X3],!;{X3=''}),
	([X4],!;{X4=''}),
	([X5],!;{X5=''}),
	{
	concat_atom([X1, X2, X3, X4, X5],X),
	add_error_message_once(syntax,fo_formula, X,'Formula expected'),
	fail
	}.
*/
%% fo_chained_formula(Term1:DOBSOD,Tree:DOBSOD,FreeVar:list(DOBSOD))
%
%  Parses chained formulas.
%  Term1 is the first term in the formula. Term1 is stored because every term 
%  (except the terms at the beginning and at the end) is needed twice in Tree.
%
%  Example:
%  ==
%  
%  fo_chained_formula(Term1,Tree,FreeVar,[x,'=',x,r,y,r,z],[]).
%  Term1 = type~variable..arity~0..name~x,
%  Tree = type~logical_symbol..arity~2..name~ & .. args~[
%  		type~relation..arity~2..name~ (=)..args~[type~variable..arity~0..name~x, type~variable..arity~0..name~x], 
%  		type~logical_symbol..arity~2..name~ & .. args~[
%  			type~relation..arity~2..name~r..args~[type~variable..arity~0..name~x, type~variable..arity~0..name~y], 
%  			type~relation..arity~2..name~r..args~[type~variable..arity~0..name~y, type~variable..arity~0..name~z]
%  		]
%  	],
%  FreeVar = [type~variable..arity~0..name~x, type~variable..arity~0..name~y, type~variable..arity~0..name~z].
%  ==

fo_chained_formula(Term1,Tree,FreeVar) -->
	fo_term_intern(Term1,FreeVar1),
	fo_relation(RelationTree),
	fo_chained_formula(Term2,Tree2,FreeVar2),
	{
	union_in_right_order(FreeVar1,FreeVar2,FreeVar),
	RelationTree = args~[Term1,Term2],
	Tree = type~logical_symbol..name~'&'..arity~2..args~[RelationTree,Tree2]
	}.

fo_chained_formula(Term1,RelationTree,FreeVar) -->
	fo_term_intern(Term1,FreeVar1),
	fo_relation(RelationTree),
	fo_term_intern(Term2,FreeVar2),
	{
	union_in_right_order(FreeVar1,FreeVar2,FreeVar),
	RelationTree = args~[Term1,Term2]
	}.

fo_simple_formula(Tree,FreeVar) -->
	left_bracket,
	fo_formula_intern(Tree,FreeVar),
	right_bracket,
	!.	

fo_simple_formula(Tree,[]) -->
	{ Tree = arity~0 },
	fo_relation(Tree).	

fo_simple_formula(Tree,FreeVar) -->
	{ 
	Tree = args~TermList
	},	
	fo_relation(Tree),
	left_bracket,
	fo_term_list(TermList,FreeVar),
	right_bracket,
	!.

fo_simple_formula(Tree,FreeVar) -->
	{ 
	Tree = arity~2..args~[TermTree1,TermTree2]
	},
	fo_term_intern(TermTree1,FreeVar1),
	fo_relation(Tree),
	fo_term_intern(TermTree2,FreeVar2),
	{ union_in_right_order(FreeVar1,FreeVar2,FreeVar) },
	!.

fo_simple_formula(Tree,FreeVar) -->
	{ Tree = args~[QuantifiedVars,FoFormula] },
	fo_quantifier(Tree),
	fo_var_list(QuantifiedVars,BoundVar),
	fo_formula_intern(FoFormula,FreeVarFoFormula),
	{ subtract(FreeVarFoFormula,BoundVar,FreeVar) },
	!.

fo_simple_formula(Tree,FreeVar) -->
	{ Tree = arity~1..args~[FoFormula] },
	fo_logical_symbol(Tree),
	fo_formula_intern(FoFormula,FreeVar),
	!.



fo_formula_intern(Tree,FreeVar) -->
	fo_simple_formula(Tree,FreeVar).


fo_formula_intern(Tree,FreeVar) -->
	{ Tree = arity~2..args~[FoFormula1,FoFormula2] },
	fo_simple_formula(FoFormula1,FreeVar1),
	fo_logical_symbol(Tree),
	fo_formula_intern(FoFormula2,FreeVar2),
	{ union_in_right_order(FreeVar1,FreeVar2,FreeVar) }.


fo_var_list([Tree],[Tree]) -->
	fo_variable(Tree).

fo_var_list([VarTree|ListTree],FreeVar) -->
	fo_variable(VarTree),
	comma,
	fo_var_list(ListTree,FreeVarRest),
	{union_in_right_order([VarTree],FreeVarRest,FreeVar) }.


% ------------- Special Notions ---------------------------------

%%	fo_free_predicate_symbol(-Tree,-FreeVar)
%
%	Accepts unquantified formulas without logical symbols

fo_free_predicate_symbol(Tree,[]) -->
	{ Tree = arity~0 },
	fo_relation(Tree),
	!.
fo_free_predicate_symbol(Tree,FreeVar) -->
	{ 
	Tree = args~TermList
	},
	fo_relation(Tree),
	left_bracket,
	fo_term_list(TermList,FreeVar),
	right_bracket,
	!.

fo_free_predicate_symbol(Tree,FreeVar) -->
	{ 
	Tree = arity~2..args~[TermTree1,TermTree2]
	},
	fo_term_intern(TermTree1,FreeVar1),
	fo_relation(Tree),
	fo_term_intern(TermTree2,FreeVar2),
	{ union_in_right_order(FreeVar1,FreeVar2,FreeVar) },
	!.


%%	fo_free_function(-FunctionSymbol,-Tree,-FreeVar)
%
%	Let f be the FunctionsSymbol, T be the Term.
%	Succeeds if T has the form f(x_1,...,x_n), where the x_i are distinct variables.
%	Tree contains the Term in tree form, FreeVar contains the free Variables of T.

fo_free_function(FunctionSymbol,Tree,[]) -->
	fo_constant(Tree),
	{ Tree=name~FunctionSymbol },
	!.

fo_free_function(FunctionSymbol,Tree,FreeVar) -->
	{ Tree = name~FunctionSymbol..args~ArgList},
	fo_function(Tree,FreeVar),
	{ fo_check_arglist(ArgList) },
	!.

%%	fo_check_arglist(+List)
%
%	Checks that List is a list of distinct variables.

fo_check_arglist([]).
fo_check_arglist([Head|Tail]) :-
	Head = type~variable,
	\+ member(Head,Tail),
	fo_check_arglist(Tail).

%%	fo_term_without(+FunctionSymbol,-Tree,-FreeVar)
%
%	Let f be the FunctionSymbol, T be the Term.
%	Succeeds if f does not appear in T.
%	Tree contains the Term in tree form, FreeVar contains the free Variables of T.

fo_term_without(FunctionSymbol,Tree,FreeVar,X,Y) :-
	fo_term_intern(Tree, FreeVar, X, Y),
	append(Term, Y, X),
	\+ member(FunctionSymbol, Term),
	!.
/*
fo_term_without(FunctionSymbol,Tree,FreeVar) -->
	{
	\+ member(FunctionSymbol, Tree),
	},
	fo_term_intern(Tree, FreeVar).
	%TAG


fo_term_without(_,Tree,FreeVar) -->
	fo_variable(Tree),
	{FreeVar=[Tree]},
	!.

fo_term_without(FunctionSymbol,Tree,[]) -->
	{ Tree = name~Name },
	fo_constant(Tree),
	{ Name \= FunctionSymbol },
	!.	

fo_term_without(FunctionSymbol,Tree,FreeVar) -->
	fo_function_without(FunctionSymbol,Tree,FreeVar),
	!.

fo_term_without_list(FunctionSymbol,[Tree],FreeVar) -->
	fo_term_without(FunctionSymbol,Tree,FreeVar).

fo_term_without_list(FunctionSymbol,[Tree|TreeList],FreeVar) -->
	fo_term_without(FunctionSymbol,Tree,FreeVarTerm),
	comma,
	fo_term_without_list(FunctionSymbol,TreeList,FreeVarList),
	{ union_in_right_order(FreeVarTerm,FreeVarList,FreeVar) }.


%%	fo_function_without(+FunctionSymbol,-Tree,-FreeVar)
%
%	Let f be the FunctionSymbol, T be the Term.
%	Succeeds if T is a function with Arguments, and f does not appear in T.
%	Tree contains the Term in tree form, FreeVar contains the free Variables of T.

fo_function_without(FunctionSymbol,Tree,FreeVar) -->
	{ Tree = name~Name..args~ArgList },
	fo_function_symbol(Tree),
	{ Name \= FunctionSymbol },
	left_bracket,
	fo_term_without_list(FunctionSymbol,ArgList,FreeVar),
	right_bracket,
	!.

fo_function_without(FunctionSymbol,Tree,FreeVar) -->
	{ Tree = name~Name..arity~2..args~[Arg1,Arg2] },
	{ Name \= FunctionSymbol },
	fo_term_without(FunctionSymbol,Arg1,FreeVar1),
	fo_function_symbol(Tree),
	fo_term_without(FunctionSymbol,Arg2,FreeVar2),
	{ union_in_right_order(FreeVar1,FreeVar2,FreeVar) },
	!.

fo_function_without(FunctionSymbol,Tree,FreeVar) -->
	{ Tree = arity~2..args~[Arg1,Arg2] },
	fo_term_without(FunctionSymbol,Arg1,FreeVar1),	
	fo_function_symbol(Tree),
	fo_term_without(FunctionSymbol,Arg2,FreeVar2),
	{ 
	union_in_right_order(FreeVar1,FreeVar2,FreeVar)
	}.
*/

%%	fo_succ(-Tree,-FreeVar)
%
%	Succeeds if the String to be parsed is of the form succ(Var).
%	Tree contains the Term in tree form, FreeVar contains the Var.

fo_succ(Tree,FreeVar) -->
	{ 
	Tree = type~function..name~succ..arity~1..args~[VarTree]
	},
	[succ],
	left_bracket,
	fo_variable(VarTree),
	right_bracket,
	{ FreeVar=[VarTree] },
	!.

%%	fo_induction_start(+FunctionSymbol,-Number,-Tree,-LHSFreeVar,-RHSFreeVar)
%
%	Let F be the FunctionSymbol, N the Number. Tree are as usual.
%	Succeeds if F has arity M > N, and F with 1 as Nth Argument and variables
%	at all other arguments is defined by an F-free Term. LHSFreeVar are the
%	variables on the left hand side of the defining equation, RHSFreeVar
%	those on the right hand side.

fo_induction_start(FunctionSymbol,Number,Tree,LHSFreeVar,RHSFreeVar) -->
	{ Tree = name~'='..arity~2..type~relation..args~[LHSTree,RHSTree] },
	fo_induction_start_LHS(FunctionSymbol,['1'],Number,LHSTree,LHSFreeVar),
	fo_relation(Tree),
	fo_term_without(FunctionSymbol,RHSTree,RHSFreeVar),
	!.


%%	fo_induction_step(+FunctionSymbol,+Number,-Tree,-LHSFreeVar,-RHSFreeVar)
%
%	Let F be the FunctionSymbol, N be the Number.
%	True if the parsed item defines F(succ(Var)) as a Term of F(Var).
%	Succ(Var) and (Var) must be the Nth argument of F. All other 
%	arguments of F must be variables.
%	Tree as usual. LHSFreeVar are the variables on the left hand side of the
%	defining equation, RHSFreeVar those on the right hand side.

fo_induction_step(FunctionSymbol,Number,Tree,LHSFreeVar,RHSFreeVar) -->
	{ Tree = name~'='..arity~2..type~relation..args~[LHSTree,RHSTree] },
	fo_induction_step_LHS(FunctionSymbol,Var,Number,LHSTree,LHSFreeVar),
	fo_relation(Tree),
	fo_induction_step_RHS(FunctionSymbol,Var,Number,RHSTree,RHSFreeVar),
	!.

%%	fo_induction_start_LHS(+FunctionSymbol,Symbol,?Number,-Tree,-FreeVar)
%
%	Let F be the FunctionSymbol, N be the Number, Tree and FreeVar as usual.
%	Succeeds if the String parsed has the form F(...,Symbol,...), where Symbol is the
%	Nth Argument of F and all other arguments are distinct variables. 

fo_induction_start_LHS(FunctionSymbol,Arg,N,Tree,FreeVar) -->
	{
	math_lexicon(Arg,ArgTree), 
	Tree = name~FunctionSymbol..type~function..args~TermList
	},
	fo_function(Tree,FreeVar),
	{ 
	Tree = args~TermList,
	nth1(N,TermList,ArgTree),
	select(ArgTree,TermList,VariableList),
	fo_check_arglist(VariableList)
	},
	!.
/*
fo_induction_start_LHS(FunctionSymbol,Arg,1,Tree,[VarArg]) -->
	{
	math_lexicon(Arg,ArgTree), 
 	Tree = name~FunctionSymbol..type~function..arity~2..args~[ArgTree,VarArg]
	},
	Arg,
	fo_function_symbol(Tree),
	fo_variable(VarArg),
	!.

fo_induction_start_LHS(FunctionSymbol,Arg,2,Tree,[VarArg]) -->
	{
	math_lexicon(Arg,ArgTree),
	Tree = name~FunctionSymbol..type~function..arity~2..args~[VarArg,ArgTree] 
	},
	fo_variable(VarArg),
	fo_function_symbol(Tree),
	Arg,
	!.
*/
%%	fo_induction_step_LHS(+FunctionSymbol,SuccTree,?Number,-Tree,-FreeVar)
%
%	Let F be the FunctionSymbol, N be the Number, Tree and FreeVar as usual.
%	Succeeds if the String parsed has the form F(...,succ(Var),...), Symbol
%	is the Nth Argument of F, and all other arguments are variables not
%	equal to Var. SuccTree is the tree created from succ(Var).


fo_induction_step_LHS(FunctionSymbol,Var,N,Tree,FreeVar) -->
	{
	Tree = name~FunctionSymbol..type~function..args~List
	},
	fo_function(Tree,FreeVar),
	{
	fo_induction_step_list(N,List,Var,FreeVar)
	},
	!.
/*
% Special Case for functions in the middle
fo_induction_step_LHS(FunctionSymbol,Var,1,Tree,FreeVar) -->
	{
 	Tree = name~FunctionSymbol..type~function..arity~2..args~[SuccTree,VarArg]
	},	
	fo_succ(SuccTree,SuccFreeVar),
	fo_function_symbol(Tree),
	fo_variable(VarArg),
	{
	SuccFreeVar \= [VarArg],
	SuccTree=args~[Var],
	union_in_right_order(SuccFreeVar,[VarArg],FreeVar)
	},
	!.
fo_induction_step_LHS(FunctionSymbol,Var,2,Tree,FreeVar) -->
	{
	Tree = name~FunctionSymbol..type~function..arity~2..args~[VarArg,SuccTree] 
	},
	fo_variable(VarArg),
	fo_function_symbol(Tree),
	fo_succ(SuccTree,SuccFreeVar),
	{ 
	SuccFreeVar \= [VarArg],
	SuccTree = args~Var,
	union_in_right_order([VarArg],SuccFreeVar,FreeVar)
	},	
	!.
*/
%%	fo_induction_step_list(+N,+List,-Var,-FreeVar)
%
%	Succeeds if List has the form [Term1,...,TermM], where
%	TermN = succ(Var) and all other terms are distinct variables 
%	not equal to Var.
%	Return Var and the free variables.
fo_induction_step_list(N,List,Var,FreeVar) :-
	nth1(N,List,SuccVar),
	SuccVar = type~function..name~succ..args~[Var],
	replace_nth(N,List,Var,FreeVar),
	fo_check_arglist(FreeVar).

%%	replace_nth(+Index, +List, +Element, -NewList)
%
%    	Replace the Nth (1-based) element of a list.
%    	This is taken over from pl-5.7.8, but undefined in pl-5.6.64. So when we update prolog, 
%    	we can remove this definition.

replace_nth(1, [_|T], V, [V|T]) :- !.
replace_nth(I, [H|T0], V, [H|T]) :-
	I2 is I - 1,
	replace_nth(I2, T0, V, T).

%% 	fo_induction_step_RHS(+FunctionSymbol,+Var,+Number,-Tree,-FreeVar)
%
%	Accepts the input if it does not contain the function symbol, expect when
%	it comes in the form f(...,Var,..) with the Var entry on the Nth
%	position.

fo_induction_step_RHS(FunctionSymbol,Var,Number,Tree,FreeVar) -->
	% Parse the term	
	fo_term_intern(Tree,FreeVar),
	% Now check the Tree whether it fullfills the conditions.
	{ fo_check_tree(Tree,FunctionSymbol,Var,Number) }.

%%	fo_check_tree(+Tree,+FunctionSymbol,+Var,+Number)
%
%	Checks whether every occurence of FunctionSymbol in the tree has Var as argument number Number.

fo_check_tree(Tree,_,_,_) :-
	Tree = type~constant.

fo_check_tree(Tree,_,_,_) :-
	Tree = type~variable.

fo_check_tree(Tree,FunctionSymbol,Var,Number) :-
	Tree = type~function..name~FunctionSymbol..args~ArgList,
	nth1(Number,ArgList,Var).

fo_check_tree(Tree,FunctionSymbol,Var,Number) :-
	Tree = type~function..name~Name..args~ArgList,
	Name \= FunctionSymbol,
	fo_check_list(ArgList,FunctionSymbol,Var,Number).

fo_check_list([],_,_,_).
	
fo_check_list([Head|Tail],FunctionSymbol,Var,Number) :-
	fo_check_tree(Head,FunctionSymbol,Var,Number),
	fo_check_list(Tail,FunctionSymbol,Var,Number).

