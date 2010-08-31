:- module(dcg,[naproche_text/5]).

:- use_module(library(pldoc)).
:- use_module(naproche(gulp4swi)).
:- use_module(naproche(dcg_lexicon)).
:- use_module(naproche(fo_grammar)).
:- use_module(naproche(dcg_utils)).
:- use_module(naproche(error_logger)).
:- use_module(naproche(prs)).

:- op(601,xfx, user:(=>)).
:- op(601,xfx, user:(==>)).
:- op(601,xfx, user:(:=)).
:- op(601,xfx, user:(<=>)).
:- op(601,xfx, user:(v)).
:- op(601,xfx, user:(<=)).
:- op(699,xfx, user:(::)).

/** 	<module> Naproche Grammar
	
This module parses the input text (in its preparsed Prolog-readable form) and produces the PRS from it. 

@author Marcos Cramer
@author Daniel Kühlwein
@author John Schmid
*/ 

%=====================
% Macrostructure
%=====================


% BASICS


%%	naproche_text(+PRSIn,-PRSOut,?Type,+DCGList,-DCGList)
%
%	A Naproche text consists of axioms, lemmas, theorems and text.
%	Adds the content of the parsed text to the PRS.
%
%	@param Type is a GULP term that has features "initial_text" (taking values "yes" and "no") and "all_ass_closed" (taking values "yes" and "no").

naproche_text(In,Out,_) --> 
	{
	In = accafter~AccIn
	},
	axiom(AccIn,AxiomPRS,ConseqPRSIn),
	naproche_text(ConseqPRSIn,ConseqPRSOut,_),
	add_condition(_,In,AxiomPRS ==> ConseqPRSOut,Out).

naproche_text(In,Out,_) --> 
	theorem(In,TmpOut,_), 
	naproche_text(TmpOut,Out,_).

naproche_text(In,Out,all_ass_closed~no) --> 
	% This rule is for assumptions that don't get closed by a "thus". 
	% We could theoretically refrain from checking the absence of a closing "thus",
	% as such a thus would make the text_tail fail. But this would produce a huge amount
	% of extra progressing power, which would increase exponentially. 
	% Hence we check the absence of a closing "thus" using the auxiliary predicate 
	% no_assumption_closing. The reason why this predicate makes the program much more
	% efficient, is because it doesn't do any work (like PRS construction) apart from 
	% checking the absence of a "thus" that closes this assumption.
	[sentence(Id,Content)],
	rest_list(List),
	{ no_assumption_closing(normal,List,[]) },
	new_prs(Id,In,AssumptionPRSIn),
	{ assumption(AssumptionPRSIn,AssumptionPRSOut,Content,[]) },
	!,
	new_prs(conseq(Id),AssumptionPRSOut,ConclusionPRSIn),
	% In this case, the assumption does not get closed, therefore the complete remaining
	% PRS construction proceeds in the RHS of the Assumption Condition.
	naproche_text(ConclusionPRSIn,ConclusionPRSOut,_),
	add_condition(_,In,AssumptionPRSOut ==> ConclusionPRSOut,Out).

naproche_text(In,Out,_) --> 
	assumption_naproche_text(In,TmpOut), 
	naproche_text(TmpOut,Out,_).

naproche_text(In,Out,_) -->
	[sentence(_,Content)],
	{ var_type_fix(Content,[]) },
	naproche_text(In,Out,_).

naproche_text(In,Out,initial_text~yes) --> 
	text(In,TmpOut,type~no_ass..empty~no), 
	naproche_text(TmpOut,Out,initial_text~no).

naproche_text(In,In,_) --> [].


%%	var_type_fix(+ListIn,-ListOut)
%
%	A var_type_fix is a sentence that links certain variable symbol collections to certain predicates
%	(considered 'types'). 

var_type_fix -->
	symbol_collections([],SymbolData),
	infix_var_type_fix,
	noun_parser(Noun,number~plural),
	{
	getval(var_types,VarTypesIn),
	update_var_types(SymbolData,Noun,VarTypesIn,VarTypesOut),
	setval(var_types,VarTypesOut)
	}.

%%	symbol_collections(+SymbolDataIn,-SymbolDataOut,+ListIn,-ListOut)
%
%	This parses a noun phrase that denotes some symbol collection(s).
%	@param SymbolDataIn and SymbolDataOut are lists of feature lists. Each feature list defines one symbol collection by specifying 
%	the capitalisation and/or the alphabet.

symbol_collections(SDIn,SDOut) -->
	symbol_collection(SD),
	comma_or_and(obligatory),
	symbol_collections([SD|SDIn],SDOut).

symbol_collections(SDIn,[SD1,SD2|SDIn]) -->
	symbol_collection(SD1),
	[and],
	symbol_collection(SD2).

symbol_collections(SDIn,[SD|SDIn]) -->
	symbol_collection(SD).

symbol_collection(SD) -->
	optional_capitalization(SD),
	alphabet(SD),
	[letters].

optional_capitalization(cap~small) -->
	[small].
	
optional_capitalization(cap~cap) -->
	[capital].
	
optional_capitalization(_) -->
	[].

alphabet(alph~latin) -->
	[latin].

alphabet(alph~greek) -->
	[greek].

alphabet(alph~fraktur) -->
	[fraktur].

alphabet(alph~fraktur) -->
	[german].

%%	infix_var_type_fix(+ListIn,-ListOut)
%
%	This is the middle part of a var_type_fix sentence that starts with a symbol collection and ends with a noun.

infix_var_type_fix -->
	[always],
	[denote].

infix_var_type_fix -->
	[will],
	[always],
	[denote].

infix_var_type_fix -->
	[will],
	[be],
	[used],
	[throughout],
	[to],
	[denote].

infix_var_type_fix -->
	[will],
	[stand],
	[throughout],
	[for].


%%	text(+In:PRS,-Out:PRS,?GULP)
%
% 	Text contains statements, definitions, assumptions, assumption closings, cases and case closings. A text can never be empty.
%	Adds the content of the parsed text to the In:PRS
%
%	@param The GULP feature "type" can assume the modi "normal", "no_ass", "all_ass_closed", "theorem" and "in_case". empty can assume the modi no and possible.
	

text(In,Out,type~Type) --> 
	[sentence(Id,Content)],  
	{
	statement(In,TmpOut,Id,Content,[]),
	!
	},
	text(TmpOut,Out,type~Type..empty~possible).

text(In,Out,type~Type) --> 
	definition_text(In,TmpOut),
	text(TmpOut,Out,type~Type..empty~possible).

text(In,Out,type~Type) --> 
	% This rule is for assumptions that don't get closed by a "thus". 
	% We could theoretically refrain from checking the absence of a closing "thus",
	% as such a thus would make the text_tail fail. But this would produce a huge amount
	% of extra progressing power, which would increase exponentially. 
	% Hence we check the absence of a closing "thus" using the auxiliary predicate 
	% no_assumption_closing. The reason why this predicate makes the program much more
	% efficient, is because it doesn't do any work (like PRS construction) apart from 
	% checking the absence of a "thus" that closes this assumption.
	{
	\+ Type = all_ass_closed,
	\+ Type = no_ass
	},
	[sentence(Id,Content)],
	rest_list(List),
	{ no_assumption_closing(Type,List,[]) },
	new_prs(Id,In,AssumptionPRSIn),
	{ assumption(AssumptionPRSIn,AssumptionPRSOut,Content,[]) },
	!,
	new_prs(conseq(Id),AssumptionPRSOut,ConclusionPRSIn),
	% In this case, the assumption does not get closed, therefore the complete remaining
	% PRS construction proceeds in the RHS of the Assumption Condition.
	text(ConclusionPRSIn,ConclusionPRSOut,type~Type..empty~possible),
	add_condition(_,In,AssumptionPRSOut ==> ConclusionPRSOut,Out).

text(In,Out,type~Type) --> 
	% The closed assumption case. For unclosed assumptions, see the previous rule.
	{ \+ Type = no_ass },
	assumption_text(In,TmpOut), 
	text(TmpOut,Out,type~Type..empty~possible).

% during any text, we may introduce cases, close them with a closing statement, and continue with text.
text(In,Out,type~Type) -->
	cases_text(In,TmpOut,type~Type), 
	text(TmpOut,Out,type~Type..empty~possible).

% at the end of a proof, however, if we aren't in a case distinction already, we may introduce cases and don't need to close them.
text(In,Out,type~Type) --> 
	{
	\+ Type = in_case
	},
	cases(In,TmpOut,[],CaseList,type~Type..subtype~beginning),
	{
	% this predicate adds a condition stating that (at least) one of the cases that have been introduced must hold.
	add_case_distinction(CaseList,TmpOut,Out)
	}.

text(In,Out,type~theorem) -->
	theorem(In,TmpOut,lemma),
	text(TmpOut,Out,type~theorem..empty~no).

text(In,In,empty~possible) --> [].


% STATEMENTS
 
%%	statement(+In:PRS,-Out:PRS,+Id)
%
% 	A statement has a proposition coordination as its core, can start with a statement trigger, and can have a reference.
% 	Alternatively, it can just be a reference, or the sentence "trivial" or "contradiction".

statement(In,Out,Id) --> 
	trigger(type~statement), 
	new_prs(Id,In,PRSIn),
	references(PRSIn,TmpPRS1),
	proposition_coord(TmpPRS1,TmpPRS2,mode~finite..subordinated~no),
	references(TmpPRS2,PRSOut),
	add_condition(_,In,PRSOut,Out).

statement(In,Out,Id) --> 
	new_prs(Id,In,PRSIn),
	necessary_references(PRSIn,PRSOut),
	add_condition(_,In,PRSOut,Out).

statement(In,Out,Id) --> 
	[trivial],
	new_prs(Id,In,PRS),
	add_condition(_,In,PRS,Out).

statement(In,Out,Id) --> 
	[contradiction],
	new_prs(Id,In,PRSIn),
	add_condition(_,PRSIn,contradiction,TmpPRS),
	references(TmpPRS,PRSOut),
	add_condition(_,In,PRSOut,Out).


% REFERENCES

%%	references(+PRSIn,-PRSOut)
%
% 	references can be used to refer to a number of axioms, lemmas, theorems or proof methods, (optionally from a different Naproche text).
%	The global variable refids contains a list of ref(RefId,Identifier) tupels of all the known references.

references(PRSIn,PRSOut) -->
	necessary_references(PRSIn,PRSOut).

references(PRSIn,PRSIn) -->
	[].

necessary_references(PRSIn,PRSOut) -->
	[by],
	reference_list(PRSIn,PRSOut),
	comma(comma~optional).

reference_list(PRSIn,PRSOut) -->
	reference(PRSIn,TmpPRS),
	comma_or_and(obligatory),
	reference_list(TmpPRS,PRSOut).

reference_list(PRSIn,PRSOut) -->
	reference(PRSIn,PRSOut).

reference(PRSIn,PRSOut) -->
	[axiom], [Identifier],
	{
	getval(refids,Refs),
    (
        (    member(ref(RefId,axiom(Identifier)) ,Refs), ! )
        ;
        (   concat_atom(['The referenced Axiom ', Identifier, ' does not exist'],Message),
            PRSIn=id~Id,
            add_error_message_once(referenceError,reference,Id,Message), !, fail
        )
    )
	},
	add_rref(PRSIn,RefId,PRSOut).

reference(PRSIn,PRSOut) --> 
	[theorem], [Identifier],
	{
	getval(refids,Refs),
    (
        (    member(ref(RefId,theorem(Identifier)) ,Refs), ! )
        ;
        (   concat_atom(['The referenced Theorem ', Identifier, ' does not exist yet'],Message),
            PRSIn=id~Id,
            add_error_message_once(referenceError,reference,Id,Message), !, fail
        )
    )
    },
	add_rref(PRSIn,RefId,PRSOut).

reference(PRSIn,PRSOut) --> 
	[lemma], [Identifier],
	{
	getval(refids,Refs),
	(
		(	 member(ref(RefId,lemma(Identifier)) ,Refs), ! )
		;
		(	concat_atom(['The referenced Lemma ', Identifier, ' does not exist yet'],Message),
			PRSIn=id~Id,
			add_error_message_once(referenceError,reference,Id,Message), !, fail
		)
	)
	},
	add_rref(PRSIn,RefId,PRSOut).

reference(PRSIn,PRSOut) --> 
	[definition], [Identifier],
	{
	getval(refids,Refs),
    (
        (    member(ref(RefId,definition(Identifier)) ,Refs), ! )
        ;
        (   concat_atom(['The referenced Definition ', Identifier, ' does not exist'],Message),
            PRSIn=id~Id,
            add_error_message_once(referenceError,reference,Id,Message), !, fail
        )
    )
    },
	add_rref(PRSIn,RefId,PRSOut).

reference(PRSIn,PRSOut) --> 
	[induction],
	add_rref(PRSIn,induction,PRSOut).

% Potential update which allows references in other texts.
% reference --> [by], [axiom], identifier, [in], naproche_text_id, comma(comma~optional).
% reference --> [by], [theorem], identifier, [in], naproche_text_id, comma(comma~optional).
% reference --> [by], [lemma], identifier, [in], naproche_text_id, comma(comma~optional).


% DEFINITIONS

%%	definition_text(+In,-Out,+DCGList,-DCGList)
%
%	definition_text parses a definition possible preceeded with a sentence declaring a name for the definition (e.g. "Definition 4:").

definition_text(In,Out) -->
	[sentence(_,[definition,Identifier])],
	definition_text_core(In,Out,ReferenceId),
	{
	% Store Rref Identifier
	getval(refids,Refs),
	setval(refids,[ref(ReferenceId,definition(Identifier))|Refs])
	},
	([sentence(_,[end_definition])];[]).

definition_text(In,Out) -->
	definition_text_core(In,Out,_),
	([sentence(_,[end_definition])];[]).

%%	definition_text_core(+In,-Out,-ReferenceId,+DCGList,-DCGList)
%
%	@param ReferenceId is the PRS ID which an Rref referring to this definition should use.

% Definition texts can consist of one-sentence definitions:

definition_text_core(In,Out,definiens(Id)) -->
	[sentence(Id,Content)],
	{
	definition(In,Out,Id,Content,[]),
	!
	}.


% Currently the only multi-sentence definitions are recursive definitions of function symbols (e.g. in Naproche-Landau
% without talk about sets and functions).
% fo_induction_start and fo_induction_step are of the required form to for a recursive definition of FunctionSymbol.

definition_text_core(In,Out,definiens(Id)) --> 
	[sentence(Id,Content)],
	{
	Content = [define, math([FunctionSymbol]), recursively],
	fo_function_symbol(FunctionSymbolTree,[FunctionSymbol],[]),
	FunctionSymbolTree = name~FunctionSymbolName
	},
	[sentence(_,[math(BaseCase)])], 
	[sentence(_,[math(SuccCase)])], 
	{
	fo_induction_start(FunctionSymbolName,Number,BaseCaseTree,BaseCaseLHSVar,BaseCaseRHSVar,BaseCase,[]),
	fo_induction_step(FunctionSymbolName,Number,SuccCaseTree,SuccCaseLHSVar,SuccCaseRHSVar,SuccCase,[]),
	new_prs(domain(Id),In,DomainPRSIn,_,_),
	
	% In the base case, math_ids are created for the new variables on LHS. On RHS, there may not be new variables.
	create_math_ids(BaseCaseLHSVar,add_dref~yes,DomainPRSIn,TmpDomainPRS,_),
	no_new_var(BaseCaseRHSVar,TmpDomainPRS),
	
	% In the successor case, math_ids are created for the new variables on LHS. On RHS, there may not be new variables.
	% The use of check_var_acc/3 instead of create_math_ids/3 is a hack in order to allow the same variables as in the
	% base case to be used without them getting a new mref and math_id.
	check_var_acc(SuccCaseLHSVar,TmpDomainPRS,DomainPRSOut),
	no_new_var(SuccCaseRHSVar,DomainPRSOut)
	},
	new_prs(definiens(Id),DomainPRSOut,DefiniensPRSIn),
	add_condition(add_dref~yes,DefiniensPRSIn,math_id(BaseCaseDref,BaseCaseTree),TmpDefiniensPRS1),
	add_condition(add_dref~yes,TmpDefiniensPRS1,holds(BaseCaseDref),TmpDefiniensPRS2),
	add_condition(add_dref~yes,TmpDefiniensPRS2,math_id(SuccCaseDref,SuccCaseTree),TmpDefiniensPRS3),
	add_condition(add_dref~yes,TmpDefiniensPRS3,holds(SuccCaseDref),DefiniensPRSOut),
	add_condition(add_dref~yes,In,FunctionSymbol :: DomainPRSOut => DefiniensPRSOut,Out).


%%	definition(+In,-Out,+Id)
%
% 	Here is how to define new predicates symbols, new adjectives, new nouns and new verbs.

definition(In,Out,Id) --> 
	[define], 
	{
	In = accafter~Accessibles,
	DefiniendumPRSIn = id~Id..drefs~[]..mrefs~[]..conds~[]..rrefs~[]..accbefore~Accessibles..accafter~Accessibles
	},
	definiendum(DefiniendumPRSIn,DefiniendumPRSOut), 
	iff, 
	{
	DefiniendumPRSOut = accafter~Acc,
	DefiniensPRSIn = id~definiens(Id)..drefs~[]..mrefs~[]..conds~[]..accbefore~Acc..accafter~Acc..rrefs~[]
	},
	proposition_coord(DefiniensPRSIn,DefiniensPRSOut,mode~finite..subordinated~yes),
	{
	In = id~PRSId..mrefs~Mrefs..drefs~Drefs..rrefs~Rrefs..conds~Conds..accbefore~InAcc..accafter~AccAfter,
	Out = id~PRSId..mrefs~Mrefs..drefs~Drefs..rrefs~Rrefs..conds~[DefiniendumPRSOut := DefiniensPRSOut|Conds]..accbefore~InAcc..accafter~AccAfter
	}.


% Here is how to explicitly define new function symbols, including 0-ary (i.e. constants).
% fo_free_function(F,_,_) is defined to be any term of the form F(x1, ..., xn), where F is an n-ary function_symbol.
% fo_term_without(F,_,_) is defined to be any term not containing the function symbol F.

definition(In,Out,Id) --> 
	[define], 
	[math(FreeFunction)], 
	copula(mode~to-infinitive),
	[math(Term)], 
	{
	fo_free_function(FunctionSymbol,FreeFunctionTree,FreeVar,FreeFunction,[]),
	!,
	fo_term_without(FunctionSymbol,TermTree,TermFreeVar,Term,[]),
	!,
	new_prs(domain(Id),In,DomainPRSIn,_,_),
	create_math_ids(FreeVar,add_dref~yes,DomainPRSIn,DomainPRSOut,_),
	no_new_var(TermFreeVar,DomainPRSOut)
	},
	new_prs(definiens(Id),DomainPRSOut,DefiniensPRSIn),
	add_condition(add_dref~yes,DefiniensPRSIn,math_id(Dref,type~relation..arity~2..name~ (=)..args~[FreeFunctionTree,TermTree]),TmpDefiniensPRS),
	add_condition(add_dref~yes,TmpDefiniensPRS,holds(Dref),DefiniensPRSOut),
	add_condition(add_dref~yes,In,FunctionSymbol :: DomainPRSOut => DefiniensPRSOut,Out).


%%	definiendum(+PRSIn,-PRSOut)
%
%	Creates the left hand side of a definition condition.

definiendum(PRSIn,PRSOut) --> 
	[math(Formula)], 
	{
	fo_free_predicate_symbol(Tree,FreeVar,Formula,[]),
	!,
	create_math_ids(FreeVar,add_dref~yes,PRSIn,TmpPRS1,_)
	},
	add_condition(add_dref~yes,TmpPRS1,math_id(FormulaDref,Tree),TmpPRS2),
	add_condition(add_dref~yes,TmpPRS2,holds(FormulaDref),PRSOut).

definiendum(PRSIn,PRSOut) --> 
	[math(Var)],  
	{fo_variable(VarTree,Var,[])},
	copula(mode~to-infinitive..number~singular),
	indefinite_article(number~singular),
	!,
	{
	PRSIn = id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter,
	% Var should be a variable that is not accessible
	not_accessible(VarTree,AccAfter,Id),
	TmpPRS = id~Id..drefs~[Dref|Drefs]..mrefs~[VarTree|Mrefs]..conds~[math_id(Dref,VarTree)|Conds]..rrefs~Rrefs..accbefore~AccBefore..accafter~[math_id(Dref,VarTree)|AccAfter],
	VarTree = dref~Dref
	},
	noun(Dref,TmpPRS,PRSOut,number~singular).

definiendum(PRSIn,PRSOut) --> 
	[math(Var)],  
	{fo_variable(VarTree,Var,[])},
	copula(mode~to-infinitive..number~singular),
	{
	PRSIn = id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter,
	% Var should be a variable that is not accessible
	not_accessible(VarTree,AccAfter,Id),
	TmpPRS = id~Id..drefs~[Dref|Drefs]..mrefs~[VarTree|Mrefs]..conds~[math_id(Dref,VarTree)|Conds]..rrefs~Rrefs..accbefore~AccBefore..accafter~[math_id(Dref,VarTree)|AccAfter],
	VarTree = dref~Dref
	},
	adjective(Dref,TmpPRS,PRSOut,number~singular..adj_trans~no).

definiendum(PRSIn,PRSOut) --> 
	[math(Var1)],  
	{
	PRSIn = id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter,
	fo_variable(Var1Tree,Var1,[]),
	% Var1 should be a variable that is not accessible
	not_accessible(Var1Tree,AccAfter,Id)
	},
	copula(mode~to-infinitive..number~singular),
	adjective_parser(Adjective,number~singular..adj_trans~T),
	{ \+ T = no },
	[T],
	[math(Var2)],
	{
	fo_variable(Var2Tree,Var2,[]),
	% Var2 should be a variable that is not accessible
	not_accessible(Var2Tree,[math_id(Dref1,Var1Tree)|AccAfter],Id),
	TmpPRS = id~Id..drefs~[Dref2,Dref1|Drefs]..mrefs~[Var2Tree,Var1Tree|Mrefs]..conds~[math_id(Dref2,Var2Tree),math_id(Dref1,Var1Tree)|Conds]..rrefs~Rrefs..accbefore~AccBefore..accafter~[math_id(Dref2,Var2Tree),math_id(Dref1,Var1Tree)|AccAfter],
	Var1Tree = dref~Dref1,
	Var2Tree = dref~Dref2,
	transitive_verb_semantics([Adjective],Dref1,[Dref2],[TmpPRS],[PRSOut])
	}.

definiendum(PRSIn,PRSOut) --> 
	[math(Var1)],  
	{
	PRSIn = id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter,
	fo_variable(Var1Tree,Var1,[]),
	% Var1 should be a variable that is not accessible
	not_accessible(Var1Tree,AccAfter,Id)
	},
	[and],
	[math(Var2)],
	{
	fo_variable(Var2Tree,Var2,[]),
	% Var2 should be a variable that is not accessible
	not_accessible(Var2Tree,[math_id(Dref1,Var1Tree)|AccAfter],Id)
	},
	copula(mode~to-infinitive..number~plural),
	adjective_parser(Adjective,number~plural..adj_trans~T),
	{ 
	\+ T = no,
	TmpPRS = id~Id..drefs~[Dref2,Dref1|Drefs]..mrefs~[Var2Tree,Var1Tree|Mrefs]..conds~[math_id(Dref2,Var2Tree),math_id(Dref1,Var1Tree)|Conds]..rrefs~Rrefs..accbefore~AccBefore..accafter~[math_id(Dref2,Var2Tree),math_id(Dref1,Var1Tree)|AccAfter],
	Var1Tree = dref~Dref1,
	Var2Tree = dref~Dref2,
	transitive_verb_semantics([Adjective],Dref1,[Dref2],[TmpPRS],[PRSOut])
	}.

definiendum(PRSIn,PRSOut) --> 
	[math(Var)],  
	{fo_variable(VarTree,Var,[]),
	PRSIn = id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter,
	% Var shuold be a variable that is not accessible
	not_accessible(VarTree,AccAfter,Id),
	TmpPRS = id~Id..drefs~[Dref|Drefs]..mrefs~[VarTree|Mrefs]..conds~[math_id(Dref,VarTree)|Conds]..rrefs~Rrefs..accbefore~AccBefore..accafter~[math_id(Dref,VarTree)|AccAfter],
	VarTree = dref~Dref
	},
	intransitive_verb(Dref,TmpPRS,PRSOut,mode~to-infinitive..number~singular).

definiendum(PRSIn,PRSOut) --> 
	[math(Var1)],  
	{fo_variable(VarTree1,Var1,[]),
	PRSIn = id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter,
	% Var1 should be a variable that is not accessible
	not_accessible(VarTree1,AccAfter,Id)
	},
	transitive_verb_parser(Verb,mode~to-infinitive..number~singular),
	[math(Var2)],  
	{
	fo_variable(VarTree2,Var2,[]),
	% Var2 should be a variable that is not accessible
	not_accessible(VarTree2,[math_id(Dref1,VarTree1)|AccAfter],Id),
	TmpPRS = id~Id..drefs~[Dref2,Dref1|Drefs]..mrefs~[VarTree2,VarTree1|Mrefs]..conds~[math_id(Dref2,VarTree2),math_id(Dref1,VarTree1)|Conds]..rrefs~Rrefs..accbefore~AccBefore..accafter~[math_id(Dref2,VarTree2),math_id(Dref1,VarTree1)|AccAfter],
	VarTree1 = dref~Dref1,
	VarTree2 = dref~Dref2,
	transitive_verb_semantics([Verb],Dref1,[Dref2],[TmpPRS],[PRSOut])
	}.


% ASSUMPTIONS

%%	assumption_naproche_text(+In:PRS,-Out:PRS)
%
% 	An assumption_naproche_text starts with an assumption and ends with an assumption closing.
% 	In between there is naproche_text.

assumption_naproche_text(In,Out) -->
	[sentence(Id,Content)],
	new_prs(Id,In,AssumptionPRSIn),
	{ assumption(AssumptionPRSIn,AssumptionPRSOut,Content,[]), ! },
	new_prs(conseq(Id),AssumptionPRSOut,ConseqPRSIn),
	naproche_text(ConseqPRSIn,ConseqPRSOut,all_ass_closed~yes), 
	[sentence(ClosingId,ClosingContent)],
	new_prs(ClosingId,In,ClosingPRSIn),
	{ closing(ClosingPRSIn,ClosingPRSOut,ClosingContent,[]) },
	add_condition(_,In,AssumptionPRSOut ==> ConseqPRSOut,TmpOut),
	add_condition(_,TmpOut,ClosingPRSOut,Out).

%%	assumption_text(+In:PRS,-Out:PRS)
%
% 	An assumption_text starts with an assumption and ends with an assumption closing.
% 	In between there is text.

assumption_text(In,Out) -->
	[sentence(Id,Content)],
	new_prs(Id,In,AssumptionPRSIn),
	{ assumption(AssumptionPRSIn,AssumptionPRSOut,Content,[]), ! },
	new_prs(conseq(Id),AssumptionPRSOut,ConseqPRSIn),
	text(ConseqPRSIn,ConseqPRSOut,type~all_ass_closed), 
	[sentence(ClosingId,ClosingContent)],
	new_prs(ClosingId,In,ClosingPRSIn),
	{ closing(ClosingPRSIn,ClosingPRSOut,ClosingContent,[]) },
	add_condition(_,In,AssumptionPRSOut ==> ConseqPRSOut,TmpOut),
	add_condition(_,TmpOut,ClosingPRSOut,Out).


%%	assumption(+PRSIn,-PRSOut)
%
% 	An assumption is a proposition_coord prefixed with an assumption trigger. Additionally, an assumption can be used to introduce new variables.

assumption(PRSIn,PRSOut) -->
	trigger(type~variable_declaration),
	variable_list_bar(Dref,PRSIn,PRSOut,add_dref~yes..number~singular..dref_list~[Dref]).

assumption(PRSIn,PRSOut) -->
	trigger(type~variable_declaration),
	new_prs(prefix(plural),PRSIn,PluralPRSIn),
	variable_list_bar(Dref,PluralPRSIn,TmpPluralPRS,add_dref~yes..number~plural..dref_list~DrefList),
	add_condition(add_dref~yes,PRSIn,plural_dref(Dref,DrefList),TmpPRS),
	{add_accessible(add_dref~yes,TmpPluralPRS,plural_dref(Dref,DrefList),PluralPRSOut)},
	add_condition(_,TmpPRS,plural(Dref,PluralPRSOut),TmpPRS2),
	{dissolve_plurals(TmpPRS2,PRSOut)}.
	
assumption(PRSIn,PRSOut) --> 
	[let], 
	variable_list_bar(Dref,PRSIn,PRSOut,add_dref~yes..number~singular..dref_list~[Dref]),
	[be], [given].

assumption(PRSIn,PRSOut) --> 
	[let], 
	new_prs(prefix(plural),PRSIn,PluralPRSIn),
	variable_list_bar(Dref,PluralPRSIn,TmpPluralPRS,add_dref~yes..number~plural..dref_list~DrefList),
	add_condition(add_dref~yes,PRSIn,plural_dref(Dref,DrefList),TmpPRS),
	{add_accessible(add_dref~yes,TmpPluralPRS,plural_dref(Dref,DrefList),PluralPRSOut)},
	add_condition(_,TmpPRS,plural(Dref,PluralPRSOut),TmpPRS2),
	{dissolve_plurals(TmpPRS2,PRSOut)},
	[be], [given].

assumption(PRSIn,PRSOut) --> 
	trigger(type~ass..mode~Mode), 
	proposition_coord(PRSIn,PRSOut,mode~Mode..subordinated~yes).

%%	assumptions(+PRSIn,-PRSOut)
%
%	assumptions is a possibly empty list of assumptions.

assumptions(PRSIn,PRSOut) -->
	[sentence(Id,Content)],
	new_prs(Id,PRSIn,AssIn),
	{ assumption(AssIn,AssOut,Content,_) },
	add_condition(_,PRSIn,AssOut,TmpPRS),
	assumptions(TmpPRS,PRSOut).

assumptions(PRSIn,PRSIn) -->
	[].

%%	closing(+PRSIn,-PRSOut)
%
% 	closing starts with an assumption closing trigger.

closing(PRSIn,PRSOut) --> 
	trigger(type~ass_closing), 
	references(PRSIn,TmpPRS),
	proposition_coord(TmpPRS,TmpPRS2,mode~finite..subordinated~no),
	references(TmpPRS2,PRSOut).

% CASES

%%	cases_text(+In,-Out,?GULP)
%
% 	cases_text can either be used for listing mutually exclusive cases, or for proving something by case distinction.
% 	
%	@param: In & Out are PRSes
%	@param: GULP: type is just handed over from text to cases, if cases is triggered.
% 	
% 	Here is how mutually exclusive cases work:

cases_text(In,Out,type~_) -->
	[sentence(_,Content)],
	{ trigger(type~statement,Content,[precisely,one,of,the,following,cases,holds]), ! },
	add_condition(_,In,><([]),TmpOut),
	exclusive_cases(TmpOut,Out,'><').

cases_text(In,Out,type~_) -->
	[sentence(_,Content)],
	{ trigger(type~statement,Content,[at,most,one,of,the,following,cases,holds]), ! },
	add_condition(_,In,<>([]),TmpOut),
	exclusive_cases(TmpOut,Out,'<>').

% Here is how proofs by case distinction work:
%
% cases_text consists of cases and a case closing. It receives a CaseList from cases and adds a case distinction. 

cases_text(In,Out,type~Type) --> 
	cases(In,TmpOut,[],CaseList,type~Type..subtype~beginning),
	{
	% one of the introduced cases must hold
	add_case_distinction(CaseList,TmpOut,TmpOut2)
	},
	[sentence(Id,Content)],
	{ 
	TmpOut2 = accafter~Acc,
	ClosingPRSIn = id~Id..drefs~[]..mrefs~[]..conds~[]..rrefs~[]..accbefore~Acc..accafter~Acc,
	
	% a sentences containing 'in all cases...'
	case_closing(ClosingPRSIn,ClosingPRSOut,Content,[]), 
	!,
	ClosingPRSOut = accafter~AccAfter,
	TmpOut2 = id~Identifier..drefs~Drefs..mrefs~Mrefs..rrefs~Rrefs..conds~Conds..accbefore~AccBefore,
	Out = id~Identifier..drefs~Drefs..mrefs~Mrefs..rrefs~Rrefs..conds~[ClosingPRSOut|Conds]..accbefore~AccBefore..accafter~AccAfter
	}.

%%	exclusive_cases(+In,-Out,+Symbol)
%
%	This is a list of the mutually exclusive cases. Each of the cases can be prefixed by a sentence of the form "Case X".
%	
%	@param: In is a PRS whose last condition is an exclusive disjunction condition. At each step, one disjunct is added to it.
%	@param: Symbol is either '><', for saying 'precisely one case holds', or '<>', for saying 'at most one case holds'.
%

exclusive_cases(In,Out,Symbol) -->
	[sentence(_,[case,Identifier])],
	[sentence(SentenceId,Content)],
	new_prs(SentenceId,In,PRSIn),
	{
	proposition_coord(PRSIn,PRSOut,mode~finite..subordinated~yes,Content,[]),
	!
	},
	exclusive_cases(In,TmpOut,Symbol),
	{
	TmpOut = conds~[ExclCaseCondIn|Conds],
	ExclCaseCondIn =.. [Symbol,List],
	ExclCaseCondOut =.. [Symbol,[PRSOut|List]],
	change_feature(TmpOut,conds,[ExclCaseCondOut|Conds],Out),

	% Store Rref Identifier
	getval(refids,Refs),
	setval(refids,[ref(Content,case(Identifier))|Refs])
	}.
	
exclusive_cases(In,Out,Symbol) -->
	[sentence(SentenceId,Content)],
	new_prs(SentenceId,In,PRSIn),
	{
	proposition_coord(PRSIn,PRSOut,mode~finite..subordinated~yes,Content,[]),
	!
	},
	exclusive_cases(In,TmpOut,Symbol),
	{
	TmpOut = conds~[ExclCaseCondIn|Conds],
	ExclCaseCondIn =.. [Symbol,List],
	ExclCaseCondOut =.. [Symbol,[PRSOut|List]],
	change_feature(TmpOut,conds,[ExclCaseCondOut|Conds],Out)
	}.
	
exclusive_cases(In,In,_) --> [].


%% 	cases(+In,-Out,+CaseListIn,-CaseListOut,?GULP)
%
%	a case contains the sentence "case n: Statement" and Text. Statement and Text are both transformed into PRSes.
%	an implication condition Statement ==> Text is added to In, and Statement is added to CaseListIn.
%
%	@param In & Out are PRSes,
%	@param CaseListIn & CaseListOut are lists of PRSes.
%	@param GULP: type enters from text or cases_text and is transferred to text or cases itself. 
%		if type is "in_case", new case distinctions must be announced.
%		if type is "rest-list", announcements ("there are N cases") may not be made before the case.
%
%	Case distinctions within case distinctions are possible, here "cases_text" is called by text.

cases(In,Out,CaseListIn,CaseListOut,type~Type..subtype~SubType) -->
	case_introduction(type~Type..subtype~SubType),
	[sentence(_,[case,Identifier])],
	[sentence(CaseRestrId,Content)],
	{
	In = accafter~Accessibles,
	CaseRestrIn = id~CaseRestrId..drefs~[]..mrefs~[]..conds~[]..rrefs~[]..accbefore~Accessibles..accafter~Accessibles,
	proposition_coord(CaseRestrIn,CaseRestrOut,mode~finite..subordinated~yes,Content,[]),
	!,

	% Store Rref Identifier
	getval(refids,Refs),
	setval(refids,[ref(Content,case(Identifier))|Refs]),
	
	CaseRestrOut = accafter~Acc,
	CaseScopeIn = id~text_case(Identifier)..accbefore~Acc..drefs~[]..mrefs~[]..conds~[]..rrefs~[]..accafter~Acc
	},
	text(CaseScopeIn,CaseScopeOut,type~in_case),
	!,
	{
	In = conds~Conds..id~Id..drefs~Drefs..mrefs~Mrefs..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter,
	TmpOut = conds~[CaseRestrOut ==> CaseScopeOut|Conds]..id~Id..drefs~Drefs..mrefs~Mrefs..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter,
	!,
	expand_id(case_disjunct,CaseRestrOut,HeadCase)
	},
	cases(TmpOut,Out,[HeadCase|CaseListIn],CaseListOut,type~Type..subtype~rest-list).

cases(In,In,CaseListIn,CaseListIn,subtype~rest-list) --> [].

% case_introduction contains either a sentence saying "there are N cases" (N>=2) or nothing. if type is 'in_case', this sentence is obligatory. on the other hand, if we're just presenting the second case, the sentence isn't permitted.

case_introduction(subtype~SubType) --> 
	{ \+ SubType = rest-list },
	[sentence(_,Content)],
	{
	trigger(type~statement,Content,[there,are,Number,cases]),
	number(Number,[Number],[])
	}.

case_introduction(type~Type) --> 
	{ \+ Type = in_case }, 
	[].

% even within a case, if a subcase A  has been introduced, subcase B must not be prefixed by an introductory sentence.

case_introduction(subtype~rest-list) --> [].


% case_closing starts with a case_closing_trigger

case_closing(PRSIn,PRSOut) --> 
	trigger(type~statement), 
	trigger(type~case_closing), 
	proposition_coord(PRSIn,PRSOut,mode~finite..subordinated~no).


% AXIOMS, LEMMAS AND THEOREMS


%%	axiom(+Acc,-AxiomPRS,-ConseqPRSIn)
%
% 	An axiom consists of a heading followed by text.
%
%	@param AxiomPRS is the PRS representing the content of the axiom
%	@param ConseqPRSIn is the frame for the to-be-build PRS containing the consequence of the axiom (i.e. the remaining text).

axiom(Acc,AxiomPRSOut,ConseqPRSIn) --> 
	[sentence(Id,[axiom])],
	[sentence(_,Content)],
	{
	AxiomPRSIn = id~axiom(Id)..drefs~[]..mrefs~[]..conds~[]..rrefs~[]..accbefore~Acc..accafter~Acc,
	proposition_coord(AxiomPRSIn,AxiomPRSOut,mode~finite..subordinated~yes,Content,[]),
	!,
	AxiomPRSOut = accafter~AccAfter,
	ConseqPRSIn = id~conseq(axiom(Id))..drefs~[]..mrefs~[]..conds~[]..rrefs~[]..accbefore~AccAfter..accafter~AccAfter
	},
	([sentence(_,[end_axiom])];[]).

axiom(Acc,AxiomPRSOut,ConseqPRSIn) --> 
	[sentence(Id,[axiom,Identifier])],
	[sentence(_,Content)],
	{
	AxiomPRSIn = id~axiom(Id)..drefs~[]..mrefs~[]..conds~[]..rrefs~[]..accbefore~Acc..accafter~Acc,
	proposition_coord(AxiomPRSIn,AxiomPRSOut,mode~finite..subordinated~yes,Content,[]),
	!,
	AxiomPRSOut = accafter~AccAfter,
	ConseqPRSIn = id~conseq(axiom(Id))..drefs~[]..mrefs~[]..conds~[]..rrefs~[]..accbefore~AccAfter..accafter~AccAfter,

	%Store Rref Identifier
	getval(refids,Refs),
	setval(refids,[ref(axiom(Id),axiom(Identifier))|Refs])
	}.


%%  theorem(+In,-Out,-TheoremType)
%
%   A theorem consists of a heading, a goal text, the marker "Proof", a body text (which possibly includes lemmas) and the marker "Qed".

theorem(In,Out,TheoremType) -->
    [sentence(Id,Heading)],
    {
	heading(Id,TheoremType,Heading,[])
	},
	new_prs(ass(Id),In,AssIn),
	assumptions(AssIn,AssOut),
	new_prs(goal(Id),AssOut,GoalIn),
    text(GoalIn,GoalOut,type~no_ass..empty~no),
	goal_end(TheoremType),
    [sentence(ProofId,[proof])],
	new_prs(proof(ProofId),AssOut,ProofIn),
	{
	TheoremType = theorem -> ProofTextType = theorem ; ProofTextType = normal
	},
    text(ProofIn,ProofOut,type~ProofTextType..empty~no),
    ([sentence(_,[qed])];[sentence(_,[end_proof])]),
    !,
    {
	TheoremCond = theorem(TheoremType,GoalOut,ProofOut),
	( 
		AssIn = AssOut
		->
		add_condition(_,In,TheoremCond,Out,_,_)
		;
		(
			new_prs(conseq(Id),In,ConseqPRSIn,_,_),
			add_condition(_,ConseqPRSIn,TheoremCond,ConseqPRSOut,_,_),
			add_condition(_,In,AssOut ==> ConseqPRSOut,Out,_,_)
		)
	)
	}.


heading(_,TheoremType) -->
	[TheoremType],
	{ TheoremType = theorem ; TheoremType = lemma }.

heading(Id,TheoremType) -->
	[TheoremType],
	{ TheoremType = theorem ; TheoremType = lemma },
	[Identifier],
	{
	Ref =.. [TheoremType,Identifier],
    % Store Rref Identifier
    getval(refids,Refs),
    setval(refids,[ref(goal(Id),Ref)|Refs])
	}.


goal_end(TheoremType) -->
	{ atom_concat(end_,TheoremType,GoalEnd) },
	[sentence(_,[GoalEnd])].

goal_end(_) -->
	[].


%=====================
% proposition_coord
%=====================

%%	proposition_cord(+PRSIn,-PRSOut,+GULP)
%
% 	proposition_coord can be either a simple sentence_coord, or a number of sentence_coords linked with "if...then" or "iff".
%
% 	@param GULP indicates whether the GULP feature 'subordinated' has value "yes" or "no", and whether the GULP feature 
%	'mode' has value "finite", "infinitive" or "that". 
% 	"finite" is the normal case. "infinitive" inidicates that the verb phrase has to be infinitive. 
% 	"that" indicates that the sentence has to be prefixed with "that".

proposition_coord(PRSIn,PRSOut,mode~Mode..subordinated~S1) -->
	{ var(S1); S1 = no; S2 = yes },
	new_prs(copy,PRSIn,PRS1In),
	sentence_coord(PRS1In,PRS1Out,mode~Mode..subordinated~S2),
	proposition_coord_tail(PRS1Out,PRSIn,PRSOut,S1,S2).

proposition_coord(PRSIn,PRSOut,mode~finite..subordinated~Sub) --> 
	[if], 
	new_prs(prefix(prot),PRSIn,LeftPRSIn),
	sentence_coord(LeftPRSIn,LeftPRSOut,mode~finite..subordinated~yes), 
	comma(comma~optional),
	[then],
	new_prs(new_prefix(apod),LeftPRSOut,RightPRSIn),
	references(RightPRSIn,TmpRightPRS1),
	trigger(type~conseq),
	references(TmpRightPRS1,TmpRightPRS2),
	proposition_coord(TmpRightPRS2,RightPRSOut,mode~finite..subordinated~Sub),
	add_condition(_,PRSIn,LeftPRSOut => RightPRSOut,PRSOut).

proposition_coord(PRSIn,PRSOut,mode~that..subordinated~Sub) --> 
	[that],
	[if], 
	new_prs(prefix(prot),PRSIn,LeftPRSIn),
	sentence_coord(LeftPRSIn,LeftPRSOut,mode~finite..subordinated~yes), 
	comma(comma~optional),
	[then],
	new_prs(new_prefix(apod),LeftPRSOut,RightPRSIn),
	references(RightPRSIn,TmpRightPRS1),
	trigger(type~conseq),
	references(TmpRightPRS1,TmpRightPRS2),
	proposition_coord(TmpRightPRS2,RightPRSOut,mode~finite..subordinated~Sub),
	add_condition(_,PRSIn,LeftPRSOut => RightPRSOut,PRSOut).

proposition_coord_tail(PRS1In,PRSIn,PRSOut,S,S) -->
	comma(comma~optional),
	[if],
	new_prs(prefix(prot),PRS1In,PRS2In),
	sentence_coord(PRS2In,PRS2Out,mode~finite..subordinated~yes),
	{expand_id(apod,PRS1In,PRS1Out)},
	make_conditional_semantics(reversed_conditional,PRSIn,PRS1Out,PRS2Out,PRSOut).
	
proposition_coord_tail(PRS1In,PRSIn,PRSOut,_,yes) -->
	comma(comma~optional),
	iff,
	new_prs(prefix(right_cond),PRS1In,PRS2In),
	sentence_coord(PRS2In,PRS2Out,mode~finite..subordinated~yes),
	{expand_id(left_cond,PRS1In,PRS1Out)},
	make_conditional_semantics(biconditional,PRSIn,PRS1Out,PRS2Out,PRSOut).

proposition_coord_tail(PRS1,PRSIn,PRSOut,S,S) -->
	[],
	{ prs_conjunction(PRSIn,PRS1,_,PRSOut) }.
	

%=====================
% sentence_coord
%=====================

%%	sentence_coord(+PRSIn,-PRSOut,+GULP)
%
%	sentence_coord works like in Attempto (with "i.e." added). It links a number of topicalised_sentences with
% 	"and", "or", ", and", ", or" and "i.e." in such a way that the bracketing is unambiguous.
%
% 	@param GULP indicates whether the GULP feature 'mode' has value "finite", "infinitive" or "that", whether 
%	the GULP feature "subordinated" has value "yes" (which disallows reasoning with "i.e." and references) or 
%	"no".

sentence_coord(PRSIn,PRSOut,mode~Mode..subordinated~S) --> 
	% The ID of PRS1 will be prefixed with "ie_conjunct1"; but its sub-PRSs should not have
	% this prefix, so we only add it after the PRS has be constructed.
	new_prs(copy,PRSIn,PRS1In),
	sentence_coord_0(PRS1In,TmpPRS1,mode~Mode), 
	new_prs(prefix(conseq_conjunct2),TmpPRS1,PRS2In),
	sentence_coord_tail(TmpPRS1,PRS1OutWithSimpleID,PRS2In,PRS2Out,mode~Mode..subordinated~S),
	{
	% Here we add the prefix "ie_conjunct1" to the ID of first PRS; see the comment above.
	expand_id(conseq_conjunct1,PRS1OutWithSimpleID,PRS1Out),
	prs_conjunction(PRSIn,PRS1Out,PRS2Out,PRSOut)
	}.

sentence_coord_tail(PRS1In,PRS1Out,PRS2In,PRS2Out,mode~Mode..subordinated~no) --> 
	references(PRS1In,PRS1Out),
	conseq_conjunct_marker,
	references(PRS2In,TmpPRS2),
	sentence_coord(TmpPRS2,PRS2Out,mode~Mode).

sentence_coord_tail(PRS1In,PRS1Out,PRS2In,PRS2Out,mode~Mode..subordinated~no) --> 
	references(PRS1In,PRS1Out),
	comma(comma~optional),
	[and],
	necessary_references(PRS2In,TmpPRS2),
	sentence_coord(TmpPRS2,PRS2Out,mode~Mode).

sentence_coord_tail(PRS1In,PRS1Out,PRS2In,PRS2Out,mode~Mode..subordinated~no) --> 
	necessary_references(PRS1In,PRS1Out),
	comma(comma~optional),
	[and],
	sentence_coord(PRS2In,PRS2Out,mode~Mode).

sentence_coord_tail(PRS1In,PRS1In,PRS2In,PRS2In,_) --> 
	[].



sentence_coord_0(PRSIn,PRSOut,mode~Mode) --> 
	% The ID of the left PRS will be prefixed with "comma_disjunct1"; but its sub-PRSs should not have
	% this prefix, so we only add it after the PRS has be constructed.
	new_prs(copy,PRSIn,LeftPRSIn),
	sentence_coord_1(LeftPRSIn,LeftPRSOutWithSimpleID,mode~Mode),
	new_prs(prefix(comma_disjunct2),PRSIn,RightPRSIn), 
	sentence_coord_0_tail(RightPRSIn,RightPRSOut,mode~Mode),
	{
	% Here we add the prefix "comma_disjunct1" to the ID of left PRS; see the comment above.
	expand_id(comma_disjunct1,LeftPRSOutWithSimpleID,LeftPRSOut),
	prs_disjunction(PRSIn,LeftPRSOut,RightPRSOut,PRSOut)
	}.

sentence_coord_0_tail(PRSIn,PRSOut,mode~Mode) --> 
	[','], [or], 
	sentence_coord_0(PRSIn,PRSOut,mode~Mode).

sentence_coord_0_tail(PRSIn,PRSIn,mode~_) --> 
	[].




sentence_coord_1(PRSIn,PRSOut,mode~Mode) --> 
	% The ID of PRS1 will receive a prefix; but its sub-PRSs should not have
	% this prefix, so we only add it after the PRS has be constructed.
	new_prs(copy,PRSIn,PRS1In),
	sentence_coord_2(PRS1In,PRS1OutWithSimpleID,mode~Mode),
	new_prs(prefix(comma_conjunct2),PRS1OutWithSimpleID,PRS2In), 
	sentence_coord_1_tail(PRS2In,PRS2Out,mode~Mode),
	{
	% Here we add the prefix to the ID of first PRS; see the comment above.
	expand_id(comma_conjunct1,PRS1OutWithSimpleID,PRS1Out),
	prs_conjunction(PRSIn,PRS1Out,PRS2Out,PRSOut)
	}.

sentence_coord_1_tail(PRS2In,PRS2Out,mode~Mode) --> 
	[','], 
	trigger(type~conjunction),
	sentence_coord_1(PRS2In,PRS2Out,mode~Mode).

sentence_coord_1_tail(PRS2In,PRS2In,mode~_) --> 
	[].




sentence_coord_2(PRSIn,PRSOut,mode~Mode) -->
	% The ID of the left PRS will be prefixed with "disjunct1"; but its sub-PRSs should not have
	% this prefix, so we only add it after the PRS has be constructed.
	new_prs(copy,PRSIn,LeftPRSIn), 
	sentence_coord_3(LeftPRSIn,LeftPRSOutWithSimpleID,mode~Mode),
	new_prs(prefix(disjunct2),PRSIn,RightPRSIn), 
	sentence_coord_2_tail(RightPRSIn,RightPRSOut,mode~Mode),
	{
	% Here we add the prefix "disjunct1" to the left PRS; see the comment above.
	expand_id(disjunct1,LeftPRSOutWithSimpleID,LeftPRSOut),
	prs_disjunction(PRSIn,LeftPRSOut,RightPRSOut,PRSOut)
	}.

sentence_coord_2_tail(PRSIn,PRSOut,mode~Mode) --> 
	[or], 
	sentence_coord_2(PRSIn,PRSOut,mode~Mode).

sentence_coord_2_tail(PRSIn,PRSIn,mode~_) --> 
	[].





sentence_coord_3(PRSIn,PRSOut,mode~that) --> 
	!,
	[that],
	% The ID of PRS1 will receive a prefix; but its sub-PRSs should not have
	% this prefix, so we only add it after the PRS has be constructed.
	new_prs(copy,PRSIn,PRS1In),	
	topicalised_sentence(PRS1In,PRS1OutWithSimpleID,mode~finite),
	new_prs(prefix(conjunct2),PRS1OutWithSimpleID,PRS2In), 
	sentence_coord_3_tail(PRS2In,PRS2Out,mode~that),
	{
	% Here we add the prefix to the ID of first PRS; see the comment above.
	expand_id(conjunct1,PRS1OutWithSimpleID,PRS1Out),
	prs_conjunction(PRSIn,PRS1Out,PRS2Out,PRSOut)
	}.

sentence_coord_3(PRSIn,PRSOut,mode~Mode) --> 
	% The ID of PRS1 will receive a prefix; but its sub-PRSs should not have
	% this prefix, so we only add it after the PRS has be constructed.
	new_prs(copy,PRSIn,PRS1In),	
	topicalised_sentence(PRS1In,PRS1OutWithSimpleID,mode~Mode),
	new_prs(prefix(conjunct2),PRS1OutWithSimpleID,PRS2In), 
	sentence_coord_3_tail(PRS2In,PRS2Out,mode~Mode),
	{
	% Here we add the prefix to the ID of first PRS; see the comment above.
	expand_id(conjunct1,PRS1OutWithSimpleID,PRS1Out),
	prs_conjunction(PRSIn,PRS1Out,PRS2Out,PRSOut)
	}.

sentence_coord_3_tail(PRS2In,PRS2Out,mode~Mode) -->
	trigger(type~conjunction_or_comma),
	sentence_coord_3(PRS2In,PRS2Out,mode~Mode).

sentence_coord_3_tail(PRS2In,PRS2In,mode~_) --> 
	[].


%%	topicalised_sentence(+PRSIn,-PRSOut,+GULP)
%
% 	A topicalised_sentence can be a quantified statement, two composite_sentences linked with "implies that", or just one composite_sentence.
%
% 	@param GULP indicates whether the GULP feature 'mode' has value "finite" or "infinitive". 

topicalised_sentence(PRSIn,PRSOut,mode~Mode) -->
	existential_topic(PRSIn,PRSOut,mode~Mode).

topicalised_sentence(PRSIn,PRSOut,mode~finite) --> 
	universal_topic(PRSIn,PRSOut,ScopePRSIn,ScopePRSOut),
	{change_id(new_prefix(scope),ScopePRSIn,TmpScopePRS)},
	comma(comma~optional),
	proposition_coord(TmpScopePRS,ScopePRSOut,mode~finite..subordinated~no).

topicalised_sentence(PRSIn,PRSOut,mode~finite) --> 
	rest_list(List),
	% For improving runtime, we only enter this case if the word "implies" can be found in the sentence.
	{ member(implies,List) },
	new_prs(prefix(prot),PRSIn,AntecedentPRSIn),
	composite_sentence(AntecedentPRSIn,AntecedentPRSOut,mode~finite), 
	comma(comma~optional),
	implies,
	new_prs(new_prefix(apod),AntecedentPRSOut,ConsequentPRSIn),
	composite_sentence(ConsequentPRSIn,ConsequentPRSOut,mode~finite),
	add_condition(_,PRSIn,AntecedentPRSOut => ConsequentPRSOut,PRSOut).

topicalised_sentence(PRSIn,PRSOut,mode~Mode) --> 
	composite_sentence(PRSIn,PRSOut,mode~Mode).


%%	existential_topic(+PRSIn,-PRSOut,GULP)
%
% 	An existential_topic is a sentence with a natural language existential quantification.
%
% 	@param GULP indicates whether the GULP feature 'mode' has value "finite" or "infinitive". 

existential_topic(PRSIn,PRSOut,Features) -->
	{ Features = type~existential },
	quantifier(Features), 
	np(_,PRSIn,PRSOut,Scope,Scope,Features),
	{ Features = specifier_type~indefinite ; Features = specifier_type~negative }.

	
%%	universal_topic(+PRSIn,-PRSOut,-ScopeIn,-ScopeOut)
%
% 	A universal_topic is a natural language universal quantifier (e.g. "for every") followed by an nbar.

universal_topic(PRSIn,PRSOut,ScopeIn,ScopeOut) --> 
	[for],
	np(_,PRSIn,PRSOut,ScopeIn,ScopeOut,specifier_type~universal).

%%	composite_sentence(+PRSIn,-PRSOut,+GULP)
%
% 	A composite_sentence is either a simple sentence, a formula or a proposition_coord prefixed with a sentence_init.
% 	By the constructions that contain composite_sentence, it is given that the "mode" feature can here only take the
% 	values "finite" and "infinitive" (not "that").
%
% 	@param GULP indicates whether the GULP feature 'mode' has value "finite" or "infinitive". 

composite_sentence(PRSIn,PRSOut,mode~Mode) --> 
	sentence(PRSIn,PRSOut,mode~Mode).

composite_sentence(PRSIn,PRSOut,mode~Mode) --> 
	sentence_init(PRSIn,PRSOut,ScopeIn,ScopeOut,mode~Mode), 
	proposition_coord(ScopeIn,ScopeOut,mode~that..subordinated~yes).

composite_sentence(PRSIn,PRSOut,mode~Mode) --> 
	trigger(type~formula..mode~Mode),
	[math(Formula)], 
	{
	PRSIn = id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links,
	fo_formula(FormulaTree,_,Formula,[]),
	% If Formula is already accessible, we don't need a new Dref
	% and all the FreeVar are also already accessible.
	member_subsumes(math_id(FormulaDref,FormulaTree),AccAfter),
	!,
	PRSOut = id~Id..drefs~Drefs..mrefs~Mrefs..conds~[holds(FormulaDref)|Conds]..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter..dref_cond_links~Links
	}.

composite_sentence(PRSIn,PRSOut,mode~Mode) -->
	trigger(type~formula..mode~Mode),
	[math(Formula)], 
	{
	fo_formula(FormulaTree,FreeVar,Formula,[]),
	!,
	% Formula is not accessible. We check all the Free Vars for accessibility. 
	check_var_acc(FreeVar,PRSIn,TmpPRS1),
	add_condition(add_dref~yes,TmpPRS1,math_id(FormulaDref,FormulaTree),TmpPRS2,_,_),
	add_condition(add_dref~yes,TmpPRS2,holds(FormulaDref),TmpPRS3,_,_),
	remember_dref_math_id_links(PRSIn,TmpPRS3,PRSOut)
	}.

composite_sentence(PRSIn,PRSOut,mode~Mode) -->
	metasentence(PRSIn,PRSOut,mode~Mode).


%%	sentence(+PRSIn,-PRSOut,+Mode)
%
% 	A sentence is just a noun phrase followed by a verb phrase.  
%
%	@param Mode is a GULP feature of the form mode~finite or mode~infinitive.

sentence(PRSIn,PRSOut,mode~Mode) -->
	np_coord(DrefList,PRSIn,TmpPRS,ScopeInList,ScopeOutList,number~Number,_),
	vp_working_on_np_coord(DrefList,ScopeInList,ScopeOutList,mode~Mode..number~Number),
	{ call((merge(TmpPRS,PRSOut), !)) }.


%=====================
% Noun phrases
%=====================

%%	np_coord(-DrefList,+PRSIn,-PRSOut,-ScopeInList,-ScopeOutList,-Features,-Connective)
%
% 	An np_coord is a coordination of NPs with "and" or "or". All but the last "and" can also be replaced by commas.
%
%	@param DrefList is a list of the Drefs introduced by the NPs in the NP coordination, possibly marked with a "plural" prefix.
%	@param ScopeInList is a list of PRSs produced by the NP, and contains - for every NP in the coordination - the corresponding input to the VP.
%	@param Features is a GULP term indicating whether the feature "number" is "singular" or "plural".
%	@param Connective indicates whether "and" or "or" is used for coordination. If the coordination consists of only one coord, then the connective is "no".

np_coord([plural(PluralDref)],PRSIn,PRSOut,[ScopeIn],[PRSOut],number~plural,and) -->
	rest_list(List),
	% For improving runtime, we only enter this case if the word "and" can be found in the sentence after the first word.
	{ member(and,List) },
	simple_np_conjunction(DrefList,PRSIn,TmpPRS),
	add_condition(add_dref~yes,TmpPRS,plural_dref(PluralDref,DrefList),ScopeIn).

np_coord([MarkedDref|DrefList],PRSIn,PRSOut,[ScopeIn|ScopeInList],[ScopeOut|ScopeOutList],number~plural,and) -->
	rest_list(List),
	% For improving runtime, we only enter this case if the word "and" can be found in the sentence after the first word.
	{ member(and,List) },
	new_prs(copy,PRSIn,PRS1In),
	local_cut(np(Dref,PRS1In,PRS1Out,ScopeIn,ScopeOut,number~Number)),
	{ mark_plural_dref(Dref,number~Number,MarkedDref) },
	comma_or_and(obligatory),
	{
	% We do not use new_prs/5 here, because of the very special accessibility handling here.
	( var(PRS1Out) -> ScopeIn = accafter~Acc; PRS1Out = accafter~Acc),
	PRSIn = id~Id,
	PRS2In = id~and(Id)..drefs~[]..mrefs~[]..conds~[]..rrefs~[]..accbefore~Acc..accafter~Acc..dref_cond_links~[]
	},
	np_coord(DrefList,PRS2In,PRS2Out,ScopeInList,ScopeOutList,_,and),
	add_condition(_,PRSIn,merge(PRS1Out,PRS2Out),PRSOut).

np_coord([MarkedDref1,MarkedDref2],PRSIn,PRSOut,[ScopeIn1,ScopeIn2],[ScopeOut1,ScopeOut2],number~plural,and) -->
	rest_list(List),
	% For improving runtime, we only enter this case if the word "and" can be found in the sentence after the first word.
	{ member(and,List) },
	new_prs(copy,PRSIn,PRS1In),
	local_cut(np(Dref1,PRS1In,PRS1Out,ScopeIn1,ScopeOut1,number~Number1)),
	{ mark_plural_dref(Dref1,number~Number1,MarkedDref1) },
	[and],
	{ 
	% We do not use new_prs/5 here, because of the very special accessibility handling here.
	( var(PRS1Out) -> ScopeIn1 = accafter~Acc; PRS1Out = accafter~Acc ),
	PRSIn = id~Id,
	PRS2In = id~and(Id)..drefs~[]..mrefs~[]..conds~[]..rrefs~[]..accbefore~Acc..accafter~Acc..dref_cond_links~[]
	},
	np(Dref2,PRS2In,PRS2Out,ScopeIn2,ScopeOut2,number~Number2),
	{ mark_plural_dref(Dref2,number~Number2,MarkedDref2) },
	{ 
	% We do not use add_condition/5 here, because of the very special accessibility handling here.
	( var(PRS2Out) -> ScopeIn2 = accafter~AccAfterOut; PRS2Out = accafter~AccAfterOut ),
	PRSIn = id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..dref_cond_links~Links,
	PRSOut = id~Id..drefs~Drefs..mrefs~Mrefs..conds~[merge(PRS1Out,PRS2Out)|Conds]..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfterOut..dref_cond_links~Links
	}.

np_coord([MarkedDref|DrefList],PRSIn,PRSOut,[ScopeIn|ScopeInList],[ScopeOut|ScopeOutList],number~CoordNumber,or) -->
	rest_list(List),
	% For improving runtime, we only enter this case if the word "or" can be found in the sentence after the first word.
	{ member(or,List) },
	new_prs(prefix(disjunct1),PRSIn,LeftPRSIn),
	local_cut(np(Dref,LeftPRSIn,LeftPRSOut,ScopeIn,ScopeOut,number~Number1)),
	{ mark_plural_dref(Dref,number~Number1,MarkedDref) },
	[or],
	new_prs(prefix(disjunct2),PRSIn,RightPRSIn),
	np_coord(DrefList,RightPRSIn,RightPRSOut,ScopeInList,ScopeOutList,number~Number2,or),
	add_condition(_,PRSIn,LeftPRSOut v RightPRSOut,PRSOut),
	{ disjunct_number(Number1,Number2,CoordNumber) }.

np_coord([MarkedDref1,MarkedDref2],PRSIn,PRSOut,[ScopeIn1,ScopeIn2],[ScopeOut1,ScopeOut2],number~CoordNumber,or) -->
	rest_list(List),
	% For improving runtime, we only enter this case if the word "or" can be found in the sentence after the first word.
	{ member(or,List) },
	new_prs(prefix(disjunct1),PRSIn,LeftPRSIn),
	local_cut(np(Dref1,LeftPRSIn,LeftPRSOut,ScopeIn1,ScopeOut1,number~Number1)),
	{ mark_plural_dref(Dref1,number~Number1,MarkedDref1) },
	[or],
	new_prs(prefix(disjunct2),PRSIn,RightPRSIn),
	np(Dref2,RightPRSIn,RightPRSOut,ScopeIn2,ScopeOut2,number~Number2),
	{ mark_plural_dref(Dref2,number~Number2,MarkedDref2) },
	add_condition(_,PRSIn,LeftPRSOut v RightPRSOut,PRSOut),
	{ disjunct_number(Number1,Number2,CoordNumber) }.

np_coord([MarkedDref],PRSIn,PRSOut,[ScopeIn],[ScopeOut],Features,no) -->
	np(Dref,PRSIn,PRSOut,ScopeIn,ScopeOut,Features),
	{ mark_plural_dref(Dref,Features,MarkedDref) }.


%%  simple_np_conjunction(-DrefList,+PRSIn,-PRSOut,+ListIn,-ListOut)
%
%   This predicate parses a conjunction of simple NPs, i.e. NPs without universal or negative specifier. 

simple_np_conjunction([Dref|DrefList],PRSIn,PRSOut) -->
	local_cut(np(Dref,PRSIn,TmpPRS,Scope,Scope,specifier_type~SpecifierType)),
	{
	\+ SpecifierType = universal,
	\+ SpecifierType = negative
	},
	comma_or_and(obligatory),
	simple_np_conjunction(DrefList,TmpPRS,PRSOut).
	
simple_np_conjunction([Dref1,Dref2],PRSIn,PRSOut) -->
	local_cut(np(Dref1,PRSIn,TmpPRS,Scope1,Scope1,specifier_type~SpecifierType1)),
	{ 
	\+ SpecifierType1 = universal,
	\+ SpecifierType1 = negative
	},
	[and],
	np(Dref2,TmpPRS,PRSOut,Scope2,Scope2,specifier_type~SpecifierType2),
	{ 
	\+ SpecifierType2 = universal,
	\+ SpecifierType2 = negative
	}.
	

%%	np(-Dref,+PRSIn,-PRSOut,-ScopeIn,-ScopeOut,-GULP)
%
% 	An np is either a specifier followed by an nbar, a pronoun or a term.  
%
%	@param Dref is the dref introduced by the noun phrase.
%	@param ScopeIn is produced by the NP to be given as input to the VP in its scope.
%	@param ScopeOut is the output produced  by the VP in the scope of this NP.
%	@param GULP is a GULP term, which indicates whether the feature "number" is "singular" or "plural", and whether the feature "specifier_type" is "term", "indefinite", "universal", "negative" or "definite".

np(Dref,PRSIn,PRSOut,ScopeIn,PRSOut,number~singular..specifier_type~term) -->
	term(Dref,PRSIn,TmpScopeIn),
	{remember_dref_cond_links(PRSIn,TmpScopeIn,ScopeIn)}.

np(Dref,PRSIn,PRSOut,ScopeIn,ScopeOut,Features) -->
	% Because of the different semantic handling, we need seperate clauses for singular and plural NPs.
	{Features = number~singular..dref_list~[Dref]},
	specifier(Dref,Features,PRSIn,PRSOut,RestrIn,RestrOut,ScopeIn,ScopeOut),
	{Features = specifier_type~definite -> Features = add_dref~no; Features = add_dref~yes},
	nbar(Dref,RestrIn,TmpRestr,Features),
	{
	( 
		\+ Features = specifier_type~indefinite, RestrOut = TmpRestr
		;
		remember_dref_cond_links(RestrIn,TmpRestr,RestrOut)
	)
	}.

np(Dref,PRSIn,PRSOut,ScopeIn,ScopeOut,Features) -->
	{Features = number~plural},
	rest_list(List1),
	specifier(Dref,Features,PRSIn,PRSOut,RestrIn,RestrOut,ScopeIn,ScopeOut),
	rest_list(List2),
	{
	% ( Features = specifier_type~definite -> AddDref = no; AddDref = yes ),
	Features = add_dref~yes
	},
	new_prs(prefix(plural),RestrIn,PluralPRSIn),
	nbar(Dref,PluralPRSIn,TmpPluralPRS,Features),
	{
	% A single variable without a noun in front of it should never be read as a plural nbar with empty
	% indefinite specifier, but only as a term:
	( List1 = List2, subsumes(dref_list~[_],Features) -> Features = noun~obligatory; true ),
	% If the NP contains a variable list, this variable list has a dref list listed in Features, and we need
	% a plural_dref condition linking the plural dref to this dref list:
	(
		subsumes(dref_list~DrefList,Features)
		->
		( add_condition(add_dref~yes,RestrIn,plural_dref(Dref,DrefList),TmpRestr,_,_), add_accessible(add_dref~yes,TmpPluralPRS,plural_dref(Dref,DrefList),PluralPRSOut) )
		;
		( TmpRestr = RestrIn, PluralPRSOut = TmpPluralPRS)
	)
	},
	add_condition(_,TmpRestr,plural(Dref,PluralPRSOut),TmpRestr2),
	{
	dissolve_plurals(TmpRestr2,TmpRestr3),
	(  Features = specifier_type~indefinite -> remember_dref_cond_links(RestrIn,TmpRestr3,RestrOut) ; RestrOut = TmpRestr3 )
	}.

% This should be included at a later point:
% np --> pronoun.


%%	term(-Dref,+PRSIn,-PRSOut)
%
%	@param Dref is the main Dref introduced by the term.

% We have to distinguish three cases:
% In the first case, the term is already accessible:

term(Dref,PRSIn,PRSOut) -->
	[math(Term)],
	{
	PRSIn = accafter~AccAfter,
	fo_term(TermTree,_,Term,[]),
	member_subsumes(math_id(Dref,TermTree),AccAfter),
	!,
	PRSOut = PRSIn
	}.

% In the second case, the term is not accessible, but is just a variable (this case needs to be treated separately
% so that we don't have mrefs and math_ids for both the variable and the term that just is that variable):

term(Dref,PRSIn,PRSOut) -->
	[math(Term)], 
	{
	fo_term(TermTree,_,Term,[]),
	TermTree = type~variable..dref~Dref,
	!
	},
	add_condition(add_dref~yes,PRSIn,math_id(Dref,TermTree),PRSOut).

%	PRSOut = id~Id..drefs~[Dref|Drefs]..mrefs~[TermTree|Mrefs]..conds~[math_id(Dref,TermTree)|Conds]..rrefs~Rrefs..accbefore~AccBefore..accafter~[math_id(Dref,TermTree)|AccAfter],
%	TermTree = dref~Dref
%	}.

% In the third case, the term is not accessible and is not a variable:

term(Dref,PRSIn,PRSOut) --> 
	[math(Term)], 
	{
	fo_term(TermTree,FreeVar,Term,[]),
	!,
	check_var_acc(FreeVar,PRSIn,TmpPRS1),
	add_condition(add_dref~yes,TmpPRS1,math_id(Dref,TermTree),PRSOut,_,_)
	}.
	

%%	nbar(-Dref,+PRSIn,-PRSOut,?Features)
%
% 	An nbar is a noun, possibly preceeded by an adjective, and possibly followed by a variablebar. 
% 	Alternitavely, it is just a variablebar possibly preceeded by an adjective.
%
%	@param Features is a GULP term indicating 
%		1. whether the feature "number" is "singular" or "plural",
%		2. whether "add_dref" is "yes" or "no" (in the first case the nbar1 necessarily introduces a Dref, in the second case it might not introduce any),
%		3. whether the feature "noun" has value "obligatory" or "optional".
%	Additionally Features gets the information "adj_trans~no" if an adjective is parsed at the beginning of the nbar.

nbar(Dref,PRSIn,PRSOut,Features) -->
	nbar1(Dref,PRSIn,PRSOut,Features).

nbar(Dref,PRSIn,PRSOut,Features) -->
	{Features = adj_trans~no},
	adjective(Dref,PRSIn,TmpPRS,Features),
	nbar(Dref,TmpPRS,PRSOut,Features). 

%%	nbar1(?Dref,+PRSIn,-PRSOut,?Features)
%
% 	An nbar is a noun, possibly followed by a variablebar, or just a variablebar.
%
%	@param Features is a GULP term indicating 
%		1. whether the feature "number" is "singular" or "plural", 
%		2. whether "add_dref" is "yes" or "no" (in the first case the nbar1 necessarily introduces a Dref, in the second case it might not introduce any),
%		3. what the noun_type of the noun in the nbar1 is, 
%		4. of what type the specifier is, in whose scope the nbar is contained
%		5. whether the feature "noun" has value "obligatory" or "optional".

% First we look whether the nbar1 is of the form "collection of" + indefinite plural NP:
nbar1(Dref,PRSIn,PRSOut,Features) -->
	{Features = noun_type~collection},
	noun(Dref,PRSIn,TmpPRS,Features),
	variable_list_bar(Dref,TmpPRS,TmpPRS2,Features),
	collection_complement(Features,Dref,TmpPRS2,PRSOut).

nbar1(Dref,PRSIn,PRSOut,Features) -->
	{Features = noun_type~collection},
	noun(Dref,PRSIn,TmpPRS,Features),
	add_dref(Features,TmpPRS,Dref,TmpPRS2),
	collection_complement(Features,Dref,TmpPRS2,TmpPRS3),
	optional_ppst(Dref,TmpPRS3,PRSOut,Features).

nbar1(Dref,PRSIn,PRSOut,Features) -->
	noun(Dref,PRSIn,TmpPRS,Features),
	variable_list_bar(Dref,TmpPRS,PRSOut,Features). 

nbar1(Dref,PRSIn,PRSOut,Features) -->
	noun(Dref,PRSIn,TmpPRS,Features),
	add_dref(Features,TmpPRS,Dref,TmpPRS2),
	optional_ppst(Dref,TmpPRS2,PRSOut,Features).

nbar1(Dref,PRSIn,PRSOut,Features) -->
	{ Features = noun~optional},
	variable_list_bar(Dref,PRSIn,PRSOut,Features).

collection_complement(specifier_type~SpecifierType,Dref1,PRSIn,PRSOut) -->
	[of],
	new_prs(prefix(left),PRSIn,LeftPRSIn),
	nbar(Dref2,LeftPRSIn,LeftPRSOut,number~plural..add_dref~yes),
	new_prs(new_prefix(right),LeftPRSOut,RightPRSIn),
	{
	transitive_verb_semantics([in],Dref2,[Dref1],[RightPRSIn],[RightPRSOut]),
	( SpecifierType = definite -> Operator = <=>; Operator = <= ),
	NewCondition =.. [Operator,LeftPRSOut,RightPRSOut]
	},
	add_condition(_,PRSIn,NewCondition,PRSOut).


%%	variable_list_bar(+Dref,+PRSIn,-PRSOut,?Features)
%
%	A variable_list_bar is list of variables possibly followed by a such_that_clause.
%
%	@param Features indicates whether the GULP feature "number" has value "singular" or "plural" and what the value of "dref_list" is.

variable_list_bar(Dref,PRSIn,PRSOut,Features) --> 
	{change_feature(Features,number,VariableListNumber,VariableListFeatures)},
	variable_list(PRSIn,TmpPRS,VariableListFeatures),
	{ 
	\+ VariableListNumber = null,
	( VariableListNumber = plural -> Features = number~plural; true )
	},
	% The features of optional_ppst are empty here, because if the object which the
	% such-that clause is about is already named directly before the such-that clause, we don't
	% need to resort to an earlier name of the object (which is what the features of
	% optional_ppst are for).
	optional_ppst(Dref,TmpPRS,PRSOut,_).


%%	variable_list(+PRSIn,-PRSOut,?Features)
%
%	This parses a list of variables, possible seperated by a comma.
%	The variable list can either be written in a single math mode or in a seperate math mode for each variable.
%	One can also list functions that have only variables as arguments.
%
%	@param Features indicates 
%		1. whether the GULP feature "number" has value "singular" or "plural" (this information can be input or output), 
%		2. what the vale of "dref_list" is,
%		3. whether the GULP feature "add_dref" has value "yes" or "no" (this information is input).

variable_list(PRSIn,PRSOut,number~plural..add_dref~yes..dref_list~DrefList) --> 
	[math(VariableList)], 
	{
	fo_var_list(VariableTreeList,_,VariableList,[]),
	\+ VariableTreeList = [],
	\+ VariableTreeList = [_],
	create_math_ids(VariableTreeList,add_dref~yes,PRSIn,PRSOut,DrefList)
	}.

variable_list(PRSIn,PRSOut,number~Number..add_dref~AddDref..dref_list~[Dref|RestDrefList]) -->
	[math(Variable)],
	{
	fo_variable(VariableTree,Variable,[]),
	create_math_ids([VariableTree],add_dref~AddDref,PRSIn,TmpPRS,[Dref])
	},
	comma_or_and(optional),
	variable_list(TmpPRS,PRSOut,number~RestNumber..add_dref~AddDref..dref_list~RestDrefList),
	{ 
	( RestNumber = null -> Number = singular; Number = plural )
	}.

variable_list(PRSIn,PRSOut,number~Number..add_dref~AddDref..dref_list~[NewDref|RestDrefList]) -->
	[math(FunctionTerm)],
	{
	fo_free_function(_,Tree,FreeVar,FunctionTerm,[]),
	create_math_ids(FreeVar,add_dref~yes,PRSIn,TmpPRS,_)
	},
	add_condition(add_dref~AddDref,TmpPRS,math_id(NewDref,Tree),TmpPRS2),
	comma(comma~optional),
	variable_list(TmpPRS2,PRSOut,number~RestNumber..add_dref~AddDref..dref_list~RestDrefList),
	{ 
	( RestNumber = null -> Number = singular; Number = plural ) 
	}.

variable_list(PRSIn,PRSIn,number~null..dref_list~[]) -->
	[],
	!.


%%	optional_ppst(+Dref,+PRSIn,-PRSOut,?Features)
%
%	This predicate optionally parses a prepositional phrase, a such-that clause or a
%	prepositional phrase followed by a such that clause.
%
%	@param Features contains the math_id feature (used when the ppst is a such-that
%	clause) as input, and outputs the alt_copulas feature if the ppst is a PP.

optional_ppst(Dref,PRSIn,PRSOut,Features) -->
	optionally_negated_pp(Dref,PRSIn,TmpPRS,Features),
	optional_such_that_clause(TmpPRS,PRSOut,Features).

optional_ppst(_,PRSIn,PRSOut,Features) -->
	optional_such_that_clause(PRSIn,PRSOut,Features).

%%	optionally_negated_pp(+Dref,+PRSIn,-PRSOut,-Features)
%
%	This is a propositional phrase or a negation of a propositional phrase.
%
%	@param Features can output a verb that can alternatively be used as a copula with the
%	preposition in question.

optionally_negated_pp(Dref,PRSIn,PRSOut,Features) -->
	[not],
	new_prs(prefix(neg),PRSIn,ScopeIn),
	pp(Dref,ScopeIn,ScopeOut,Features),
	add_condition(_,PRSIn,neg(ScopeOut),PRSOut).

optionally_negated_pp(Dref,PRSIn,PRSOut,Features) -->
	pp(Dref,PRSIn,PRSOut,Features).

%%	pp(+Dref,+PRSIn,-PRSOut,-Features)
%
%	A prepositional phrase is a preposition followed by a noun phrase coordination.
%
%	@param Features can output a verb that can alternatively be used as a copula with the
%	preposition in question.

pp(Dref,PRSIn,PRSOut,Features) -->
	preposition(Prep,Features),
	np_coord(DrefList,PRSIn,PRSOut,ScopeInList,ScopeOutList,_,_),
	{
	transitive_verb_semantics(Prep,Dref,DrefList,ScopeInList,ScopeOutList)
	}.


%%	optional_such_that_clause(+PRSIn,-PRSOut,+Features)
%
% 	A such-that clause is a subclause starting with "such that" followed by a proposition_coord.
%	@param Features specifies the value of "specifier_type". Additionally, it may contain the feature 
%	"math_id" with a math_id condition as value. If specifier_type is "the" and a math_id condition
%	is in Features, then this math_id condition is entered into the PRS before parsing the content 
%	of the such-that clause. (See the comment at "vp --> copula, np" for the rationale behind this).

optional_such_that_clause(PRSIn,PRSOut,Features) --> 
	such_that_clause(PRSIn,PRSOut,Features).

optional_such_that_clause(PRSIn,PRSIn,_) -->
	[].

such_that_clause(PRSIn,PRSOut,Features) -->
	comma(comma~optional),
	[such],[that],
	{
	( subsumes(math_id~_,Features), Features = specifier_type~definite ) 
	-> 
	( 
		Features = math_id~math_id(Dref,Mref), 
		add_condition(add_dref~no,PRSIn,math_id(Dref,Mref),TmpPRS,_,_), 
		% We need to remove the dref that add_condition automatically added:
		add_dref(Features,TmpPRS2,Dref,TmpPRS,_,_)
	)
	; 
	TmpPRS2 = PRSIn 
	},
	proposition_coord(TmpPRS2,PRSOut,mode~finite..subordinated~yes).


%=====================
% Verb phrases
%=====================

%%	vp_working_on_np_coord(+DrefList,+PRSInList,-PRSOutList,+Features)
%
%	This predicate parses the same input as vp, but does the PRS transformation on a list of
%	PRSs instead of on a single PRS. Additionally, if a dref is marked with the "plural" prefix,
%	this predicate ensures that the content of the VP is first written into a temporary plural
%	sub-PRS and that this plural PRS is subsequently dissolved.
%
% 	@param Features indicates whether the GULP feature 'mode' has value "finite" or "infinitive", and whether the GULP feature 'number' has value "singular" or "plural". 

vp_working_on_np_coord([Dref|DrefList],[PRSIn|PRSInList],[PRSOut|PRSOutList],Features,DCGListIn,DCGListOut) :-
	var(Dref),
	!,
	vp(Dref,PRSIn,PRSOut,Features,DCGListIn,DCGListOut),
	vp_working_on_np_coord(DrefList,PRSInList,PRSOutList,Features,DCGListIn,DCGListOut).

vp_working_on_np_coord([plural(Dref)|DrefList],[PRSIn|PRSInList],[PRSOut|PRSOutList],Features,DCGListIn,DCGListOut) :-
	new_prs(prefix(plural),PRSIn,PluralPRSIn,_,_),
	vp(Dref,PluralPRSIn,PluralPRSOut,Features,DCGListIn,DCGListOut),
	add_condition(_,PRSIn,plural(Dref,PluralPRSOut),TmpPRS,_,_),
	dissolve_plurals(TmpPRS,PRSOut),
	vp_working_on_np_coord(DrefList,PRSInList,PRSOutList,Features,DCGListIn,DCGListOut).

vp_working_on_np_coord([],[],[],_,_,_).

%%	vp(+Dref,+PRSIn,-PRSOut,+Features)
%
% 	@param Features indicates whether the GULP feature 'mode' has value "finite" or "infinitive", and whether the GULP feature 'number' has value "singular" or "plural". 

vp(Dref,PRSIn,PRSOut,Features) -->
	negation(PRSIn,PRSOut,ScopeIn,ScopeOut,Features),
	{
    Features = number~Number,
    VBarFeatures = mode~infinitive..number~Number
    },
	vbar(Dref,ScopeIn,ScopeOut,VBarFeatures).

vp(Dref,PRSIn,PRSOut,Features) -->
	vbar(Dref,PRSIn,PRSOut,Features).

vp(Dref,PRSIn,PRSOut,Features) -->
	optionally_negated_copula(PRSIn,PRSOut,ScopeIn,ScopeOut,Features),
	{
	% In order to create a correct PRS for sentences of the form "Let M be the line such that a and b are on M",
	% we need to copy the math_id of M (i.e. the math_id of Dref in PRSIn) with a substituted dref (here Dref2)
	% into the the-PRS, if a such-that clause is used in creating the the-PRS. In order to do this, we copy the
	% modified math_id into Features at this point.
	PRSIn = conds~Conds,
	( search_math_id(math_id(Dref,Mref),Conds) -> Features = math_id~math_id(Dref2,Mref) ; true )
	},
	specifier(Dref2,Features,ScopeIn,ScopeOut,RestrIn,RestrOut,NPScopeIn,NPScopeOut),
	{
	Features = dref_list~[Dref2],
	( Features = specifier_type~definite -> Features = add_dref~no; Features = add_dref~yes )
	},
	nbar(Dref2,RestrIn,RestrOut,Features),
	{Features = specifier_type~indefinite; Features = specifier_type~definite; Features = specifier_type~term},
	add_condition(_,NPScopeIn,predicate(Dref,Dref2,['=']),NPScopeOut).

vp(Dref,PRSIn,PRSOut,Features) -->
	optionally_negated_copula(PRSIn,PRSOut,ScopeIn,ScopeOut,Features),
	{Features = adj_trans~no},
	adjective(Dref,ScopeIn,ScopeOut,Features).

vp(Dref,PRSIn,PRSOut,Features) -->
	optionally_negated_copula(PRSIn,PRSOut,ScopeIn,ScopeOut,Features),
	adjective_parser(Adjective,Features),
	{
	Features = adj_trans~T, 
	\+ T = no
	},
	[T],
	np_coord(DrefList,ScopeIn,ScopeOut,ScopeInList,ScopeOutList,_,_),
	{
	transitive_verb_semantics([Adjective],Dref,DrefList,ScopeInList,ScopeOutList)
	}.

vp(Dref,PRSIn,PRSOut,Features) -->
	optionally_negated_verb(Verb,PRSIn,PRSOut,ScopeIn,ScopeOut,Features),
	pp(Dref,ScopeIn,ScopeOut,Features),
	{
	Features = alt_copulas~AltCopulas,
	member(Verb,[be|AltCopulas])
	}.

vp(_,PRSIn,PRSOut,Features) -->
	optionally_negated_copula(PRSIn,PRSOut,ScopeIn,ScopeOut,Features),
	% The features of optional_ppst are empty here, because if the object which the
	% such-that clause is about is already named directly before the such-that clause, we don't
	% need to resort to an earlier name of the object (which is what the features of
	% such_that_clause are for).
	such_that_clause(ScopeIn,ScopeOut,_).


%%	vbar(+Dref,+PRSIn,-PRSOut,+Features)
%
% 	@param Features indicates whether the GULP feature 'mode' has value "finite" or "infinitive", and whether the GULP feature 'number' has value "singular" or "plural". 

vbar(Dref,PRSIn,PRSOut,Features) -->
	transitive_verb_parser(Verb,Features),
	np_coord(DrefList,PRSIn,PRSOut,ScopeInList,ScopeOutList,_,_),
	{
	transitive_verb_semantics([Verb],Dref,DrefList,ScopeInList,ScopeOutList)
	}.

vbar(Dref,PRSIn,PRSOut,Features) -->
	intransitive_verb(Dref,PRSIn,PRSOut,Features).

%%	negation(+PRSIn,-PRSOut,-ScopeIn,-ScopeOut,+Features)
%
%	This parses a negation, i.e. the word "not" possibly preceded by the auxiliary "do"/"does".

negation(PRSIn,PRSOut,ScopeIn,ScopeOut,Features) -->
	{ Features = mode~finite, ! },
	intransitive_verb_parser(do,Features),
	[not],
	new_prs(prefix(neg),PRSIn,ScopeIn),
	add_condition(_,PRSIn,neg(ScopeOut),PRSOut).

negation(PRSIn,PRSOut,ScopeIn,ScopeOut,_) -->
	[not],
	new_prs(prefix(neg),PRSIn,ScopeIn),
	add_condition(_,PRSIn,neg(ScopeOut),PRSOut).

%%	optionally_negated_copula(+PRSIn,-PRSOut,-ScopeIn,-ScopeOut,+Feature)

optionally_negated_copula(PRSIn,PRSOut,ScopeIn,ScopeOut,Feature) -->
	negated_copula(Feature),
	new_prs(prefix(neg),PRSIn,ScopeIn),
	add_condition(_,PRSIn,neg(ScopeOut),PRSOut).

optionally_negated_copula(PRSIn,PRSOut,PRSIn,PRSOut,Feature) -->
	copula(Feature).

%%	negated_copula(+Feature)

negated_copula(Feature) -->
	{ Feature = mode~finite },
	!,
	copula(Feature),
	[not].

negated_copula(Feature) -->
	[not],
	copula(Feature).

%%	optionally_negated_verb(-Verb,+PRSIn,-PRSOut,-ScopeIn,-ScopeOut,+Features)

optionally_negated_verb(be,PRSIn,PRSOut,ScopeIn,ScopeOut,Features) -->
	optionally_negated_copula(PRSIn,PRSOut,ScopeIn,ScopeOut,Features).

optionally_negated_verb(Verb,PRSIn,PRSOut,ScopeIn,ScopeOut,Features) -->
	negation(PRSIn,PRSOut,ScopeIn,ScopeOut,Features),
	{
    Features = number~Number,
    VerbFeatures = mode~infinitive..number~Number
    },
	intransitive_verb_parser(Verb,VerbFeatures).

optionally_negated_verb(Verb,PRSIn,PRSOut,PRSIn,PRSOut,Features) -->
	intransitive_verb_parser(Verb,Features).


%====================
% Metasentences
%====================

%%	metasentence(PRSIn,PRSOut,+Mode)
%
%	A metasentence is a statement about cases, i.e. about the statements that can be made in different cases. 
%	It can be used to claim that named formulas or cases are true or false, 
%	that exactly or at least one of them is true or false, or to say that two cases are inconsistent. All these cases 
%	have to have appeared earlier! Otherwise, an appropriate error message appears.
%
%	@param Mode is a GULP feature of the form mode~finite or mode~infinitive.


% 	a classical metasentence consists of the following: 
% 	
% 	-	an np with case A, case B, etc. in it
% 	-	a vp having to do with correctness or falsehood of the cases.

metasentence(PRSIn,PRSOut,mode~Mode) -->
	{
	PRSIn = id~Id
	},
	meta_np(CaseList,Connective,Id,number~Number),
	meta_vp(CaseList,Connective,PRSIn,PRSOut,mode~Mode..number~Number).


%%	meta_np(?CaseList,?Connective,+Id)
%	
%	The noun phrase of a metasentence consists of an enumeration of cases connected by 'and'/'or'/obligatory commas,
%	possibly preceded by 'at most one of' or 'precisely one of'. The cases must exist already.
%
%	@param CaseList lists the cases in the meta_np in the text order (not reversed as many other lists). Each case in the list is represented by a pair consisting of the content of the case and the identifier of the case.
%	@param Connective can be one of "and", "or", "xor" and "at_most_one". Internally it can also be "rest".
%	@param id is the Id of the metasentence.

meta_np([[Case,Identifier]|CaseList],and,Id,number~plural) -->
	[case,Identifier],
	comma_or_and(obligatory),
	{
	getval(refids,RefIds),
	
	% The reason for using 'call' here is a complicated matter.
	%
	% Connecting cases by a ',' is ambiguous: the ',' will mostly preceed the word 'and'/'or'
	% and must therefore be interpreted in a way that is yet to be determined once
	% the rest of the sentence is parsed.
	(
	  call((member(ref(Case,case(Identifier)),RefIds), !));
	  
	  % if the case doesn't exist, return an error message
	  concat_atom(['Case ', Identifier, ' has not been introduced.'],Message),
	  add_error_message_once(case_reference_error,meta_np,Id,Message),
	  fail
	)
	},
	meta_np(CaseList,and,Id,_).

meta_np([[Case,Identifier]|CaseList],or,Id,number~singular) -->
	[case,Identifier],
	[or],
	{
	getval(refids,RefIds),
	
	% @','&'call' see remark above.
	(
	  call((member(ref(Case,case(Identifier)),RefIds), !));
	  
	  % if the case doesn't exist, return an error message
	  concat_atom(['Case ', Identifier, ' has not been introduced.'],Message),
	  add_error_message_once(case_reference_error,meta_np,Id,Message),
	  fail
	)
	},
	meta_np(CaseList,or,Id,number~singular).

meta_np([[Case1,Identifier1],[Case2,Identifier2]],or,Id,number~singular) -->
	[case,Identifier1],
	[or],
	{
	getval(refids,RefIds),
	
	% @','&'call' see remark above.
	(
	  call((member(ref(Case1,case(Identifier1)),RefIds), !));
	  
	  % if the case doesn't exist, return an error message
	  concat_atom(['Case ', Identifier1, ' has not been introduced.'],Message),
	  add_error_message_once(case_reference_error,meta_np,Id,Message),
	  fail
	)
	},
	[case,Identifier2],
	{
	getval(refids,RefIds),
	
	% @','&'call' see remark above.
	(
	  call((member(ref(Case2,case(Identifier2)),RefIds), !));
	  
	  % if the case doesn't exist, return an error message
	  concat_atom(['Case ', Identifier1, ' has not been introduced.'],Message),
	  add_error_message_once(case_reference_error,meta_np,Id,Message),
	  fail
	)
	}.

meta_np([[Case,Identifier]],and,Id,number~singular) -->
	[case,Identifier],
	{
	getval(refids,RefIds),
	
	% @','&'call' see remark above.
	(
	  call((member(ref(Case,case(Identifier)),RefIds), !));
	  
	  % if the case doesn't exist, return an error message
	  concat_atom(['Case ', Identifier, ' has not been introduced.'],Message),
	  add_error_message_once(case_reference_error,meta_np,Id,Message),
	  fail
	)
	}.

meta_np(CaseList,xor,Id,number~singular) -->
	[precisely,one,of],
	meta_np(CaseList,and,Id,number~plural).

meta_np(CaseList,at_most_one,Id,number~singular) -->
	[at,most,one,of],
	meta_np(CaseList,and,Id,number~plural).


%%	meta_vp(+CaseList,+Connective,+PRSIn,-PRSOut,+Features)
%	
%	The words in meta_vp are either an affirmative clause, a negative clause or a clause expressing an inconsistency.
%
%	@param: Features indicates the mode (finite or infinitive) and the number (singular or plural) of the VP.

meta_vp(CaseList,Connective,PRSIn,PRSOut,Features) -->
	meta_vp_parser(Type,Features),
	{
	meta_vp_semantics(Type,CaseList,Connective,PRSIn,PRSOut)
	}.

%%	meta_vp_parser(-Type,+Mode)
%
% 	Parses the VP of a metasentence and determines whether its Type is "affirmative", "negative" or "inconsistent".

meta_vp_parser(affirmative,Features) -->
	copula(Features),
	[correct].

meta_vp_parser(affirmative,Features) -->
	copula(Features),
	[true].
	
meta_vp_parser(affirmative,Features) -->
	intransitive_verb_parser(hold,Features).

meta_vp_parser(negative,Features) -->
	copula(Features),
	[incorrect].

meta_vp_parser(negative,Features) -->
	copula(Features),
	[false].
	
meta_vp_parser(negative,Features) -->
	negated_copula(Features),
	[correct].

meta_vp_parser(negative,Features) -->
	negated_copula(Features),
	[true].
	
meta_vp_parser(negative,mode~Mode..number~Number) -->
	negation(_,_,_,_,mode~Mode..number~Number),
	intransitive_verb_parser(hold,mode~infinitive..number~Number).

meta_vp_parser(inconsistent,Features) -->
	copula(Features),
	[inconsistent].

%%	meta_vp_semantics(+Type,+CaseList,+Connective,+PRSIn,-PRSOut)
%
%	Produces the semantics of a metasentence.
%
%	@param Type is "affirmative" or "negative" depending on the VP.
%	@param CaseList lists the cases of the NP as a string.
%	@param Connective is the connective used in the NP, i.e. either "and", "or", "xor" or "at_most_one".

meta_vp_semantics(affirmative,[[CaseHeadContent,_]|CaseTail],and,PRSIn,PRSOut) :-
	proposition_coord(PRSIn,TmpPRS,mode~finite..subordinated~yes,CaseHeadContent,[]),
	meta_vp_semantics(affirmative,CaseTail,and,TmpPRS,PRSOut).

meta_vp_semantics(negative,[[CaseHeadContent,Identifier]|CaseTail],and,PRSIn,PRSOut) :-
	PRSIn = id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter,
	NegPRSIn = id~case(Identifier,Id)..drefs~[]..mrefs~[]..conds~[]..rrefs~[]..accbefore~AccAfter..accafter~AccAfter,
	proposition_coord(NegPRSIn,NegPRSOut,mode~finite..subordinated~yes,CaseHeadContent,[]),
	TmpPRS = id~Id..drefs~Drefs..mrefs~Mrefs..conds~[neg(NegPRSOut)|Conds]..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter,
	meta_vp_semantics(negative,CaseTail,and,TmpPRS,PRSOut).

meta_vp_semantics(_,[],and,PRSIn,PRSIn).
		
meta_vp_semantics(Type,CaseList,or,PRSIn,PRSOut) :-
	PRSIn = id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter,
	case_list_semantics(Type,Id,AccAfter,CaseList,Disjuncts),
	disjunctive_cond(Id,AccAfter,Disjuncts,Cond),
	PRSOut = id~Id..drefs~Drefs..mrefs~Mrefs..conds~[Cond|Conds]..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter. 
		
meta_vp_semantics(Type,CaseList,xor,PRSIn,PRSOut) :-
	PRSIn = id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter,
	case_list_semantics(Type,Id,AccAfter,CaseList,PRSList),
	PRSOut = id~Id..drefs~Drefs..mrefs~Mrefs..conds~[><(PRSList)|Conds]..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter. 
		
meta_vp_semantics(Type,CaseList,at_most_one,PRSIn,PRSOut) :-
	PRSIn = id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter,
	case_list_semantics(Type,Id,AccAfter,CaseList,PRSList),
	PRSOut = id~Id..drefs~Drefs..mrefs~Mrefs..conds~[<>(PRSList)|Conds]..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter. 

meta_vp_semantics(inconsistent,CaseList,and,PRSIn,PRSOut) :-
	!,
	PRSIn = id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter,
	NegPRSIn = id~inconsistent_cases(Id)..drefs~[]..mrefs~[]..conds~[]..rrefs~[]..accbefore~AccAfter..accafter~AccAfter,
	meta_vp_semantics(affirmative,CaseList,and,NegPRSIn,NegPRSOut),
	PRSOut = id~Id..drefs~Drefs..mrefs~Mrefs..conds~[neg(NegPRSOut)|Conds]..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter.

meta_vp_semantics(inconsistent,_,_,PRSIn,_) :-
	PRSIn = id~Id,
	add_error_message_once(grammarError,metasentence,Id,'Inconsistency can only be aserted of formulae or cases that were conjuncted with "and"'),
	fail.

%%	case_list_semantics(+Type,+Id,+Acc,+CaseList,-PRSList)
%
%	Produces a list of PRSs from a list of cases.
%
%	@param Type is "affirmative" or "negative" depending on the meta-VP.
%	@param Id is the ID of the metasentence, and is included in the IDs of the PRSs.
%	@param Acc is the list of accessibles, that serves as accbefore to all PRSs in PRSList.

case_list_semantics(affirmative,Id,Acc,[[CaseHeadContent,Identifier]|CaseTail],[PRSHeadOut|PRSTail]) :-
	PRSHeadIn = id~case(Identifier,Id)..drefs~[]..mrefs~[]..conds~[]..rrefs~[]..accbefore~Acc..accafter~Acc,
	proposition_coord(PRSHeadIn,PRSHeadOut,mode~finite..subordinated~yes,CaseHeadContent,[]),
	case_list_semantics(affirmative,Id,Acc,CaseTail,PRSTail).
		
case_list_semantics(negative,Id,Acc,[[CaseHeadContent,Identifier]|CaseTail],[PRSHead|PRSTail]) :-
	NegPRSIn = id~case(Identifier,Id)..drefs~[]..mrefs~[]..conds~[]..rrefs~[]..accbefore~Acc..accafter~Acc,
	proposition_coord(NegPRSIn,NegPRSOut,mode~finite..subordinated~yes,CaseHeadContent,[]),
	PRSHead = id~not_case(Identifier,Id)..drefs~[]..mrefs~[]..conds~[neg(NegPRSOut)]..rrefs~[]..accbefore~Acc..accafter~Acc,
	case_list_semantics(negative,Id,Acc,CaseTail,PRSTail).

case_list_semantics(_,_,_,[],[]).


%=====================
% Words and triggers
%=====================

%%	noun(?Dref,+PRSIn,-PRSOut,+Features)
%
%	@param Features indicates whether number is singular or plural, and what the noun_type is (currently the only noun types are "collection" and "normal").

noun(Dref,PRSIn,PRSOut,Features) -->
	noun_parser(Noun,Features),
	{
	add_condition(_,PRSIn,predicate(Dref,Noun),PRSOut,_,_)
	}.

noun_parser(Noun,Features) -->
	{
	dcg_lexicon(DeclinedNoun,noun,Features,Noun)
	},
	DeclinedNoun,
	!.
	
	
%%	adjective(?Dref,+PRSIn,-PRSOut,+Features)
%
%	@param Features indicates whether number is singular or plural, and whether "adj_trans" is "no" or something else.

adjective(Dref,PRSIn,PRSOut,adj_trans~no) -->
	adjective_parser(Adjective,adj_trans~no),
	{
	add_condition(_,PRSIn,predicate(Dref,[Adjective]),PRSOut,_,_)
	}.

adjective(Dref,PRSIn,PRSOut,adj_trans~no..number~plural) -->
	adjective_parser(Adjective,adj_trans~T),
	{
	\+ T = no,
	add_condition(_,PRSIn,predicate(Dref,[Adjective]),PRSOut,_,_)
	}.

adjective_parser(Adjective,Features) -->
	[Adjective],
	{
	dcg_lexicon([Adjective],adjective,Features)
	},
	!.


%%	transitive_verb_parser(-Verb,+Features)
%
% 	For transitive verbs, parsing them and building their semantics are separated, because the PRSs
% 	to which the verb adds semantics are created by the NP that comes after the transitive verb.
% 	Hence we have "transitive_verb_parser" and "transitive_verb_semantics".
%
% 	@param Features indicates whether the GULP feature 'mode' has value "finite" or "infinitive", and whether the GULP feature 'number' has value "singular" or "plural". 

transitive_verb_parser(Verb,Features) -->
	{
	Features = transitive~plus,
	dcg_lexicon(ConjugatedVerb,verb,Features,Verb)
	},
	ConjugatedVerb,
	!.

%%	transitive_verb_semantics(+Verb,+Dref,+DrefList,+PRSInList,-PRSOutList)
%
%	Produces the semantics of a transitive verb. 
%
%	@param Dref is the dref of the subject NP. (If the subject is a coordinated NP, the NPs in it are already treated seperately before calling this predicate.)
%	@param DrefList is a list of object Drefs. Each Dref in DresList corresponds to one NP in the NP coordination of the object, and acts on one of the PRSs in PRSInList. There can be plural drefs of the form "plural(Dref)" in the list. In that case, the predicate is added to a temporary plural sub-PRS which gets dissolved subsequently.

transitive_verb_semantics(Verb,Dref,[DrefHead|DrefTail],[PRSInHead|PRSInTail],[PRSOutHead|PRSOutTail]) :-
	var(DrefHead),
	!,
	add_condition(_,PRSInHead,predicate(Dref,DrefHead,Verb),PRSOutHead,_,_),
	transitive_verb_semantics(Verb,Dref,DrefTail,PRSInTail,PRSOutTail).

transitive_verb_semantics(Verb,Dref,[plural(DrefHead)|DrefTail],[PRSInHead|PRSInTail],[PRSOutHead|PRSOutTail]) :-
	new_prs(prefix(plural),PRSInHead,PluralPRSIn,_,_),
	add_condition(_,PluralPRSIn,predicate(Dref,DrefHead,Verb),PluralPRSOut,_,_),
	add_condition(_,PRSInHead,plural(DrefHead,PluralPRSOut),TmpPRSHead,_,_),
	dissolve_plurals(TmpPRSHead,PRSOutHead),
	transitive_verb_semantics(Verb,Dref,DrefTail,PRSInTail,PRSOutTail).

transitive_verb_semantics(_,_,[],[],[]).


%%	intransitive_verb(+Dref,+PRSIn,-PRSOut,+Features)
%
%	@ param Dref is the dref of the subject. 
% 	@param Features indicates whether the GULP feature 'mode' has value "finite" or "infinitive", and whether the GULP feature 'number' has value "singular" or "plural". 

intransitive_verb(Dref,PRSIn,PRSOut,Features) -->
	intransitive_verb_parser(Verb,Features),
	{ intransitive_verb_semantics([Verb],Dref,PRSIn,PRSOut), ! }.

intransitive_verb_parser(Verb,Features) -->
	{
	Features = transitive~minus,
	dcg_lexicon(ConjugatedVerb,verb,Features,Verb)
	},
	ConjugatedVerb,
	!.

intransitive_verb_semantics(Verb,Dref,PRSIn,PRSOut) :-
	add_condition(_,PRSIn,predicate(Dref,Verb),PRSOut,_,_).


%%	copula(+Features)

copula(Features) -->
	{
	Features = transitive~copula,
	dcg_lexicon(ConjugatedVerb,verb,Features,_)
	},
	ConjugatedVerb,
	!.

%%	specifier(?Dref,-GULP,+PRSIn,-PRSOut,-RestrIn,-RestrOut,-ScopeIn,-ScopeOut)
%
%	@param GULP specifies the specifier_type of the specifier ("indefinite", "universal", "negative" or "definite"), its number and the dref_list of its NP.
%	@param RestrIn is a PRS created to be given as input to the N' in the restriction of the specifier.
%	@param ScopeIn is a PRS created to be given as input to the VP in the scope of the specifier.

specifier(_,specifier_type~universal..number~Number,PRSIn,PRSOut,RestrIn,RestrOut,ScopeIn,ScopeOut) -->
	{
	dcg_lexicon(Specifier,specifier,specifier_type~universal..number~Number)
	},
	Specifier,
	!,
	{ PRSIn = id~Id },
	new_prs(noun_phrase(Id),PRSIn,RestrIn),
	new_prs(verb_phrase(Id),RestrOut,ScopeIn),
	add_condition(_,PRSIn,RestrOut => ScopeOut,PRSOut).

specifier(_,specifier_type~negative..number~Number,PRSIn,PRSOut,RestrIn,RestrOut,ScopeIn,ScopeOut) -->
	{
	dcg_lexicon(Specifier,specifier,specifier_type~negative..number~Number)
	},
	Specifier,
	!,
	new_prs(prefix(neg),PRSIn,RestrIn),
	{ ScopeIn = RestrOut },
	add_condition(_,PRSIn,neg(ScopeOut),PRSOut).

specifier(Dref,specifier_type~definite..number~singular,PRSIn,PRSOut,RestrIn,RestrOut,ScopeIn,ScopeOut) -->
	{
	dcg_lexicon(Specifier,specifier,specifier_type~definite)
	},
	Specifier,
	!,
	new_prs(prefix(the),PRSIn,RestrIn),
	add_condition(add_dref~yes,PRSIn,the(Dref,RestrOut),ScopeIn),
	{ PRSOut = ScopeOut }.

specifier(_,specifier_type~indefinite..number~Number,PRSIn,PRSOut,RestrIn,RestrOut,ScopeIn,ScopeOut) -->
	{
	dcg_lexicon(Specifier,specifier,specifier_type~indefinite..number~Number)
	},
	Specifier,
	!,
	{
	RestrIn = PRSIn,
	ScopeIn = RestrOut,
	PRSOut = ScopeOut
	}.


number(Number) -->
	[Number],
	{
	dcg_lexicon([Number],number)
	},
	!.

preposition(Preposition,AltCopulas) -->
	{
	dcg_lexicon(Preposition,preposition,AltCopulas)
	},
	Preposition,
	!.

quantifier(Features) -->
	{
	dcg_lexicon(Quantifier,quantifier,Features)
	},
	Quantifier.

trigger(Features) -->
	{
	dcg_lexicon(Trigger,trigger,Features)
	},
	Trigger,
	comma(Features).

sentence_init(PRSIn,PRSOut,ScopeIn,ScopeOut,mode~Mode) --> 
	{
	dcg_lexicon(SentenceInit,sentence_init,type~negative..mode~Mode)
	},
	SentenceInit,
	!,
	{
	PRSIn = id~Id..drefs~Drefs..mrefs~Mrefs..conds~Conds..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter,
	ScopeIn = id~neg(Id)..drefs~[]..mrefs~[]..conds~[]..rrefs~[]..accbefore~AccBefore..accafter~AccAfter,
	PRSOut = id~Id..drefs~Drefs..mrefs~Mrefs..conds~[neg(ScopeOut)|Conds]..rrefs~Rrefs..accbefore~AccBefore..accafter~AccAfter
	}.

sentence_init(PRSIn,PRSOut,PRSIn,PRSOut,mode~Mode) -->
	{
	dcg_lexicon(SentenceInit,sentence_init,type~affirmative..mode~Mode)
	},
	SentenceInit.
	

%%	comma(+Features)
%
%	@param Features is a GULP term that indicated whether the feature "comma" has value "optional" or "no".

comma(comma~optional) --> [','], !.
comma(_) --> [].

iff --> [iff], !.
iff --> [if], [and], [only], [if].

implies --> [implies], [that], !.
implies --> [implies].

indefinite_article(number~singular) --> [a], !.
indefinite_article(number~singular) --> [an].
indefinite_article(number~plural) --> [].


%%	comma_or_and(+Optionality,+DCGListIn,-DCGListOut)
%
%	@param Optionality can take the values optional or obligatory.

comma_or_and(_) --> [','], !.
comma_or_and(_) --> [and].
comma_or_and(optional) --> [].

comma_or_or --> [','], !.
comma_or_or --> [or].

conseq_conjunct_marker -->
	[','], 
	trigger(type~ie),
	!.

conseq_conjunct_marker -->
	comma(comma~optional),
	[and],
	trigger(type~conseq_conjunct),
	!.


%%      local_cut(+Goal,+ListIn,?ListOut)
%
%       This predicated can be called as "local_cut(Goal)" in DCG notation. It disables backtracking into the Goal,
%       but does not commit to the choices made in the parent frame.

local_cut(Goal,ListIn,ListOut) :-
	call(Goal,ListIn,ListOut),
	!.

