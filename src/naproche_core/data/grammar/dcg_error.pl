:- module(dcg_error,[naproche_text/3]).

:- use_module(library(pldoc)).
:- use_module(naproche(gulp4swi)).
:- use_module(naproche(dcg_lexicon)).
:- use_module(naproche(fo_grammar)).
:- use_module(naproche(dcg_error_utils)).
:- use_module(naproche(dcg_utils)).

/** 	<module> Naproche Grammar
 *	
 *	This module checks whether the input obeys the grammar of the Naproche language without building PRSs.
 *	Additionally, it produces error messages in case of ungrammatical input.
 *	This parser parses text much quicker than dcg.pl, and especially recognises ungrammatical texts as such much quicker, and is hence used as a preprocessor. Since the predicates are largely parallel to those in dcg.pl, there isn't much documentation here. 
 *	@author Marcos Cramer
*/ 

%=====================
% Macrostructure
%=====================


% BASICS

% A Naproche text consists of axioms, lemmas, theorems and text.

naproche_text(_) --> 
	axiom, 
	naproche_text(_).

naproche_text(_) --> 
	theorem(_), 
	naproche_text(_).

naproche_text(_) --> 
	assumption_naproche_text, 
	naproche_text(_).

naproche_text(all_ass_closed~no) --> 
	call_position(Position),
	[sentence(Id,Content)], 
	{
	setval(sentence_id,Id),
	assumption(Position,Content,[]), 
	!
	},  
	record_position(sentences),
	naproche_text(_).

naproche_text(_) -->
	call_position(Position),
	[sentence(_,Content)],
	{ var_type_fix(Position,Content,[]) },
	record_position(sentences),
	naproche_text(_).

naproche_text(initial_text~yes) --> 
	text(empty~no..type~no_ass), 
	naproche_text(initial_text~no).

naproche_text(_) --> [].


% A var_type_fix is a sentence that links certain variable symbol collections to certain predicates (considered 'types').

var_type_fix(Sentence) -->
	symbol_collections(Sentence),
	infix_var_type_fix(Sentence),
	noun(Sentence,number~plural).

% symbol_collections parses a noun phrase that denotes some symbol collection(s).

symbol_collections(Sentence) -->
	symbol_collection(Sentence),
	comma_or_and(Sentence,obligatory),
	symbol_collections(Sentence).

symbol_collections(Sentence) -->
	symbol_collection(Sentence),
	[and],
	record_position(Sentence),
	symbol_collection(Sentence).

symbol_collections(Sentence) -->
	symbol_collection(Sentence).

symbol_collection(Sentence) -->
	optional_capitalization(Sentence),
	alphabet(Sentence),
	[letters],
	record_position(sentence).

optional_capitalization(Sentence) -->
	[small],
	record_position(Sentence).

optional_capitalization(Sentence) -->
	[capital],
	record_position(Sentence).

optional_capitalization(_) -->
	[].

alphabet(Sentence) -->
	[latin],
	record_position(Sentence).

alphabet(Sentence) -->
	[greek],
	record_position(Sentence).

alphabet(Sentence) -->
	[fraktur],
	record_position(Sentence).

alphabet(Sentence) -->
	[german],
	record_position(Sentence).

% infix_var_type_fix is the middle part of a var_type_fix sentence that starts with a symbol collection and ends with a noun.

infix_var_type_fix(Sentence) -->
	[always],
	record_position(Sentence),
	[denote],
	record_position(Sentence).

infix_var_type_fix(Sentence) -->
	[will],
	record_position(Sentence),
	[always],
	record_position(Sentence),
	[denote].

infix_var_type_fix(Sentence) -->
	[will],
	record_position(Sentence),
	[be],
	record_position(Sentence),
	[used],
	record_position(Sentence),
	[throughout],
	record_position(Sentence),
	[to],
	record_position(Sentence),
	[denote].

infix_var_type_fix(Sentence) -->
	[will],
	record_position(Sentence),
	[stand],
	record_position(Sentence),
	[throughout],
	record_position(Sentence),
	[for].


% Text contains statements, definitions, assumptions, assumption closings, cases and case closings. A text can never be empty.

text(type~Type) --> 
	call_position(Position),
	[sentence(Id,Content)], 
	{
	setval(sentence_id,Id),
	statement(Position,Content,[]), 
	!
	},  
	record_position(sentences),
	text(type~Type..empty~possible).

text(type~Type) --> 
	definition_text,  
	text(type~Type..empty~possible).

text(type~Type) --> 
	{ \+ Type = no_ass },
	assumption_text, 
	text(type~Type..empty~possible).

text(type~Type) --> 
	{ 
	\+ Type = all_ass_closed,
	\+ Type = no_ass
	},
	call_position(Position),
	[sentence(Id,Content)], 
	{
	setval(sentence_id,Id),
	assumption(Position,Content,[]), 
	!
	},  
	record_position(sentences),
	text(type~Type..empty~possible).

% during any text, we may introduce cases, close them with a closing sentence and continue with text.
text(type~Type) --> 
	cases_text(type~Type), 
	text(type~Type..empty~possible).

% at the end of a proof, however, if we aren't in a case already, we may introduce cases, and don't need to close them.
text(type~Type) --> 
	{ \+ Type = in_case}, 
	cases(type~Type..subtype~beginning).

text(type~theorem) --> 
	theorem(lemma), 
	text(type~theorem..empty~no).

text(empty~possible) --> [].



% STATEMENTS
%
% A statement has a proposition coordination as its core, starts with a statement trigger (which can be empty), 
% and can have a reference.
% Alternatively, it can just be the 'sentence' "contradiction" or "trivial".

statement(Sentence) -->
	trigger(Sentence,type~statement), 
	references(Sentence), 
	proposition_coord(Sentence,mode~finite..subordinated~no),
	references(Sentence).

statement(Sentence) --> 
	necessary_references(Sentence).
	
statement(Sentence) --> 
	[trivial],
	record_position(Sentence).

statement(Sentence) --> 
	[contradiction],
	record_position(Sentence),
	references(Sentence).

% REFERENCES
%
% references can be used to refer to a number of axioms, lemmas, theorems or proof methods, (optionally from a different Naproche text).

references(Sentence) -->
	necessary_references(Sentence).

references(_) -->
	[].

necessary_references(Sentence) -->
	[by],
	record_position(Sentence),
	reference_list(Sentence),
	comma(Sentence,comma~optional).

reference_list(Sentence) -->
	reference(Sentence),
	comma_or_and(Sentence,obligatory),
	reference_list(Sentence).

reference_list(Sentence) -->
	reference(Sentence).

reference(Sentence) --> 
	[axiom], 
	identifier(Sentence).

reference(Sentence) --> 
	[theorem], 
	record_position(Sentence),
	identifier(Sentence). 

reference(Sentence) --> 
	[lemma], 
	record_position(Sentence),
	identifier(Sentence). 

reference(Sentence) --> 
	[definition], 
	record_position(Sentence),
	identifier(Sentence).

reference(Sentence) --> 
	[induction], 
	record_position(Sentence).

% reference --> [by], [axiom], identifier, [in], naproche_text_id, optional_comma.
% reference --> [by], [lemma], identifier, [in], naproche_text_id, optional_comma.
% reference --> [by], [theorem], identifier, [in], naproche_text_id, optional_comma.
% reference --> [by], [definition], identifier, [in], naproche_text_id, optional_comma.

% DEFINITIONS

% Definition texts can consist of one-sentence definitions:

definition_text -->
	[sentence(_,[definition,_])], 
	record_position(sentences), 
	definition_text_core,
	([sentence(_,[end_definition])];[]).

definition_text -->
	definition_text_core,
	([sentence(_,[end_definition])];[]).	

definition_text_core --> 
	call_position(Position), 
	[sentence(Id,Content)], 
	{
	setval(sentence_id,Id),
	definition(Position,Content,[]), 
	!
	}, 
	record_position(sentences).


% Currently the only multi-sentence definitions are recursive definitions of function symbols (e.g. in Naproche-Landau
% without talk about sets and functions).
% fo_induction_start and fo_induction_step are of the required form to for a recursive definition of FunctionSymbol.

definition_text_core --> 
	[sentence(Id,Content)], 
		{
	setval(sentence_id,Id),
	Content = [define, math([FunctionSymbol]), recursively],
	fo_function_symbol(FunctionSymbolTree,[FunctionSymbol],[]),
	FunctionSymbolTree = name~FunctionSymbolName
	},
	record_position(sentences), 
	[sentence(_,[math(BaseCase)])], 
	{fo_induction_start(FunctionSymbolName,Number,_,_,_,BaseCase,[])}, 
	record_position(sentences), 
	[sentence(_,[math(SuccCase)])], 
	{fo_induction_step(FunctionSymbolName,Number,_,_,_,SuccCase,[])}, 
	record_position(sentences).


% Here is how to define new predicates symbols, new adjectives, new nouns and new verbs.

definition(Sentence) --> 
	[define],
	record_position(Sentence), 
	definiendum(Sentence), 
	iff(Sentence), 
	proposition_coord(Sentence,mode~finite..subordinated~yes).

% Here is how to explicitly define new function symbols, including 0-ary (i.e. constants).
% fo_free_function(F,_,_) is defined to be any term of the form F(x1, ..., xn), where F is an n-ary function_symbol.
% fo_term_without(F,_,_) is defined to be any term not containing the function symbol F.

definition(Sentence) --> 
	[define], 
	record_position(Sentence), 
	[math(X)], 
	{fo_free_function(F,_,_,X,[]), !}, 
	record_position(Sentence), 
	verb(Sentence,transitive~copula..mode~to-infinitive,_), 
	[math(Y)], 
	{fo_term_without(F,_,_,Y,[]), !},
	record_position(Sentence). 

definiendum(Sentence) --> 
	[math(X)], 
	{fo_free_predicate_symbol(_,_,X,[]), !},
	record_position(Sentence).

definiendum(Sentence) --> 
	[math(X)], 
	{fo_variable(_,X,[])},
	record_position(Sentence), 
	verb(Sentence,transitive~copula..mode~to-infinitive..number~singular,_), 
	indefinite_article(Sentence,number~singular), 
	!,
	noun(Sentence,number~singular).

definiendum(Sentence) --> 
	[math(X)], 
	{fo_variable(_,X,[])},
	record_position(Sentence), 
	verb(Sentence,transitive~copula..mode~to-infinitive..number~singular,_), 
	adjective(Sentence,number~singular..adj_trans~no). 

definiendum(Sentence) --> 
	[math(X)], 
	{fo_variable(_,X,[])},
	record_position(Sentence), 
	verb(Sentence,transitive~copula..mode~to-infinitive..number~singular,_), 
	adjective_parser(Sentence,number~singular..adj_trans~T),
	{ \+ T = no },
	[T],
	record_position(Sentence), 
	[math(Y)],
	{fo_variable(_,Y,[]), \+ X=Y},
	record_position(Sentence). 

definiendum(Sentence) --> 
	[math(X)], 
	{fo_variable(_,X,[])},
	record_position(Sentence), 
	[and],
	record_position(Sentence), 
	[math(Y)],
	{fo_variable(_,Y,[]), \+ X=Y},
	record_position(Sentence), 
	verb(Sentence,transitive~copula..mode~to-infinitive..number~plural,_), 
	adjective_parser(Sentence,number~plural..adj_trans~T),
	{ \+ T = no }.

definiendum(Sentence) --> 
	[math(X)], 
	{fo_variable(_,X,[])},
	record_position(Sentence), 
	verb(Sentence,transitive~minus..mode~to-infinitive..number~singular,_).

definiendum(Sentence) --> 
	[math(X)], 
	{fo_variable(_,X,[])},
	record_position(Sentence), 
	verb(Sentence,transitive~plus..mode~to-infinitive..number~singular,_), 
	[math(Y)], 
	{fo_variable(_,Y,[]), \+ X=Y},
	record_position(Sentence).

% ASSUMPTIONS

% assumption_naproche_text starts with an assumption and ends with an assumption closing.
% In between there is naproche_text.

assumption_naproche_text --> 
	call_position(Position), 
	[sentence(Id,Content)], 
	{
	setval(sentence_id,Id),
	assumption(Position,Content,[]),
	!
	}, 
	record_position(sentences), 
	naproche_text(all_ass_closed~yes),
	call_position(ClosingPosition), 
	[sentence(Id2,ClosingContent)], 
	{
	setval(sentence_id,Id2),
	closing(ClosingPosition,ClosingContent,[]) 
	}, 
	record_position(sentences).

% assumption_text starts with an assumption and ends with an assumption closing.
% In between there is text.

assumption_text --> 
	call_position(Position), 
	[sentence(Id,Content)], 
	{
	setval(sentence_id,Id),
	assumption(Position,Content,[]), 
	!
	}, 
	record_position(sentences), 
	text(type~all_ass_closed..empty~no), 
	call_position(ClosingPosition), 
	[sentence(Id2,ClosingContent)], 
	{
	setval(sentence_id,Id2),
	closing(ClosingPosition,ClosingContent,[]), 
	!
	}, 
	record_position(sentences).

% An assumption is a proposition_coord prefixed with an assumption trigger. Additionally, an assumption can be used to introduce new variables.

assumption(Sentence) --> 
	trigger(Sentence,type~variable_declaration), 
	variable_list_bar(Sentence,_).

assumption(Sentence) --> 
	[let], 
	record_position(Sentence), 
	variable_list_bar(Sentence,_), 
	[be], [given],
	record_position(Sentence). 

assumption(Sentence) --> 
	trigger(Sentence,type~ass..mode~Mode), 
	proposition_coord(Sentence,mode~Mode..subordinated~yes).

assumptions -->
	call_position(Position), 
	[sentence(Id,Content)],
	{
	setval(sentence_id,Id),
	assumption(Position,Content,_)
	},
	record_position(sentences),
	assumptions.

assumptions -->
	[].


% closing starts with an assumption closing trigger.

closing(Sentence) --> 
	trigger(Sentence,type~ass_closing), 
	references(Sentence), 
	proposition_coord(Sentence,mode~finite..subordinated~no),
	references(Sentence).


% CASES

% cases_text can either be used for listing mutually exclusive cases, or for proving something by case distinction.

% Here is how mutually exclusive cases work:

cases_text(type~_) --> 
	call_position(Position),
	[sentence(Id,Content)], 
	{
	setval(sentence_id,Id),
	trigger(Position,type~statement,Content,[precisely,one,of,the,following,cases,holds]), 
	!
	}, 
	record_position(sentences), 
	exclusive_cases.

cases_text(type~_) --> 
	call_position(Position),
	[sentence(Id,Content)], 
	{
	setval(sentence_id,Id),
	trigger(Position,type~statement,Content,[at,most,one,of,the,following,cases,holds]), 
	!
	}, 
	record_position(sentences), 
	exclusive_cases.

% Here is how proofs by case distinction work:
% A cases text consists of cases and a case closing.

cases_text(type~Type) --> 
	cases(type~Type..subtype~beginning), 
	call_position(Position),
	[sentence(Id,Content)],
	{
	setval(sentence_id,Id),
	case_closing(Position,Content,[]), 
	!
	},
	record_position(sentences). 

% exclusive_cases is a list of the mutually exclusive cases. Each of the cases can be prefixed by a sentence of the form "Case X".

exclusive_cases --> 
	[sentence(_,[case,_])], 
	record_position(sentences), 
	call_position(Position), 
	[sentence(Id,Content)], 
	{
	setval(sentence_id,Id),
	proposition_coord(Position,mode~finite..subordinated~yes,Content,[]),
	!
	}, 
	record_position(sentences), 
	exclusive_cases.

exclusive_cases --> 
	call_position(Position), 
	[sentence(Id,Content)], 
	{
	setval(sentence_id,Id),
	proposition_coord(Position,mode~finite..subordinated~yes,Content,[]),
	!
	}, 
	record_position(sentences), 
	exclusive_cases.

exclusive_cases --> [].


cases(type~Type..subtype~SubType) -->
	case_introduction(type~Type..subtype~SubType),	
	[sentence(_,[case,_])],
	record_position(sentences),
	call_position(Position),
	[sentence(Id,Content)],
	{
	setval(sentence_id,Id),
	proposition_coord(Position,mode~finite..subordinated~yes,Content,[]),
	!
	},
	record_position(sentences),
	text(type~in_case..empty~no),
	!,
	cases(type~Type..subtype~rest-list).

cases(subtype~rest-list) --> [].

case_introduction(subtype~SubType) --> 
	{ \+ SubType = rest-list },
	call_position(Sentence),
	[sentence(Id,Content)],
	{
	setval(sentence_id,Id),
	trigger(Sentence,type~statement,Content,[there,are|Rest]),
	number(Sentence,Rest,[cases])
	},
	record_position(sentences).
	
case_introduction(type~Type) --> { \+ Type = in_case }, [].

case_introduction(subtype~rest-list) --> [].

% case_closing starts with case closing trigger.

case_closing(Sentence) --> 
	trigger(Sentence,type~statement), 
	trigger(Sentence,type~case_closing), 
	proposition_coord(Sentence,mode~finite..subordinated~no).


% AXIOMS, LEMMAS AND THEOREMS

% An axiom consists of a heading followed by text.

axiom --> 
	[sentence(_,[axiom])], 
	record_position(sentences),
	call_position(Position),
	[sentence(Id,Content)], 
	{
	setval(sentence_id,Id),
	proposition_coord(Position,mode~finite..subordinated~yes,Content,[]), 
	!
	}, 
	record_position(sentences),
	([sentence(_,[end_axiom])];[]). 

axiom --> 
	[sentence(_,[axiom,_])], 
	record_position(sentences), 
	call_position(Position),
	[sentence(Id,Content)], 
	{
	setval(sentence_id,Id),
	proposition_coord(Position,mode~finite..subordinated~yes,Content,[]), 
	!
	}, 
	record_position(sentences).


% A theorem consists of a heading, a goal text, the marker "Proof", a body text (which possibly includes lemmas) and the marker "Qed".

theorem(TheoremType) -->
	[sentence(Id,Heading)],
	{
	heading(TheoremType,Heading,[])
	},
	rest_list(List),
	{expecting_proof_and_qed(TheoremType,Id,List,[]), !},
	record_position(sentences), 
	assumptions,
	text(type~no_ass..empty~no),
	goal_end(TheoremType),
	[sentence(_,[proof])],
	record_position(sentences), 
	{
	TheoremType = theorem -> ProofTextType = theorem ; ProofTextType = normal
	},
	text(type~ProofTextType..empty~no),
	([sentence(_,[qed])];[sentence(_,[end_proof])]),
	record_position(sentences),
	!.

heading(TheoremType) -->
	[TheoremType],
	{ TheoremType = theorem ; TheoremType = lemma }.

heading(TheoremType) -->
	[TheoremType,_],
	{ TheoremType = theorem ; TheoremType = lemma }.

goal_end(TheoremType) -->
	{ atom_concat(end_,TheoremType,GoalEnd) },
	[sentence(_,[GoalEnd])],
	record_position(sentences). 

goal_end(_) -->
	[].

%=====================
% proposition_coord
%=====================

% proposition_coord can be either a simple sentence_coord, or a number of sentence_coords linked with "if...then" or "iff".
% The gulp feature "mode" can take three values: "that" indicates that the subclause is to be prefixed with "that", "infinitive"
% indicates that the verb of the sentence is in the infinitive, and "finite" is the normal case.
% The gulp feature "that" defines whether the proposition_coord is a clause subordinated with a "that". It can tak values "plus" or "minus".

proposition_coord(Sentence,mode~Mode..subordinated~S1) -->
	{ var(S1); S1 = no; S2 = yes },
	sentence_coord(Sentence,mode~Mode..subordinated~S2),
	proposition_coord_tail(Sentence,S1,S2).

proposition_coord(Sentence,mode~finite..subordinated~Sub) --> 
	[if], 
	record_position(Sentence), 
	sentence_coord(Sentence,mode~finite..subordinated~yes), 
	comma(Sentence,comma~optional), 
	[then],
	references(Sentence),
	record_position(Sentence), 
	references(Sentence),
	trigger(Sentence,type~conseq),
	proposition_coord(Sentence,mode~finite..subordinated~Sub).

proposition_coord(Sentence,mode~that..subordinated~Sub) --> 
	[that], 
	record_position(Sentence), 
	[if], 
	record_position(Sentence), 
	sentence_coord(Sentence,mode~finite..subordinated~yes), 
	comma(Sentence,comma~optional), 
	[then], 
	references(Sentence),
	record_position(Sentence), 
	references(Sentence),
	trigger(Sentence,type~conseq),
	proposition_coord(Sentence,mode~finite..subordinated~Sub).

proposition_coord_tail(Sentence,S,S) -->
	comma(Sentence,comma~optional), 
	[if],
	record_position(Sentence),
	sentence_coord(Sentence,mode~finite..subordinated~yes).

proposition_coord_tail(Sentence,_,yes) -->
	comma(Sentence,comma~optional), 
	iff(Sentence),
	sentence_coord(Sentence,mode~finite..subordinated~yes).

proposition_coord_tail(_,S,S) -->
	[].

%=====================
% sentence_coord
%=====================

% sentence_coord works like in Attempto (with "i.e." added). It links a number of topicalised_sentences with
% "and", "or", ", and", ", or" and "i.e." in such a way that the bracketing is unambiguous.

sentence_coord(Sentence,mode~Mode..subordinated~S) --> 
	sentence_coord_0(Sentence,mode~Mode), 
	sentence_coord_tail(Sentence,mode~Mode..subordinated~S).

sentence_coord_tail(Sentence,mode~Mode..subordinated~no) --> 
	references(Sentence),
	conseq_conjunct_marker(Sentence),
	references(Sentence),
	sentence_coord(Sentence,mode~Mode).

sentence_coord_tail(Sentence,mode~Mode..subordinated~no) --> 
	references(Sentence),
	comma(Sentence,comma~optional),
	[and],
	record_position(Sentence),
	necessary_references(Sentence),
	sentence_coord(Sentence,mode~Mode).

sentence_coord_tail(Sentence,mode~Mode..subordinated~no) --> 
	necessary_references(Sentence),
	comma(Sentence,comma~optional),
	[and],
	record_position(Sentence),
	sentence_coord(Sentence,mode~Mode).

sentence_coord_tail(_,_) --> [].


sentence_coord_0(Sentence,mode~Mode) -->
	{setval(comma_conjunction,0)}, 
	sentence_coord_1(Sentence,mode~Mode),
	{getval(comma_conjunction,CC)}, 
	sentence_coord_0_tail(Sentence,CC,mode~Mode).

sentence_coord_0_tail(Sentence,CC,mode~Mode) --> 
	[','], 
	record_position(Sentence), 
	[or],
	record_position(Sentence), 
	sentence_coord_0(Sentence,mode~Mode),
	{
	getval(comma_conjunction,CC),
	( call((CC = 0, !));
	( getval(sentence_id,Id), add_warning_message_once(grammarWarning,sentence_coord,Id,'Note that the clause-joining comma in this sentence is interpreted as "and" and not as "or". Clause-joining commas are always interpreted as "and" in Naproche.') ))
	}.

sentence_coord_0_tail(_,_,mode~_) --> [].

sentence_coord_1(Sentence,mode~Mode) --> 
	sentence_coord_2(Sentence,mode~Mode), 
	sentence_coord_1_tail(Sentence,mode~Mode).

sentence_coord_1_tail(Sentence,mode~Mode) --> 
	[','],  
	record_position(Sentence), 
	trigger(Sentence,type~conjunction),
	sentence_coord_1(Sentence,mode~Mode).

sentence_coord_1_tail(_,mode~_) --> [].


sentence_coord_2(Sentence,mode~Mode) --> 
	{setval(comma_conjunction,0)}, 
	sentence_coord_3(Sentence,mode~Mode),
	{getval(comma_conjunction,CC)}, 
	sentence_coord_2_tail(Sentence,CC,mode~Mode).

sentence_coord_2_tail(Sentence,CC,mode~Mode) --> 
	[or], 
	record_position(Sentence), 
	sentence_coord_2(Sentence,mode~Mode),
	{
	getval(comma_conjunction,CC),
	( call((CC = 0, !));
	( getval(sentence_id,Id), add_warning_message_once(grammarWarning,sentence_coord,Id,'Note that the clause-joining comma in this sentence is interpreted as "and" and not as "or". Clause-joining commas are always interpreted as "and" in Naproche.') ))
	}.

sentence_coord_2_tail(_,_,mode~_) --> [].


sentence_coord_3(Sentence,mode~that) --> 
	!,
	[that], 
	record_position(Sentence), 
	topicalised_sentence(Sentence,mode~finite), 
	sentence_coord_3_tail(Sentence,mode~that).

sentence_coord_3(Sentence,mode~Mode) --> 
	topicalised_sentence(Sentence,mode~Mode), 
	sentence_coord_3_tail(Sentence,mode~Mode).

sentence_coord_3_tail(Sentence,mode~Mode) --> 
	trigger(Sentence,type~conjunction),
	sentence_coord_3(Sentence,mode~Mode).

sentence_coord_3_tail(Sentence,mode~Mode) --> 
	[','],
	sentence_coord_3(Sentence,mode~Mode),
	{ setval(comma_conjunction,1) }.

sentence_coord_3_tail(_,mode~_) --> [].


% A topicalised_sentence can be a quantified statement, two composite_sentences linked with "implies that", or just one composite_sentence. 

topicalised_sentence(Sentence,mode~Mode) --> 
	existential_topic(Sentence,mode~Mode).

topicalised_sentence(Sentence,mode~finite) --> 
	universal_topic(Sentence), 
	comma(Sentence,comma~optional), 
	proposition_coord(Sentence,mode~finite..subordinated~no).

topicalised_sentence(Sentence,mode~finite) -->
	rest_list(List),
	{ member(implies,List) },
	composite_sentence(Sentence,mode~finite), 
	comma(Sentence,comma~optional), 
	implies(Sentence), 
	composite_sentence(Sentence,mode~finite).

topicalised_sentence(Sentence,mode~Mode) --> 
	composite_sentence(Sentence,mode~Mode).

existential_topic(Sentence,Features) --> 
	{ Features = type~existential },
	quantifier(Sentence,Features),
	np(Sentence,Features),
	{ Features = specifier_type~indefinite ; Features = specifier_type~negative }.

universal_topic(Sentence) --> 
	[for],
	np(Sentence,specifier_type~universal).

% A composite_sentence is either a simple sentence, a formula or a proposition_coord prefixed with a sentence_init.
% By the constructions that contain composite_sentence, it is given that the "mode" feature can here only take the
% values "finite" and "infinitive" (not "that").

composite_sentence(Sentence,mode~Mode) --> 
	sentence(Sentence,mode~Mode).

composite_sentence(Sentence,mode~Mode) --> 
	sentence_init(Sentence,mode~Mode), 
	proposition_coord(Sentence,mode~that..subordinated~yes).

composite_sentence(Sentence,mode~Mode) --> 
	trigger(Sentence,type~formula..mode~Mode), 
	[math(X)], 
	{fo_formula(_,_,X,[]), !},
	record_position(Sentence).

composite_sentence(Sentence,mode~Mode) -->
	metasentence(Sentence,mode~Mode).

% A sentence is just a noun phrase followed by a verb phrase"

sentence(Sentence,mode~Mode) --> 
	np_coord(Sentence,number~Number,_), 
	vp(Sentence,mode~Mode..number~Number).


%=====================
% Noun phrases
%=====================

np_coord(Sentence,number~plural,and) -->
	rest_list(List),
	{ member(and,List) },
	local_cut(np(Sentence,_)),
	comma_or_and(Sentence,obligatory),
	np_coord(Sentence,_,and).

np_coord(Sentence,number~plural,and) -->
	rest_list(List),
	{ member(and,List) },
	local_cut(np(Sentence,_)),
	[and],
	record_position(Sentence),
	np(Sentence,_).

np_coord(Sentence,number~CoordNumber,or) -->
	rest_list(List),
	{ member(or,List) },
	local_cut(np(Sentence,number~Number1)),
	[or],
	record_position(Sentence),
	np_coord(Sentence,number~Number2,or),
	{ disjunct_number(Number1,Number2,CoordNumber) }.

np_coord(Sentence,number~CoordNumber,or) -->
	rest_list(List),
	{ member(or,List) },
	local_cut(np(Sentence,number~Number1)),
	[or],
	record_position(Sentence),
	np(Sentence,number~Number2),
	{ disjunct_number(Number1,Number2,CoordNumber) }.

np_coord(Sentence,number~Number,no) -->
	np(Sentence,number~Number).

np(Sentence,number~singular..specifier_type~term) -->
	term(Sentence). 

np(Sentence,number~Number..specifier_type~SpecifierType) -->
	rest_list(List1),
	specifier(Sentence,number~Number..specifier_type~SpecifierType), 
	rest_list(List2),
	nbar(Sentence,number~Number),
	rest_list(List3),
	{ 
	\+ List1 = List2
	; 
	\+ ( 
		List2 = [math(X)|List3], 
		( fo_variable(_,X,[]) ; fo_free_function(_,_,_,X,[]) ) 
	) 
	}.

% np --> pronoun.

term(Sentence) -->
	[math(X)], 
	{fo_term(_,_,X,[]), !},
	record_position(Sentence). 

nbar(Sentence,Features) --> 
	nbar1(Sentence,Features).

nbar(Sentence,Features) --> 
	{Features = adj_trans~no},
	adjective(Sentence,Features), 
	nbar(Sentence,Features).

nbar1(Sentence,number~Number) --> 
	noun(Sentence,number~Number..noun_type~collection), 
	variable_list_bar(Sentence,number~Number),
	collection_complement(Sentence).

nbar1(Sentence,number~Number) --> 
	noun(Sentence,number~Number..noun_type~collection),
	collection_complement(Sentence),
	optional_ppst(Sentence,_).

nbar1(Sentence,Features) --> 
	noun(Sentence,Features), 
	variable_list_bar(Sentence,Features).

nbar1(Sentence,Features) --> 
	noun(Sentence,Features),
	optional_ppst(Sentence,_).

nbar1(Sentence,Features) --> 
	{ Features = noun~optional },
	variable_list_bar(Sentence,Features).

collection_complement(Sentence) -->
	[of],
	record_position(Sentence),
	nbar(Sentence,number~plural).

variable_list_bar(Sentence,number~NPNumber) --> 
	variable_list(Sentence,number~VariableListNumber), 
	{
		\+ VariableListNumber = null,
		( VariableListNumber = plural -> NPNumber = plural; true )
		},
	optional_ppst(Sentence,_).

variable_list(Sentence,number~plural) --> 
	[math(X)], 
	{ 
	fo_var_list(VarList,_,X,[]),
	\+ VarList = [],
	\+ VarList = [_]
	},
	record_position(Sentence). 

variable_list(Sentence,number~Number) --> 
	[math(X)], 
	{fo_variable(_,X,[])},
	record_position(Sentence),
	comma_or_and(Sentence,optional),
	variable_list(Sentence,number~RestNumber),
	{ RestNumber = null -> Number = singular; Number = plural }.

variable_list(Sentence,number~Number) --> 
	[math(FunctionTerm)], 
	{fo_free_function(_,_,_,FunctionTerm,[])},
	record_position(Sentence), 
	comma(Sentence,comma~optional),
	variable_list(Sentence,number~RestNumber),
	{ RestNumber = null -> Number = singular; Number = plural }.

variable_list(_,number~null) --> [], !.

optional_ppst(Sentence,Features) -->
	optionally_negated_pp(Sentence,Features),
	optional_such_that_clause(Sentence).

optional_ppst(Sentence,_) -->
	optional_such_that_clause(Sentence).

optionally_negated_pp(Sentence,Features) -->
	[not],
	record_position(Sentence),
	pp(Sentence,Features).

optionally_negated_pp(Sentence,Features) -->
	pp(Sentence,Features).

pp(Sentence,Features) -->
	preposition(Sentence,Features),
	np_coord(Sentence,_,_).

optional_such_that_clause(Sentence) --> 
	such_that_clause(Sentence).

optional_such_that_clause(_) -->
	[].

such_that_clause(Sentence) -->
	comma(Sentence,comma~optional),
	[such],
	record_position(Sentence), 
	[that], 
	record_position(Sentence), 
	proposition_coord(Sentence,mode~finite..subordinated~yes).


%=====================
% Verb phrases
%=====================

vp(Sentence,Features) -->
	negation(Sentence,Features),
	{
	Features = number~Number,
	VBarFeatures = mode~infinitive..number~Number
	},
	vbar(Sentence,VBarFeatures).

vp(Sentence,Features) -->
	vbar(Sentence,Features).

vp(Sentence,Features) -->
	optionally_negated_copula(Sentence,Features),
	specifier(Sentence,Features),
	nbar(Sentence,Features),
	{Features = specifier_type~indefinite; Features = specifier_type~definite; Features = specifier_type~term}.

vp(Sentence,Features) -->
	optionally_negated_copula(Sentence,Features),
	{Features = adj_trans~no},
	adjective(Sentence,Features).

vp(Sentence,Features) -->
	optionally_negated_copula(Sentence,Features),
	adjective_parser(Sentence,Features),
	{
	Features = adj_trans~T,  
	\+ T = no
	},
	[T],
	record_position(Sentence),
	np_coord(Sentence,_,_).

vp(Sentence,Features) -->
	optionally_negated_verb(Sentence,Verb,Features),
	pp(Sentence,Features),
	{
	Features = alt_copulas~AltCopulas,
	member(Verb,[be|AltCopulas])
	}.

vp(Sentence,Features) -->
	optionally_negated_copula(Sentence,Features),
	such_that_clause(Sentence).

vbar(Sentence,mode~Mode..number~Number) --> 
	verb(Sentence,transitive~plus..mode~Mode..number~Number,_), 
	np_coord(Sentence,_,_).

vbar(Sentence,mode~Mode..number~Number) --> 
	verb(Sentence,transitive~minus..mode~Mode..number~Number,_).

negation(Sentence,Features) -->
	{ Features = mode~finite, ! },
	verb(Sentence,Features,do),
	[not],
	record_position(Sentence).

negation(Sentence,_) -->
	[not],
	record_position(Sentence).

optionally_negated_copula(Sentence,Feature) -->
	negated_copula(Sentence,Feature).

optionally_negated_copula(Sentence,mode~Mode..number~Number) -->
	verb(Sentence,mode~Mode..number~Number..transitive~copula,_).

negated_copula(Sentence,mode~finite..number~Number) -->
	!,
	verb(Sentence,mode~finite..number~Number..transitive~copula,_),
	[not].

negated_copula(Sentence,mode~Mode..number~Number) -->
	[not],
	verb(Sentence,mode~Mode..number~Number..transitive~copula,_).

optionally_negated_verb(Sentence,be,Features) -->
	optionally_negated_copula(Sentence,Features).

optionally_negated_verb(Sentence,Verb,Features) -->
	negation(Sentence,Features),
	{
	Features = number~Number,
	VerbFeatures = mode~infinitive..number~Number..transitive~no
	},
	verb(Sentence,VerbFeatures,Verb).

optionally_negated_verb(Sentence,Verb,Features) -->
	{Features = transitive~minus},
	verb(Sentence,Features,Verb).

%====================
% Metasentences
%====================

%	A metasentence can be used to claim that a named formula or case is true or false, or to say that two cases
%	are inconsistent. Or to say that precisely one or at most one of the cases is true.

metasentence(Sentence,mode~Mode) -->
	meta_np(Sentence,_,number~Number),
	meta_vp(Sentence,mode~Mode..number~Number).


meta_np(Sentence,and,number~plural) -->
	[case,_],
	record_position(Sentence),
	comma_or_and(Sentence,obligatory),
	meta_np(Sentence,and,_).

meta_np(Sentence,or,number~singular) -->
	[case,_],
	record_position(Sentence),
	[or],
	record_position(Sentence),
	meta_np(Sentence,or,number~singular).

meta_np(Sentence,or,number~singular) -->
	[case,_],
	record_position(Sentence),
	[or],
	record_position(Sentence),
	[case,_].

meta_np(Sentence,and,number~singular) -->
	[case,_],
	record_position(Sentence).

meta_np(Sentence,xor,number~singular) -->
	[precisely,one,of],
	record_position(Sentence),
	meta_np(Sentence,and,number~plural).

meta_np(Sentence,at_most_one,number~singular) -->
	[at,most,one,of],
	record_position(Sentence),
	meta_np(Sentence,and,number~plural).



meta_vp(Sentence,mode~Mode..number~Number) -->
	verb(Sentence,mode~Mode..number~Number..transitive~copula,_),
	[correct],
	record_position(Sentence).

meta_vp(Sentence,mode~Mode..number~Number) -->
	verb(Sentence,mode~Mode..number~Number..transitive~copula,_),
	[true],
	record_position(Sentence).
	
meta_vp(Sentence,Features) -->
	verb(Sentence,Features,hold).

meta_vp(Sentence,mode~Mode..number~Number) -->
	verb(Sentence,mode~Mode..number~Number..transitive~copula,_),
	[incorrect],
	record_position(Sentence).

meta_vp(Sentence,mode~Mode..number~Number) -->
	verb(Sentence,mode~Mode..number~Number..transitive~copula,_),
	[false],
	record_position(Sentence).
	
meta_vp(Sentence,Features) -->
	negated_copula(Sentence,Features),
	[correct],
	record_position(Sentence).

meta_vp(Sentence,Features) -->
	negated_copula(Sentence,Features),
	[true],
	record_position(Sentence).
	
meta_vp(Sentence,mode~Mode..number~Number) -->
	negation(Sentence,mode~Mode..number~Number),
	verb(Sentence,mode~infinitive..number~Number,hold).

meta_vp(Sentence,mode~Mode..number~Number) -->
	verb(Sentence,mode~Mode..number~Number..transitive~copula,_),
	[inconsistent],
	record_position(Sentence).


%=====================
% Words and triggers
%=====================

noun(Sentence,Number) -->
	{
	dcg_lexicon(Noun,noun,Number,_)
	},
	Noun,
	!,
	record_position(Sentence).

/*
pronoun(Sentence,Number) -->
	[Pronoun],
	{
	dcg_lexicon([Pronoun],pronoun,Number)
	},
	record_position(Sentence), 
	!.
*/

adjective(Sentence,adj_trans~no) -->
	adjective_parser(Sentence,adj_trans~no).

adjective(Sentence,adj_trans~no..number~plural) -->
	adjective_parser(Sentence,adj_trans~T),
	{ \+ T = no }.

adjective_parser(Sentence,Features) -->
	[Adjective],
	{
	dcg_lexicon([Adjective],adjective,Features)
	},
	record_position(Sentence), 
	!.

verb(Sentence,Features,Verb) -->
	{
	dcg_lexicon(ConjugatedVerb,verb,Features,Verb)
	},
	ConjugatedVerb,
	record_position(Sentence), 
	!.

specifier(Sentence,Features) -->
	{
	dcg_lexicon(Specifier,specifier,Features)
	},
	Specifier,
	!,
	record_position(Sentence).

number(Sentence) -->
	{
	dcg_lexicon([Number],number)
	},
	[Number],
	!,
	record_position(Sentence).

preposition(Sentence,AltCopulas) -->
	{
	dcg_lexicon(Preposition,preposition,AltCopulas)
	},
	Preposition,
	record_position(Sentence),
	!.

quantifier(Sentence,Features) -->
	{
	dcg_lexicon(Quantifier,quantifier,Features)
	},
	Quantifier,
	record_position(Sentence).

trigger(Sentence,Features) -->
	{
	dcg_lexicon(Trigger,trigger,Features)
	},
	Trigger,
	comma(Sentence,Features).

sentence_init(Sentence,mode~Mode) -->
	{
	dcg_lexicon(SentenceInit,sentence_init,mode~Mode)
	},
	SentenceInit,
	!,
	record_position(Sentence).

comma(Sentence,comma~optional) --> 
	[','],
	!,
	record_position(Sentence).

comma(Sentence,_) --> 
	[],
	record_position(Sentence).

iff(Sentence) --> 
	[iff],
	!,
	record_position(Sentence).

iff(Sentence) --> 
	[if], 
	record_position(Sentence),
	[and], 
	record_position(Sentence),
	[only], 
	record_position(Sentence),
	[if],
	record_position(Sentence).

implies(Sentence) -->
	[implies], 
	record_position(Sentence),
	[that],
	!,
	record_position(Sentence).

implies(Sentence) --> 
	[implies],
	record_position(Sentence).

identifier(Sentence) --> 
	[_],
	record_position(Sentence).

indefinite_article(Sentence,number~singular) --> 
	[a],
	!,
	record_position(Sentence).

indefinite_article(Sentence,number~singular) --> 
	[an],
	record_position(Sentence).

indefinite_article(_,number~plural) --> [].

comma_or_and(Sentence,_) --> 
	[','],
	!,
	record_position(Sentence).

comma_or_and(Sentence,_) --> 
	[and],
	record_position(Sentence).

comma_or_and(_,optional) --> 
	[].

comma_or_or(Sentence) --> 
	[','],
	!,
	record_position(Sentence).

comma_or_or(Sentence) --> 
	[or],
	record_position(Sentence).

conseq_conjunct_marker(Sentence) -->
		[','],
	record_position(Sentence),
		trigger(Sentence,type~ie),
		!.

conseq_conjunct_marker(Sentence) -->
		comma(Sentence,comma~optional),
		[and],
	record_position(Sentence),
		trigger(Sentence,type~conseq_conjunct),
		!.


%%	local_cut(+Goal,+ListIn,?ListOut)
%
%	This predicated can be called as "local_cut(Goal)" in DCG notation. It disables backtracking into the Goal,
%	but does not commit to the choices made in the parent frame.

local_cut(Goal,ListIn,ListOut) :-
	call(Goal,ListIn,ListOut),
	!.
