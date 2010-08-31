:- module(dcg_simple,[]).

:- use_module(library(pldoc)).
:- use_module(naproche(gulp4swi)).
:- use_module(naproche(dcg_lexicon)).
:- use_module(naproche(fo_grammar)).

/** 	<module> Naproche Grammar
 *	
 *	This module describes the definite clause grammar for the Naproche language.
 *	@author Marcos Cramer
*/ 

%=====================
% Macrostructure
%=====================


% BASICS

% A Naproche text consists of axioms, lemmas, theorems and text.

naproche_text --> axiom, naproche_text.
naproche_text --> lemma, naproche_text.
naproche_text --> theorem, naproche_text.
naproche_text --> assumption_naproche_text, naproche_text.
naproche_text --> text(type~normal..empty~no), naproche_text.
naproche_text --> [].

naproche_text_not_starting_with_text --> axiom, naproche_text.
naproche_text_not_starting_with_text --> lemma, naproche_text.
naproche_text_not_starting_with_text --> theorem, naproche_text.
naproche_text_not_starting_with_text --> assumption_naproche_text, naproche_text.
naproche_text_not_starting_with_text --> [].


% Text contains statements, definitions, assumptions, assumption closings, cases and case closings. 
% A text can only be empty, if the GULP feature 'empty' is 'possible'.

text(type~Type) --> [sentence(_,Content)], {statement(Content,[]), !},  text(type~Type..empty~possible).
text(type~Type) --> definition_text,  text(type~Type..empty~possible).
text(type~Type) --> { \+ Type = all_ass_closed },[sentence(_,Content)], {assumption(Content,[]), !},  text(type~Type..empty~possible).
text(type~Type) --> assumption_text, text(type~Type..empty~possible).

% during any text, we may introduce cases, close them with a closing sentence and continue with text.
text(type~Type) --> cases_text(type~Type), text(type~Type..empty~possible).

% at the end of a proof, however, if we aren't in a case already, we may introduce cases, and don't need to close them.
text(type~Type) --> { \+ Type = in_case}, cases(type~Type).

text(type~theorem) --> lemma, text(type~theorem..empty~possible).
text(empty~possible) --> [].


% STATEMENTS
%
% A statement has a proposition coordination as its core, starts with a statement trigger (which can be empty), 
% and can have a reference.
% Alternatively, it can just be the 'sentence' "contradiction" or "trivial".

statement --> trigger(type~statement), proposition_coord(mode~finite).
statement --> trigger(type~statement), reference, proposition_coord(mode~finite).
statement --> trigger(type~statement), proposition_coord(mode~finite), reference.
statement --> reference.
statement --> [trivial].
statement --> [contradiction].
statement --> [contradiction], reference.

% REFERENCES
%
% A reference refers to an axiom, lemma, theorem or proof method, (optionally from a different Naproche text).

reference --> [by], [axiom], identifier, optional_comma.
% reference --> [by], [axiom], identifier, [in], naproche_text_id, optional_comma.
reference --> [by], [lemma], identifier, optional_comma.
% reference --> [by], [lemma], identifier, [in], naproche_text_id, optional_comma.
reference --> [by], [theorem], identifier, optional_comma.
% reference --> [by], [theorem], identifier, [in], naproche_text_id, optional_comma.
reference --> [by], [definition], identifier, optional_comma.
% reference --> [by], [definition], identifier, [in], naproche_text_id, optional_comma.
reference --> [by], [induction], optional_comma.


% DEFINITIONS

% Definition texts can consist of one-sentence definitions:

definition_text --> [sentence(_,Content)], {definition(Content,[]), !}.

% One can give a definition a name, e.g. "Definition 12":

definition_text --> [sentence(_,[definition,_])], [sentence(_,Content)], {definition(Content,[]),!}.

% Currently the only multi-sentence definitions are recursive definitions of function symbols (e.g. in Naproche-Landau
% without talk about sets and functions).
% fo_induction_start and fo_induction_step are of the required form to for a recursive definition of FunctionSymbol.

definition_text --> 
	[sentence(_,Content)], 
        {
	Content = [define, math([FunctionSymbol]), recursively],
	fo_function_symbol(FunctionSymbolTree,[FunctionSymbol],[]),
	FunctionSymbolTree = name~FunctionSymbolName
	},
	[sentence(_,[math(BaseCase)])], 
	[sentence(_,[math(SuccCase)])], 
	{
	fo_induction_start(FunctionSymbolName,Number,_,_,_,BaseCase,[]),
	fo_induction_step(FunctionSymbolName,Number,_,_,_,SuccCase,[])
	}.

% Recursive definitions with a name:

definition_text --> 
	[sentence(_,[definition,_])], 
	[sentence(_,Content)], 
        {
	Content = [define, math([FunctionSymbol]), recursively],
	fo_function_symbol(FunctionSymbolTree,[FunctionSymbol],[]),
	FunctionSymbolTree = name~FunctionSymbolName
	},
	[sentence(_,[math(BaseCase)])], 
	[sentence(_,[math(SuccCase)])], 
	{
	fo_induction_start(FunctionSymbolName,Number,_,_,_,BaseCase,[]),
	fo_induction_step(FunctionSymbolName,Number,_,_,_,SuccCase,[])
	}.


% Here is how to define new predicates symbols, new adjectives, new nouns and new verbs.

definition --> [define], definiendum, iff, proposition_coord(mode~finite).

% Here is how to explicitly define new function symbols, including 0-ary (i.e. constants).
% fo_free_function(F,_,_) is defined to be any term of the form F(x1, ..., xn), where F is an n-ary function_symbol.
% fo_term_without(F,_,_) is defined to be any term not containing the function symbol F.

definition --> [define], [math(X)], [to], [be], [math(Y)], {fo_free_function(F,_,_,X,[]), fo_term_without(F,_,_,Y,[])}.

definiendum --> [math(X)], {fo_free_predicate_symbol(_,_,X,[])}.
definiendum --> [math(X)], verb(transitive~copula..mode~to-infinitive), adjective, {fo_variable(_,X,[])}.
definiendum --> [math(X)], verb(transitive~copula..mode~to-infinitive), specifier(indefinite), noun, {fo_variable(_,X,[])}.
definiendum --> [math(X)], verb(transitive~minus..mode~to-infinitive), {fo_variable(_,X,[])}.
definiendum --> [math(X)], verb(transitive~plus..mode~to-infinitive), [math(Y)], {fo_variable(_,X,[]), fo_variable(_,Y,[]), \+ X=Y}.

% ASSUMPTIONS

% assumption_naproche_text starts with an assumption and ends with an assumption closing.
% In between there is naproche_text.

assumption_naproche_text --> [sentence(_,Content)], {assumption(Content,[]),!}, naproche_text_all_ass_closed, [sentence(_,ClosingContent)], {closing(ClosingContent,[]),!}.

% assumption_text starts with an assumption and ends with an assumption closing.
% In between there is text.

assumption_text --> [sentence(_,Content)], {assumption(Content,[]),!}, text(type~all_ass_closed), [sentence(_,ClosingContent)], {closing(ClosingContent,[]),!}.

% An assumption starts with an assumption trigger.

assumption --> trigger(type~ass..mode~Mode), proposition_coord(mode~Mode).

% Alternatively, an assumption can be used to introduce new variables (possibly fixing some property for them, e.g. using a such-that-clause).

assumption --> trigger(type~variable_declaration), variable_list_bar.
assumption --> [let], variable_list, [be], [given].

% naproche_text_all_ass_closed is just like naproche_text, but without unclosed assumptions.

naproche_text_all_ass_closed --> axiom, naproche_text_all_ass_closed.
naproche_text_all_ass_closed --> lemma, naproche_text_all_ass_closed.
naproche_text_all_ass_closed --> theorem, naproche_text_all_ass_closed.
naproche_text_all_ass_closed --> assumption_naproche_text, naproche_text_all_ass_closed.
naproche_text_all_ass_closed --> text(type~all_ass_closed..empty~no), naproche_text_all_ass_closed.
naproche_text_all_ass_closed --> [].

naproche_text_not_starting_with_text_all_ass_closed --> axiom, naproche_text_all_ass_closed.
naproche_text_not_starting_with_text_all_ass_closed --> lemma, naproche_text_all_ass_closed.
naproche_text_not_starting_with_text_all_ass_closed --> theorem, naproche_text_all_ass_closed.
naproche_text_not_starting_with_text_all_ass_closed --> assumption_naproche_text, naproche_text_all_ass_closed.
naproche_text_not_starting_with_text_all_ass_closed --> [].

% closing starts with an assumption closing trigger.

closing --> trigger(type~ass_closing), proposition_coord(mode~finite).
closing --> trigger(type~ass_closing), reference, proposition_coord(mode~finite).
closing --> trigger(type~ass_closing), proposition_coord(mode~finite), reference.


% CASES

% cases_text can either be used for listing mutually exclusive cases, or for proving something by case distinction.
% Here is how mutually exclusive cases work:

cases_text(_) --> [sentence(_,Content)], {trigger(type~statement,Content,[precisely,one,of,the,following,cases,holds]),!}, exclusive_cases.

cases_text(_) --> [sentence(_,Content)], {trigger(type~statement,Content,[at,most,one,of,the,following,cases,holds]),!}, exclusive_cases.

% Here is how proofs by case distinction work:
% A cases text consists of cases and a case closing.

cases_text(type~Type) --> cases(type~Type), case_closing.

% exclusive_cases is a list of the mutually exclusive cases. Each of the cases can be prefixed by a sentence of the form "Case X".

exclusive_cases --> [sentence(_,[case,_])], [sentence(_,Content)], exclusive_cases, {proposition_coord(mode~finite,Content,[]),!}.
exclusive_cases --> [sentence(_,Content)], exclusive_cases, {proposition_coord(mode~finite,Content,[]),!}.
exclusive_cases --> [].

% cases presents a case with its restriction and its scope, and then continues with the next case.

cases(type~Type) --> 
	case_introduction(type~Type), 
	[sentence(_,[case,_])], 
	[sentence(_,Content)], 
	{proposition_coord(mode~finite,Content,[]),!}, 
	text(type~in_case..empty~no), 
	cases(type~rest-list).

cases(type~rest-list) --> 
	[sentence(_,[case,_])], 
	[sentence(_,Content)], 
	{proposition_coord(mode~finite,Content,[]),!}, 
	text(type~in_case..empty~no).

% case_introduction contains either a sentence saying "there are N cases" (N>=2) or nothing. if we have entered a case, this sentence is obligatory. on the othe other hand, if we're just presenting the second case, the sentence isn't permitted.

case_introduction(subtype~SubType) --> { \+ SubType = rest-list }, trigger(type~statement), [there], [are], number, [cases].
case_introduction(type~Type) --> { \+ Type = in_case }, [].
case_introduction(subtype~rest-list) --> [].

% case_closing starts with case closing trigger (and possibly a sentence trigger).

case_closing --> trigger(type~statement), trigger(type~case_closing), proposition_coord(mode~finite), ['.'].


% AXIOMS, LEMMAS AND THEOREMS

% An axiom consists of a heading followed by text.

axiom --> [sentence(_,[axiom])], [sentence(_,Content)], {proposition_coord(mode~finite,Content,[]), !}.
axiom --> [sentence(_,[axiom,_])], [sentence(_,Content)], {proposition_coord(mode~finite,Content,[]), !}.

% A lemma consists of a heading, a goal text, the marker "Proof", a body text and the marker "Qed".

lemma --> [sentence(_,[lemma])], text(type~normal..empty~no), [sentence(_,[proof])], text(type~normal..empty~no), [sentence(_,[qed])].
lemma --> [sentence(_,[lemma,_])], text(type~normal..empty~no), [sentence(_,[proof])], text(type~normal..empty~no), [sentence(_,[qed])].

% A lemma consists of a heading, a goal text, the marker "Proof", a body text (which possibly includes lemmas) and the marker "Qed".

theorem --> [sentence(_,[theorem])], text(type~normal..empty~no), [sentence(_,[proof])], text(type~theorem..empty~no), [sentence(_,[qed])].
theorem --> [sentence(_,[theorem,_])], text(type~normal..empty~no), [sentence(_,[proof])], text(type~theorem..empty~no), [sentence(_,[qed])].


%=====================
% proposition_coord
%=====================

% proposition_coord can be either a simple sentence_coord, or a number of sentence_coords linked with "if...then" or "iff".
% The gulp feature "mode" can take three values: "that" indicates that the subclause is to be prefixed with "that", "infinitive"
% indicates that the verb of the sentence is in the infinitive, and "finite" is the normal case.
% The gulp feature "that" defines whether the proposition_coord is a clause subordinated with a "that". It can tak values "plus" or "minus".

proposition_coord(mode~Mode) --> sentence_coord(mode~Mode).
proposition_coord(mode~finite) --> [if], sentence_coord(mode~finite), optional_comma, [then], trigger(type~conseq), sentence_coord(mode~finite).
proposition_coord(mode~that) --> [that], [if], sentence_coord(mode~finite), optional_comma, [then], trigger(type~conseq), sentence_coord(mode~finite).
proposition_coord(mode~Mode) --> sentence_coord(mode~Mode), optional_comma, iff, sentence_coord(mode~finite).
proposition_coord(mode~Mode) --> sentence_coord(mode~Mode), optional_comma, [if], sentence_coord(mode~finite).

%=====================
% sentence_coord
%=====================

% sentence_coord works like in Attempto (with "i.e." added). It links a number of topicalised_sentences with
% "and", "or", ", and", ", or" and "i.e." in such a way that the bracketing is unambiguous.
% No coordination is allowed in infinitive mode.

sentence_coord(mode~infinitive) --> !, composite_sentence(mode~infinitive).

sentence_coord(mode~Mode) --> sentence_coord_0(mode~Mode), sentence_coord_tail(mode~Mode).
sentence_coord_tail(mode~Mode) --> [','], ['i.e.'], sentence_coord(mode~Mode).
sentence_coord_tail(mode~_) --> [].

sentence_coord_0(mode~Mode) --> sentence_coord_1(mode~Mode), sentence_coord_0_tail(mode~Mode).
sentence_coord_0_tail(mode~Mode) --> [','], [or], sentence_coord_0(mode~Mode).
sentence_coord_0_tail(mode~_) --> [].

sentence_coord_1(mode~Mode) --> sentence_coord_2(mode~Mode), sentence_coord_1_tail(mode~Mode).
sentence_coord_1_tail(mode~Mode) --> [','], [and], trigger(type~statement), sentence_coord_1(mode~Mode).
sentence_coord_1_tail(mode~_) --> [].

sentence_coord_2(mode~Mode) --> sentence_coord_3(mode~Mode), sentence_coord_2_tail(mode~Mode).
sentence_coord_2_tail(mode~Mode) --> [or], sentence_coord_2(mode~Mode).
sentence_coord_2_tail(mode~_) --> [].

sentence_coord_3(mode~that) --> [that], topicalised_sentence(mode~finite), sentence_coord_3_tail(mode~that).
sentence_coord_3(mode~Mode) --> topicalised_sentence(mode~Mode), sentence_coord_3_tail(mode~finite).
sentence_coord_3_tail(mode~Mode) --> [and], trigger(type~statement), sentence_coord_3(mode~Mode).
sentence_coord_3_tail(mode~Mode) --> [','], trigger(type~statement), sentence_coord_3(mode~Mode).
sentence_coord_3_tail(mode~_) --> [].

% A topicalised_sentence can be a quantified statement, two composite_sentences linked with "implies that", or just one composite_sentence. 

topicalised_sentence(mode~Mode) --> existential_topic(mode~Mode).
topicalised_sentence(mode~finite) --> universal_topic, optional_comma, proposition_coord(mode~finite).
topicalised_sentence(mode~finite) --> composite_sentence(mode~finite), optional_comma, implies, composite_sentence(mode~finite).
topicalised_sentence(mode~Mode) --> composite_sentence(mode~Mode).

existential_topic(mode~Mode) --> quantifier(type~existential..mode~Mode), specifier(indefinite), nbar. 
existential_topic(mode~Mode) --> quantifier(type~existential..mode~Mode), specifier(indefinite), variable_list_bar. 
existential_topic(mode~Mode) --> quantifier(type~existential..mode~Mode), variable_list_bar.
existential_topic(mode~Mode) --> quantifier(type~existential..mode~Mode), plural_noun(_), variable_list, optional_comma, such_that_clause.
existential_topic(mode~Mode) --> quantifier(type~existential..mode~Mode), specifier(negative), nbar. 
existential_topic(mode~Mode) --> quantifier(type~existential..mode~Mode), specifier(negative), variable_list_bar. 

universal_topic --> quantifier(type~universal..mode~finite), nbar.
universal_topic --> quantifier(type~universal..mode~finite), variable_list_bar.

variable_list_bar --> variable_list, optional_comma, such_that_clause.
variable_list_bar --> variable_list.

variable_list --> [math(X)], variable_list_tail, {fo_variable(_,X,[])}.
variable_list --> [math(X)], { \+ X = [], fo_var_list(_,_,X,[]) }.
variable_list --> [math(FunctionTerm)], variable_list_tail, {fo_free_function(_,_,_,FunctionTerm,[])}.
variable_list_tail --> optional_comma, [math(X)], variable_list_tail, {fo_variable(_,X,[])}.
variable_list_tail --> optional_comma, [math(FunctionTerm)], variable_list_tail, {fo_free_function(_,_,_,FunctionTerm,[])}.
variable_list_tail --> [].

such_that_clause --> [such], [that], proposition_coord(mode~finite).

% A composite_sentence is either a simple sentence, a formula or a proposition_coord prefixed with a sentence_init.
% By the constructions that contain composite_sentence, it is given that the "mode" feature can here only take the
% values "finite" and "infinitive" (not "that").

composite_sentence(mode~Mode) --> sentence(mode~Mode).
composite_sentence(mode~finite) --> sentence_init, proposition_coord(mode~that).
composite_sentence(mode~_) --> trigger(type~formula), [math(X)], {fo_formula(_,_,X,[])}.
composite_sentence(mode~Mode) --> metasentence(mode~Mode).

sentence_init --> [it], [is], [false].
sentence_init --> [it], [is], [not], [the], [case].
sentence_init --> [it], [is], [the], [case].

% A sentence is just a noun phrase followed by a verb phrase

sentence(mode~Mode) --> np, vp(mode~Mode).


%=====================
% Noun phrases
%=====================

np --> [math(X)], {fo_term(_,_,X,[])}.
% np --> pronoun.
np --> specifier(_), nbar.

nbar --> adjective, nbar.
nbar --> nbar1.
nbar1 --> noun, variablebar.
nbar1 --> noun.
nbar1 --> variablebar.

variablebar --> [math(X)], such_that_clause, {fo_variable(_,X,[])}.
variablebar --> [math(X)], {fo_variable(_,X,[])}.


%=====================
% Verb phrases
%=====================

vp(mode~Mode) --> vbar(mode~Mode).

vp(mode~finite) --> [does], [not], vbar(mode~infinitive).
vp(mode~infinitive) --> [not], vbar(mode~infinitive).

vp(mode~Mode) --> verb(transitive~copula..mode~Mode), adjective.
vp(mode~Mode) --> verb(transitive~copula..mode~Mode), [not], adjective.
vp(mode~Mode) --> verb(transitive~copula..mode~Mode), specifier(indefinite), nbar.
vp(mode~Mode) --> verb(transitive~copula..mode~Mode), [not], specifier(indefinite), nbar.

vbar(mode~Mode) --> verb(transitive~plus..mode~Mode), np.
vbar(mode~Mode) --> verb(transitive~minus..mode~Mode).


%====================
% Metasentences
%====================

metasentence(mode~Mode) --> trigger(type~statement), meta_np(_), meta_vp(mode~Mode).

metasentence(mode~Mode) --> [case,Id1,and,case,Id2], verb(mode~Mode), [inconsistent],
	{getval(refids,RefIds), member(ref(_,case(Id1)),RefIds), member(ref(_,case(Id2)),RefIds)}.

meta_np(and) --> [case,Id,','], {getval(refids,RefIds), member(ref(_,case(Id)),RefIds)}, meta_np(and).
meta_np(or) --> [case,Id,','], {getval(refids,RefIds), member(ref(_,case(Id)),RefIds)}, meta_np(or).
meta_np(xor_rest) --> [case,Id,','], {getval(refids,RefIds), member(ref(_,case(Id)),RefIds)}, meta_np(xor_rest).
meta_np(at_most_one_rest) --> [case,Id,','], {getval(refids,RefIds), member(ref(_,case(Id)),RefIds)}, meta_np(at_most_one_rest).

meta_np(and) --> [case,Id,and], {getval(refids,RefIds), member(ref(_,case(Id)),RefIds)}, meta_np(and).
meta_np(or) --> [case,Id,or], {getval(refids,RefIds), member(ref(_,case(Id)),RefIds)}, meta_np(or).
meta_np(xor_rest) --> [case,Id,and], {getval(refids,RefIds), member(ref(_,case(Id)),RefIds)}, meta_np(xor_rest).
meta_np(at_most_one_rest) --> [case,Id,and], {getval(refids,RefIds), member(ref(_,case(Id)),RefIds)}, meta_np(at_most_one_rest).

meta_np(xor) --> [precisely,one,of],meta_np(xor_rest).
meta_np(at_most_one) --> [at,most,one,of],meta_np(at_most_one_rest).
meta_np(_) --> [].

meta_vp(mode~Mode) --> correct(mode~Mode).
meta_vp(mode~Mode) --> false(mode~Mode).

correct(mode~Mode) --> verb(mode~Mode), [true].
correct(mode~Mode) --> verb(mode~Mode), [correct].
correct(mode~finite) --> [holds].
correct(mode~finite) --> [hold].

false(mode~Mode) --> verb(mode~Mode), [false].
false(mode~Mode) --> verb(mode~Mode), [incorrect].
false(mode~finite) --> [does,not,hold].
false(mode~finite) --> [do,not,hold].


%=====================
% Words and triggers
%=====================

noun -->
	[Noun],
	{
	dcg_lexicon([Noun],noun)
	},
	!.

pronoun -->
	[Pronoun],
	{
	dcg_lexicon([Pronoun],pronoun)
	},
	!.

adjective -->
	[Adjective],
	{
	dcg_lexicon([Adjective],adjective)
	},
	!.

number -->
	{
	dcg_lexicon([Number],number)
	},
	[Number],
	!.

verb(transitive~Transitive..mode~Mode) -->
	{
	dcg_lexicon(ConjugatedVerb,verb,transitive~Transitive..mode~Mode,_)
	},
	ConjugatedVerb,
	!.

specifier(Type) -->
	{
	dcg_lexicon(Specifier,specifier,Type)
	},
	Specifier.

quantifier(Type) -->
	{
	dcg_lexicon(Quantifier,quantifier,Type)
	},
	Quantifier.

trigger(Type) -->
	{
	dcg_lexicon(Trigger,trigger,Type)
	},
	Trigger,
	optional_comma.

plural_noun(Noun) -->
        [PluralNoun],
	{
	dcg_lexicon([Noun],noun),
	atom_concat(Noun,'s',PluralNoun)
	},
	!.


optional_comma --> [','].
optional_comma --> [].

iff --> [iff].
iff --> [if], [and], [only], [if].

implies --> [implies, that].
implies --> [implies].

identifier --> [_].
