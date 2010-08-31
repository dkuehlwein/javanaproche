%%  text(+PRS,?Features)
%
%	A text can contain different kinds of construct, depending on its level. It may always contain simple statements.
%
%   @param Features is a feature list with features "in_proof" (taking values "no", "lemma" and "theorem"), "in_case" (taking values "yes" and "no") and "empty" (taking values "yes" and "no").

text(PRS,F) --->
	{ F = in_proof~no..in_case~no },
	{[ add_cond(PRS,AxiomPRS ==> ConseqPRS) ]},
	axiom(AxiomPRS),
	text(ConseqPRS,F).

text(PRS,F) --->
    theorem(PRS,TheoremType),
	{ 
	\+ F = in_proof~lemma,
	( TheroemType = theorem -> ( F = in_proof~no..in_case~no ) )
	},
    text(PRS,F).

text(PRS,F) --->
    definition(PRS),
    text(PRS,F).

text(PRS,F) --->
	{[ add_cond(PRS,AssPRS ==> ConclPRS) ]},
	[assumption(AssPRS)],
	{ change_feature(F,type,all_ass_closed,F0) },
	text(ConclPRS,F0),
	optional_closing_and_text(PRS,F).

text(PRS,F) --->
	{ F = in_proof~no..in_case~no },
	[var_type_fix],
	text(PRS,F).

text(PRS,F) --->
	{[ add_cond(PRS,StatementPRS) ]},
	[statement(StatementPRS)],
	{[ check(StatementPRS) ]},
	text(PRS,F).

text(PRS,F) --->
	exclusive_cases(PRS),
	text(PRS,F).

text(PRS,F) --->
	cases(PRS,F),	
	optional_case_closing_and_text(PRS,F).

text(PRS,empty~yes) ---> 
	[].


%%  theorem(+PRS,-TheoremType)
%
%   A theorem consists of a heading, a goal text, the marker "Proof", a body text (which possibly includes lemmas) and the marker "Qed".

theorem(PRS,TheoremType) --->
	[heading(TheoremType)],
	assumptions(PRS,ConclPRS),
	{[ add_cond(ConclPRS,theorem(TheoremType,GoalPRS,ProofPRS)) ]},
	statements(GoalPRS),
	goal_end(TheoremType),
	[proof],
	text(ProofPRS,in_proof~TheoremType..in_case~no..empty~no),
	{[ check(GoalPRS) ]},
	[proof_end].


goal_end(TheoremType) --->
	[end(TheoremType)].

goal_end(_) --->
    [].
