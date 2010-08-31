:- module(prs_export,
          [prs_to_xml/2,
           write_prs_xhtml_to_file/3,
           write_prs_xhtml/1,
           display_prs/1]).

:- ensure_loaded(naproche(translation_tptp)).

%% prs_to_xml(?PRS, ?XML) is det.
%
% Converts between GULP and XML representations of a PRS.
%
% To write a PRS to a file as XML:
%
%==
% % PRS is of the form id~_..drefs~_..mrefs~_..conds~_..rrefs~_
% ...
% prs_to_xml(PRS, XML),
% open('output-file.xml', write, Stream),
% xml_write(Stream, XML, []),
% close(Stream),
% ...
%==
%
% To read a PRS from an XML file:
%
%==
% ...
% xml_doc('input-file.xml', XML),
% prs_to_xml(PRS, XML),
% ...
%==
%
%@ author: Mona Rahn


prs_to_xml(PRS, XML) :-
  
	  % get PRS
	  (var(PRS); is_prs(PRS)),
	  PRS = id~Id..
	        drefs~Drefs..
	        mrefs~Mrefs..
	        conds~Conds..
	        rrefs~Rrefs,
  
	  % make XML PRS
	  XML = element(prs, [], [	
	  			element(id,	[],	[AtomId]),
  				element(drefs, 	[], 	XDrefs),
                               	element(mrefs, 	[], 	XMrefs),
                               	element(conds, 	[], 	XConds),
                               	element(rrefs, 	[], 	XRrefs)
				]
               		),
  
	  % convert PRS to XML
	  term_to_atom(Id,AtomId),
	  refs_list_to_xml(dref,Drefs,XDrefs),
	  refs_list_to_xml(rref,Rrefs,XRrefs),
	  dobsod_list_to_xml(mref,Mrefs,XMrefs),
	  condition_list_to_xml(Conds,XConds),	
	  !.

% converts between GULP and XML representations of dref- or rref-lists
refs_list_to_xml(_RefType,[],[]) :- !.
refs_list_to_xml(RefType,[H|T],[element(RefType, [], [XMLH]) | XMLT]) :-
	% Drefs and Mrefs are assumed to be terms.
	term_to_atom(H,XMLH),
	refs_list_to_xml(RefType,T,XMLT).

% --------------------------------------------------------------------

% % condition_list_to_xml(?ConditionList:list(DOBSOD),?XMLConditionList:list(XML)) 
%
% converts between GULP and XML representations of a list of conditions


condition_list_to_xml([], []) :-!.
condition_list_to_xml([Feature|T], [element(cond, [], [ElementContent])|Es]) :-
	condition_to_xml(Feature, ElementContent),
	condition_list_to_xml(T, Es).


% implication
condition_to_xml(Premise => Conclusion, element(implication, [], [PremiseElement, ConclusionElement]) ) :-
	!,
	PremiseElement = element(premise, [], [LeftSideElement]),
	ConclusionElement = element(conclusion, [], [RightSideElement]),
	prs_to_xml(Premise, LeftSideElement),
	prs_to_xml(Conclusion, RightSideElement).

% reversed implication
condition_to_xml(Conclusion <= Premise, element(reversed_implication, [], [ConclusionElement, PremiseElement]) ) :-
	!,
	PremiseElement = element(conclusion, [], [LeftSideElement]),
	ConclusionElement = element(premise, [], [RightSideElement]),
	prs_to_xml(Conclusion, LeftSideElement),
	prs_to_xml(Premise, RightSideElement).

% biconditional
condition_to_xml(Left <=> Right, element(biconditional, [], [LeftElement, RightElement]) ) :-
	!,
	LeftElement = element(left, [], [LeftSideElement]),
	RightElement = element(right, [], [RightSideElement]),
	prs_to_xml(Left, LeftSideElement),
	prs_to_xml(Right, RightSideElement).

% relation definition
condition_to_xml(Definiendum := Definiens, element(definition, [], [DefiniendumElement, DefiniensElement]) ) :-
	!,
	DefiniendumElement = element(definiendum, [], [LeftSideElement]),
	DefiniensElement = element(definiens, [], [RightSideElement]),
	prs_to_xml(Definiendum, LeftSideElement),
	prs_to_xml(Definiens, RightSideElement).

% function definition
condition_to_xml(FunctionSymbol :: Domain => Definiens, element(function_definition, [], [FunctionElement, DomainElement, DefiniensElement]) ) :-
	!,
	FunctionElement = element(function_symbol, [], [FunctionSymbol]),
	DomainElement = element(domain, [], [LeftSideElement]),
	DefiniensElement = element(definiens, [], [RightSideElement]),
	prs_to_xml(Domain, LeftSideElement),
	prs_to_xml(Definiens, RightSideElement).

% assumption
condition_to_xml(Premise ==> Conclusion, element(implication, [], [PremiseElement, ConclusionElement]) ) :-
	!,
	PremiseElement = element(premise, [], [LeftSideElement]),
	ConclusionElement = element(conclusion, [], [RightSideElement]),
	prs_to_xml(Premise, LeftSideElement),
	prs_to_xml(Conclusion, RightSideElement).

/*
% the
condition_to_xml(the(Dref,PRS), element(the, [], [DrefElement, PRSElement]) ) :-
	!,
	DrefElement = element(dref, [], [XMLDref]),
	PRSElement = element(conclusion, [], [XMLPRSElement]),
	term_to_atom(Dref,XMLDref),
	prs_to_xml(PRS, XMLPRSElement).
*/

% disjunction
condition_to_xml(A v B, element(disjunction, [], [ElementA, ElementB]) ) :-
	!,
	ElementA = element(disjunct, [], [XMLA]),
	ElementB = element(disjunct, [], [XMLB]),
	prs_to_xml(A, XMLA),
	prs_to_xml(B, XMLB).

% exclusive disjunction
condition_to_xml(><(PrsList), element(exclusive_disjunction, [], XMLPrsList) ) :-
	!,
	exclusive_disjunction_to_xml(PrsList,XMLPrsList).

% negated PRS
condition_to_xml(neg(A), element(negation, [], [XMLA]) ) :-
	!,
	prs_to_xml(A, XMLA).

% contradiction
condition_to_xml(contradiction, element(contradiction, [], []) ) :-
	!.

% predicate(_,_)
condition_to_xml(predicate(Dref,Predicate), element(predication, [], [DrefElement, PredicateElement]) ) :-
	!,
	DrefElement = element(dref, [], [XMLDref]),
	PredicateElement = element(predicate, [], [Predicate]),
	term_to_atom(Dref,XMLDref).

% predicate(_,_,_)
condition_to_xml(predicate(Dref1,Dref2,Predicate), element(predication, [], [DrefElement1, DrefElement2, PredicateElement]) ) :-
	!,
	DrefElement1 = element(dref, [], [XMLDref1]),
	DrefElement2 = element(dref, [], [XMLDref2]),
	PredicateElement = element(predicate, [], [Predicate]),
	term_to_atom(Dref1,XMLDref1),
	term_to_atom(Dref2,XMLDref2).

% holds
condition_to_xml(holds(Rref), Atom) :-
	Atom = element(holds,[],[AtomRref]),
	term_to_atom(Rref,AtomRref),
	!.

% math_id
condition_to_xml(math_id(Rref,Math), Atom) :-
	Atom = element(math_id,[],[
				element(dref,[],[AtomRref]),
				XMLMath]),
	term_to_atom(Rref,AtomRref),
	dobsod_to_xml(mref,Math,XMLMath),
	!.

% another PRS
condition_to_xml(What, XMLPRS) :-
  	(is_prs(What); var(What)),
	  prs_to_xml(What, XMLPRS).

% -------------------------------------------------------------
% exclusive_disjunction_to_xml
% is an internal predicate for condition_to_xml for processing an exclusive disjunction
exclusive_disjunction_to_xml([],[]):- !.
exclusive_disjunction_to_xml([Head|Tail],[HeadElement|TailElements]):-
	HeadElement = element(disjunct, [], [XMLHead]),
	prs_to_xml(Head,XMLHead),
	exclusive_disjunction_to_xml(Tail,TailElements).

% ---------------------------------------------------------------

%% dobsod_list_to_xml(+Type,?DobsodList:list(DOBSOD),?XMLDobsodList:list(XML))
%
% converts a list of DOBSODs into a list in xml format (or vice versa)
% the elements of XMLDobsodList are of the type
% 	element(Type,[],...).

dobsod_list_to_xml(_DobsodType,[],[]) :- !.
dobsod_list_to_xml(DobsodType,DobsodList,XMLDobsodList) :-
	DobsodList = [H|T],
	XMLDobsodList = [XMLH|XMLT],
	dobsod_to_xml(DobsodType,H,XMLH),
	dobsod_list_to_xml(DobsodType,T,XMLT).

dobsod_to_xml(DobsodType,Dobsod,XMLDobsod):-
	Dobsod = type~Type..name~Name..arity~Arity..args~Args,
	XMLDobsod = 
		element(DobsodType,[],[
			element(name,[],[Name]),
			element(type,[],[Type]),
			element(arity,[],[AtomArity]),
			element(args,[],XMLArgs)
			]
		),
	term_to_atom(Arity,AtomArity),
	dobsod_list_to_xml(arg, Args,XMLArgs).


/*
% make_atom(+Term,-Atom) converts Term to an atom if it isn't already an atom 
make_atom(Atom,Atom) :-
	atom(Atom),!.
make_atom(Term,Atom) :-
	term_to_atom(Term,Atom).
*/
% -----------------------------------------------------------------


%% prs_xhtml(+PRS, +ViewMode, -XHTML)
%
% Converts a GULP encoded PRS to an XHTML term. ViewMode can be "user", in which
% case the features accbefore and accafter are ignored, or it can be "debug", in
% which case they are included into the XHTML term.
%
% We use HTML definition lists to represent feature value structures.
%
% Please note that XHTML is a code _fragment_, i.e. there is no doctype, and no
% head or body tags. This is necessary in order to keep the predicate generic,
% so that we can embed more than one PRS in a page if needed.
%
% If you need to write a complete file look at prs_xhtml_to_file/2.
%
% If you need to write a PRS to a file just to look at it in a browser use
% display_prs/1.
prs_xhtml(PRS, ViewMode, XHTML) :-
  	%is_prs(PRS),
  	PRS = id~Id..
        	drefs~Drefs..
        	mrefs~Mrefs..
        	conds~Conds..
        	rrefs~Rrefs..
		accbefore~AccBefore..
		accafter~AccAfter,
	% isnt Id already an atom?
	term_to_atom(Id,CoercedId),
  	maplist(xhtml_refs, [Drefs, Mrefs, Rrefs],
                      ['Drefs:', 'Mrefs:', 'Rrefs:'],
                      [XDrefs, XMrefs, XRrefs]),
	% If conds is empty, write '-', else use normal construction
	( Conds == [] ->
  	XConds = [element(dt, [], ['Conds:']), element(dd, [], ['—'])]	
	;
	xhtml_conds(Conds, ['Conds:'], ViewMode, XConds)
	),
	% If accbefore is empty, write '-', else use normal construction
	( AccBefore == [] ->
  	XAccBefore = [element(dt, [], ['AccBe:']), element(dd, [], ['—'])]	
	;
	xhtml_conds(AccBefore, ['AccBe:'], ViewMode, XAccBefore)
	),
	% If accafter is empty, write '-', else use normal construction
	( AccAfter == [] ->
  	XAccAfter = [element(dt, [], ['AccAf:']), element(dd, [], ['—'])]	
	;
	xhtml_conds(AccAfter, ['AccAf:'], ViewMode, XAccAfter)
	),
	( ViewMode = user ->
  	append([XDrefs, XMrefs, XConds, XRrefs], Features);
  	append([XAccBefore,XDrefs, XMrefs, XConds, XAccAfter, XRrefs], Features)
	),
	% Changed Id
  	XHTML = element(div, [class=prs], [element(span, [class=label], [CoercedId]),
                                     element(dl, [], Features)
                                    ]).

xhtml_refs(Refs, Label, Xrefs) :-
  (   Refs == []
  ->  XRefsStr = '—'
  ;   ( strip_math(Refs, CleanRefs),
        concat_atom(CleanRefs, ', ', XRefsStr) )
  ),
  Xrefs = [element(dt, [], [Label]), element(dd, [], [XRefsStr])].

strip_math([], []).
strip_math([H|T],[N|NT]) :-
	dobsod_to_atom(H,N),
	strip_math(T,NT).
% Old code, should not be needed anymore
%strip_math([math(H)|T], [N|NewT]) :-
%  dobsod_to_atom(H,N),
%  name(N, H),
%  strip_math(T, NewT).
strip_math([H|T], [H|NewT]) :-
	(atomic(H) ; is_list(H)),
  	strip_math(T, NewT).
strip_math([H|T], [CH|NewT]) :-
	term_to_atom(H,CH),
  	strip_math(T, NewT).



xhtml_conds(Conds, Prefix, ViewMode, [element(dt, [], Prefix),
                                    element(dd, [], Contents)]) :-
  xhtml_conds_x(Conds, ViewMode, Contents).

% implications
xhtml_conds_x([Premise => Conclusion|Conds], ViewMode, [element(div, [class=connected], [PremiseElement,
                                                                               element(div, [class=connector], ['→']),
                                                                               ConclusionElement])
                                              | Elements]) :-
  is_prs(Premise),
  is_prs(Conclusion),
  !,
  prs_xhtml(Premise, ViewMode, PremiseElement),
  prs_xhtml(Conclusion, ViewMode, ConclusionElement),
  xhtml_conds_x(Conds, ViewMode, Elements).

% implications not
xhtml_conds_x([Premise => neg(Conclusion)|Conds], ViewMode, [element(div, [class=connected], [PremiseElement,
                                                                                element(div, [class=connector], ['?']),
                                                                                element(div, [class=connector], ['¬']),
                                                                                ConclusionElement])
                                              | Elements]) :-
  is_prs(Premise),
  is_prs(Conclusion),
  !,
  prs_xhtml(Premise, ViewMode, PremiseElement),
  prs_xhtml(Conclusion, ViewMode, ConclusionElement),
  xhtml_conds_x(Conds, ViewMode, Elements).

% not implications
xhtml_conds_x([neg(Premise) => Conclusion|Conds], ViewMode, [element(div, [class=connected], [element(div, [class=connector], ['¬']),
                                                                                PremiseElement,                                                                 
                                                                                element(div, [class=connector], ['?']),
                                                                                ConclusionElement])
                                              | Elements]) :-
  is_prs(Premise),
  is_prs(Conclusion),
  !,
  prs_xhtml(Premise, ViewMode, PremiseElement),
  prs_xhtml(Conclusion, ViewMode, ConclusionElement),
  xhtml_conds_x(Conds, ViewMode, Elements).

% not implications not
xhtml_conds_x([neg(Premise) => neg(Conclusion)|Conds], ViewMode, [element(div, [class=connected], [element(div, [class=connector], ['¬']),
                                                                                PremiseElement,                                                                 
                                                                                element(div, [class=connector], ['?']),
                                                                                element(div, [class=connector], ['¬']),
                                                                                ConclusionElement])
                                              | Elements]) :-
  is_prs(Premise),
  is_prs(Conclusion),
  !,
  prs_xhtml(Premise, ViewMode, PremiseElement),
  prs_xhtml(Conclusion, ViewMode, ConclusionElement),
  xhtml_conds_x(Conds, ViewMode, Elements).

% reversed implications
% (Variable names are not very logical for this case; this can still be adjusted.)
xhtml_conds_x([Premise <= Conclusion|Conds], ViewMode, [element(div, [class=connected], [PremiseElement,
                                                                               element(div, [class=connector], ['←']),
                                                                               ConclusionElement])
                                              | Elements]) :-
  is_prs(Premise),
  is_prs(Conclusion),
  !,
  prs_xhtml(Premise, ViewMode, PremiseElement),
  prs_xhtml(Conclusion, ViewMode, ConclusionElement),
  xhtml_conds_x(Conds, ViewMode, Elements).

% biconditionals
% (Variable names are not very logical for this case; this can still be adjusted.)
xhtml_conds_x([Premise <=> Conclusion|Conds], ViewMode, [element(div, [class=connected], [PremiseElement,
                                                                               element(div, [class=connector], ['<=>']),
                                                                               ConclusionElement])
                                              | Elements]) :-
  is_prs(Premise),
  is_prs(Conclusion),
  !,
  prs_xhtml(Premise, ViewMode, PremiseElement),
  prs_xhtml(Conclusion, ViewMode, ConclusionElement),
  xhtml_conds_x(Conds, ViewMode, Elements).

% relation definitions
xhtml_conds_x([Definiendum := Definiens|Conds], ViewMode, [element(div, [class=connected], [DefiniendumElement,
                                                                                  element(div, [class=connector], [':=']),
                                                                                  DefiniensElement])
                                                 | Elements]) :-
  is_prs(Definiendum),
  is_prs(Definiens),
  !,
  prs_xhtml(Definiendum, ViewMode, DefiniendumElement),
  prs_xhtml(Definiens, ViewMode, DefiniensElement),
  xhtml_conds_x(Conds, ViewMode, Elements).

% function definitions
xhtml_conds_x([FunctionSymbol :: Domain => Definiens|Conds], ViewMode, [element(div, [class=connected], [
										  element(div, [class=function_symbol], [FunctionSymbol]),
                                                                                  element(div, [class=connector], [':']),
										  DomainElement,
										  element(div,[class=connector], ['→']),
                                                                                  DefiniensElement])
                                                 | Elements]) :-
  is_prs(Domain),
  is_prs(Definiens),
  !,
  prs_xhtml(Domain, ViewMode, DomainElement),
  prs_xhtml(Definiens, ViewMode, DefiniensElement),
  xhtml_conds_x(Conds, ViewMode, Elements).

% definitions not
xhtml_conds_x([Definiendum := neg(Definiens)|Conds], ViewMode, [element(div, [class=connected], [DefiniendumElement,
                                                                                  element(div, [class=connector], [':=']),
                                                                                  element(div, [class=connector], ['¬']),
                                                                                  DefiniensElement])
                                                 | Elements]) :-
  is_prs(Definiendum),
  is_prs(Definiens),
  !,
  prs_xhtml(Definiendum, ViewMode, DefiniendumElement),
  prs_xhtml(Definiens, ViewMode, DefiniensElement),
  xhtml_conds_x(Conds, ViewMode, Elements).


% assumptions
xhtml_conds_x([Prefix ==> Matrix|Conds], ViewMode, [element(div, [class=connected], [PrefixElement,
                                                                           element(div, [class=connector], ['==>']),
                                                                           MatrixElement])
                                          | Elements]) :-
  is_prs(Prefix),
  is_prs(Matrix),
  !,
  prs_xhtml(Prefix, ViewMode, PrefixElement),
  prs_xhtml(Matrix, ViewMode, MatrixElement),
  xhtml_conds_x(Conds, ViewMode, Elements).

% assumptions not
xhtml_conds_x([Prefix ==> neg(Matrix)|Conds], ViewMode, [element(div, [class=connected], [PrefixElement,
                                                                                element(div, [class=connector], ['==>']),
                                                                                element(div, [class=connector], ['¬']),
                                                                                MatrixElement])
                                          | Elements]) :-
  is_prs(Prefix),
  is_prs(Matrix),
  !,
  prs_xhtml(Prefix, ViewMode, PrefixElement),
  prs_xhtml(Matrix, ViewMode, MatrixElement),
  xhtml_conds_x(Conds, ViewMode, Elements).

% the
xhtml_conds_x([the(Dref,PRS)|Conds], ViewMode, [element(div, [class=connected], 
		[
		element(div, [class=function_symbol], [the]),
		element(div, [class=connector], ['(']),
		element(div, [class=connector], [DrefElement]),
		element(div, [class=connector], [',']),
		PRSElement,
		element(div, [class=connector], [')'])
		])
                | Elements]) :-
  is_prs(PRS),
  !,
  term_to_atom(Dref,DrefElement),
  prs_xhtml(PRS, ViewMode, PRSElement),
  xhtml_conds_x(Conds, ViewMode, Elements).

% static
xhtml_conds_x([static(PRS)|Conds], ViewMode, [element(div, [class=connected], 
		[
		element(div, [class=function_symbol], [static]),
		element(div, [class=connector], ['(']),
		PRSElement,
		element(div, [class=connector], [')'])
		])
                | Elements]) :-
  is_prs(PRS),
  !,
  prs_xhtml(PRS, ViewMode, PRSElement),
  xhtml_conds_x(Conds, ViewMode, Elements).

% plural (for debugging purposes, this temporary PRS condition can also be displayed)
xhtml_conds_x([plural(Dref,PRS)|Conds], ViewMode, [element(div, [class=connected], 
		[
		element(div, [class=function_symbol], [plural]),
		element(div, [class=connector], ['(']),
		element(div, [class=connector], [DrefElement]),
		element(div, [class=connector], [',']),
		PRSElement,
		element(div, [class=connector], [')'])
		])
                | Elements]) :-
  is_prs(PRS),
  !,
  term_to_atom(Dref,DrefElement),
  prs_xhtml(PRS, ViewMode, PRSElement),
  xhtml_conds_x(Conds, ViewMode, Elements).

% theorem
xhtml_conds_x([theorem(TheoremType,GoalPRS,ProofPRS)|Conds], ViewMode, [element(div, [class=connected], 
        [
        element(div, [class=function_symbol], [theorem]),
        element(div, [class=connector], ['(']),
        element(div, [class=connector], [TheoremTypeElement]),
        element(div, [class=connector], [',']),
        GoalPRSElement,
        element(div, [class=connector], [',']),
        ProofPRSElement,
        element(div, [class=connector], [')'])
        ])
                | Elements]) :-
  is_prs(GoalPRS),
  is_prs(ProofPRS),
  !,
  term_to_atom(TheoremType,TheoremTypeElement),
  prs_xhtml(GoalPRS, ViewMode, GoalPRSElement),
  prs_xhtml(ProofPRS, ViewMode, ProofPRSElement),
  xhtml_conds_x(Conds, ViewMode, Elements).

% or
xhtml_conds_x([Left v Right|Conds], ViewMode, 
	[element(div, [class=connected], 
		[LeftElement,
		element(div, [class=connector], ['v']),
		RightElement
		])
%	element(br, [], []) 
	| Elements]) :-
  is_prs(Left),
  is_prs(Right),
  !,
  prs_xhtml(Left, ViewMode, LeftElement),
  prs_xhtml(Right, ViewMode, RightElement),
  xhtml_conds_x(Conds, ViewMode, Elements).

% exclusive or, 2 arguments
xhtml_conds_x([><([PRS1,PRS2])|Conds], ViewMode, 
	[element(div, [class=connected], 
		[
		element(div, [class=function_symbol], [><]),
		element(div, [class=connector], ['[']),
		FirstElement,
		element(div, [class=connector], [',']),
		SecondElement,
		element(div, [class=connector], [']'])
		])
	| Elements]) :-
  is_prs(PRS1),
  is_prs(PRS2),
  !,
  prs_xhtml(PRS1, ViewMode, FirstElement),
  prs_xhtml(PRS2, ViewMode, SecondElement),
  xhtml_conds_x(Conds, ViewMode, Elements).

% exclusive or, 3 arguments
xhtml_conds_x([><([PRS1,PRS2,PRS3])|Conds], ViewMode, 
	[element(div, [class=connected], 
		[
		element(div, [class=function_symbol], [><]),
		element(div, [class=connector], ['[']),
		FirstElement,
		element(div, [class=connector], [',']),
		SecondElement,
		element(div, [class=connector], [',']),
		ThirdElement,
		element(div, [class=connector], [']'])
		])
	| Elements]) :-
  is_prs(PRS1),
  is_prs(PRS2),
  is_prs(PRS3),
  !,
  prs_xhtml(PRS1, ViewMode, FirstElement),
  prs_xhtml(PRS2, ViewMode, SecondElement),
  prs_xhtml(PRS3, ViewMode, ThirdElement),
  xhtml_conds_x(Conds, ViewMode, Elements).

% exclusive or, 4 arguments
xhtml_conds_x([><([PRS1,PRS2,PRS3,PRS4])|Conds], ViewMode, 
	[element(div, [class=connected], 
		[
		element(div, [class=function_symbol], [><]),
		element(div, [class=connector], ['[']),
		FirstElement,
		element(div, [class=connector], [',']),
		SecondElement,
		element(div, [class=connector], [',']),
		ThirdElement,
		element(div, [class=connector], [',']),
		FourthElement,
		element(div, [class=connector], [']'])
		])
	| Elements]) :-
  is_prs(PRS1),
  is_prs(PRS2),
  is_prs(PRS3),
  is_prs(PRS4),
  !,
  prs_xhtml(PRS1, ViewMode, FirstElement),
  prs_xhtml(PRS2, ViewMode, SecondElement),
  prs_xhtml(PRS3, ViewMode, ThirdElement),
  prs_xhtml(PRS4, ViewMode, FourthElement),
  xhtml_conds_x(Conds, ViewMode, Elements).

% at most one, 2 arguments
xhtml_conds_x([<>([PRS1,PRS2])|Conds], ViewMode, 
	[element(div, [class=connected], 
		[
		element(div, [class=function_symbol], [<>]),
		element(div, [class=connector], ['[']),
		FirstElement,
		element(div, [class=connector], [',']),
		SecondElement,
		element(div, [class=connector], [']'])
		])
	| Elements]) :-
  is_prs(PRS1),
  is_prs(PRS2),
  !,
  prs_xhtml(PRS1, ViewMode, FirstElement),
  prs_xhtml(PRS2, ViewMode, SecondElement),
  xhtml_conds_x(Conds, ViewMode, Elements).

% at most one, 3 arguments
xhtml_conds_x([<>([PRS1,PRS2,PRS3])|Conds], ViewMode, 
	[element(div, [class=connected], 
		[
		element(div, [class=function_symbol], [<>]),
		element(div, [class=connector], ['[']),
		FirstElement,
		element(div, [class=connector], [',']),
		SecondElement,
		element(div, [class=connector], [',']),
		ThirdElement,
		element(div, [class=connector], [']'])
		])
	| Elements]) :-
  is_prs(PRS1),
  is_prs(PRS2),
  is_prs(PRS3),
  !,
  prs_xhtml(PRS1, ViewMode, FirstElement),
  prs_xhtml(PRS2, ViewMode, SecondElement),
  prs_xhtml(PRS3, ViewMode, ThirdElement),
  xhtml_conds_x(Conds, ViewMode, Elements).

% at most one, 4 arguments
xhtml_conds_x([<>([PRS1,PRS2,PRS3,PRS4])|Conds], ViewMode, 
	[element(div, [class=connected], 
		[
		element(div, [class=function_symbol], [<>]),
		element(div, [class=connector], ['[']),
		FirstElement,
		element(div, [class=connector], [',']),
		SecondElement,
		element(div, [class=connector], [',']),
		ThirdElement,
		element(div, [class=connector], [',']),
		FourthElement,
		element(div, [class=connector], [']'])
		])
	| Elements]) :-
  is_prs(PRS1),
  is_prs(PRS2),
  is_prs(PRS3),
  is_prs(PRS4),
  !,
  prs_xhtml(PRS1, ViewMode, FirstElement),
  prs_xhtml(PRS2, ViewMode, SecondElement),
  prs_xhtml(PRS3, ViewMode, ThirdElement),
  prs_xhtml(PRS4, ViewMode, FourthElement),
  xhtml_conds_x(Conds, ViewMode, Elements).

% negation
xhtml_conds_x([neg(PRS)|Conds], ViewMode, [element(div, [class=connected], [
                                                                           element(div, [class=connector], ['¬']),
                                                                           NegatedElement])
                                          | Elements]) :-
  !,
  prs_xhtml(PRS, ViewMode, NegatedElement),
  xhtml_conds_x(Conds, ViewMode, Elements).

% math_id
% old version: math(Y) !
xhtml_conds_x([math_id(X,Y)|Conds], ViewMode, [element(div, [class=condition],[CondAsAtom])|Elements]) :-
		term_to_atom(X,ID),
        dobsod_to_atom(Y,Atom),
        concat_atom(['math_id(',ID,',',Atom,')'],CondAsAtom),
        xhtml_conds_x(Conds, ViewMode, Elements).

% statements
xhtml_conds_x([Cond|Conds], ViewMode, [Element|Elements]) :-
  prs_xhtml(Cond, ViewMode, Element),
  !,
  xhtml_conds_x(Conds, ViewMode, Elements).

xhtml_conds_x([Cond|Conds], ViewMode, [element(div,[class=condition],[CondAsAtom])|Elements]) :-
  term_to_atom(Cond, CondAsAtom),
  xhtml_conds_x(Conds, ViewMode, Elements).

xhtml_conds_x([], _, []).

%%      dobsod_to_atom(Input:DOBSOD,Output:atom)
%
%       Translates a DOBSOD into an atom

dobsod_to_atom(Grammar_exp,'$false'):-
        Grammar_exp = type~relation ..name~'$false',
        !.

dobsod_to_atom(Grammar_exp,Atom):-
        Grammar_exp = type~logical_symbol ..name~'~' ..args~[A],
        !,
%       Process the argument A
        dobsod_to_atom(A,NewA),
        concat_atom(['~','(',NewA,')'],Atom).

dobsod_to_atom(Grammar_exp,Atom):-
        Grammar_exp = type~logical_symbol ..args~[A,B] ..name~X,
        !,
%       Process the arguments A and B and create the atom 'AXB', where X is a logical symbol of the type &(and)/|(or)/=>(implies)/<=>(equivalence).
        dobsod_to_atom(A,NewA),
        dobsod_to_atom(B,NewB),
        concat_atom(['(',NewA,')',X,'(',NewB,')'],Atom).

dobsod_to_atom(Grammar_exp,Atom):-
        Grammar_exp = type~relation ..args~Arglist ..name~X,
        !,
%       Process the list of arguments Arglist using the predicate prepare_list and use the output to create a relation of the form X(a1,....,aN), where
%       N is the length of Arglist.
        prepare_list(Arglist,Newlist),
        concat_atom([NewArglist,','],Newlist),
        concat_atom([X,'(',NewArglist,')'],Atom).

dobsod_to_atom(Grammar_exp,Atom):-
        Grammar_exp = type~quantifier ..args~[A,B] ..name~X,
        !,
        prepare_list(A,NewA),
        concat_atom([Newlist,','],NewA),
        dobsod_to_atom(B,NewB),
        concat_atom([X,'[',Newlist,']',':','(',NewB,')'],Atom).

dobsod_to_atom(Grammar_exp,Atom):-
        Grammar_exp = type~function..args~Arglist..name~X,
        prepare_list(Arglist,Newlist),
        concat_atom([NewArglist,','],Newlist),
        concat_atom([X,'(',NewArglist,')'],Atom),
        !.
                
dobsod_to_atom(Grammar_exp,Atom):-
        Grammar_exp = type~variable ..name~Atom,
        !.

dobsod_to_atom(Grammar_exp,Atom):-
        Grammar_exp = type~constant ..name~Atom,
        !.

prepare_list([H|T],Atomlist):-
        !,
        dobsod_to_atom(H,NewH),
        prepare_list(T,NewT),
        concat_atom([NewH,',',NewT],Atomlist).
prepare_list([],''):- !.

%% write_prs_xhtml_to_file(+PRS, +ViewMode, -File)
%
% Writes PRS to a complete HTML file, so that it can be viewed in a web
% browser.
%
% The HTML file includes a link to a CSS stylesheet needed to visualise the PRS
% sturtures correctly.
%
% The HTML files produced are XHTML 1.0 Strict and are ASCII or UTF-8 encoded
% (if the presence of special characters necessitates this).
%
% ViewMode = user|debug

write_prs_xhtml_to_file(PRS, ViewMode, OutFile) :-
  	% working_directory(PWD, PWD),
  	% format(atom(StylesheetHREF), 'file://~w~w', [PWD, 'misc/prs_display.css']),
  	prs_xhtml(PRS, ViewMode, XHTML),

	% Define Output File
	check_src(SRC),
	session_id(Id),
	concat_atom([SRC,'/tmp/',Id,'/prs.php'],File),
	concat_atom(['../tmp/',Id,'/prs.php'],OutFile),
	StylesheetHREF = '../../css/prs_display.css',

  	open(File, write, Stream),
  	Source = element(html, [xmlns='http://www.w3.org/1999/xhtml'], [
    		element(head, [], [
      		element(title, [], ['PRS Display']),
      		element(link, [href=StylesheetHREF, rel=stylesheet, type='text/css'], []),
      		element(meta, ['http-equiv'='content-type', 'content'='application/xhtml+xml; charset=utf-8'], [])
    	]),
    	element(body, [], [
      		XHTML
    	])]),
  	format(Stream, '~w~n',   '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"'),
  	format(Stream, '~w~n~n', '      "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">'),
	format(Stream,'<?php include($_SERVER["DOCUMENT_ROOT"]."/naproche/inc/init.php"); ?>',[]),
	format(Stream,'<?php include($_SERVER["DOCUMENT_ROOT"]."/naproche/inc/prsErrorHandling.php"); ?>',[]),
  	xml_write(Stream, Source, [header(false)]),
  	close(Stream).

%% 	write_prs_xhtml(+PRS)
%
% Writes PRS to a complete HTML file, which is written on the standard OutStream.
%
% The HTML file includes a link to a CSS stylesheet needed to visualise the PRS
% sturtures correctly.
%
% The HTML files produced are XHTML 1.0 Strict and are ASCII or UTF-8 encoded
% (if the presence of special characters necessitates this).
write_prs_xhtml(PRS) :-
  	prs_xhtml(PRS, user, XHTML),

	% Define Output File
	check_src(SRC),
	session_id(Id),
	concat_atom([SRC,'/tmp/',Id,'/prsdiv.html'],File),

  	open(File, write, Stream),
  	xml_write(Stream, XHTML, [header(false)]),
  	close(Stream).




%% display_prs(+PRS)
%
% Display a PRS in a web browser.
%
% Uses prs_xhtml_to_file/2 to write PRS to a temporary file and display this
% file in a web browser. The success of this predicate depends on two factors:
%
%  * the Prolog process must be able to create a temporary file (see tmp_file/2)
%  * the operating system must route the request to the web browser correctly
%  (see www_open_url/1)
%
% Both of those work under OS X and Windows XP. According to mailing list
% discussions opening a url on Linux might be problematic; if this predicate
% does not open a browser window, consider replacing the www_open_url/1 call
% with a shell command using shell/1.

% The temporary file does not survive the Prolog porcess that created it.
display_prs(PRS) :-
%  	tmp_file(prs, TmpFile),
  	write_prs_xhtml_to_file(PRS, debug, _FileName).

%  www_open_url(FileName),
%  at_halt(catch(delete_file(File), _, true)).
