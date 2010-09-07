:- module(graph,[
	obligation_graph/3,
	accessible_graph/2,
	update_graph/7,
	add_rrefs/4,
	display_graph/3,
	add_formula_link/4,
	add_derivation_link/4]).

:- use_module(library(ugraphs)).
:- use_module(naproche(discharge_obligations)).

:- ensure_loaded(naproche(gulp4swi)).




/**	<module> Building and maintaining proof graphs
 *
 *	This module contains predicates to create and maintain proof graphs which are needed in the premise selection process.
 */

%%	update_graph(+PRS_Id:atom,+Id:term,+ProofFile:Atom,+InGraph,-OutGraph,-NewResult,-Edges)
%
%	Run ProofSummary on ProofFile and adds new edges for every axiom that was used in the proof

update_graph(PRS_Id,CId,File,In,Out,NewResult,TEdges) :-
	%run proof summary
	check_src(SRC),
	session_id(SId),
	
	strip_id(PRS_Id,StripId),
	term_to_atom(PRS_Id,PRSA),
	term_to_atom(CId,CIdA),
	term_to_atom(File,FileA),

	concat_atom(['\"',FileA,'\"'],QuotedFile),
	concat_atom([SRC,'/tmp/',SId,'/',StripId,'-',PRSA,'-',CIdA,'.ProofSummary'],TmpFile),
	concat_atom(['\"',TmpFile,'\"'],QuotedTmpFile),
	concat_atom([SRC,'/inc/ProofSummary -f prolog ',QuotedFile,' >  ',QuotedTmpFile],Call),
	shell(Call),
	open(TmpFile,read,IS),
	read(IS,Term),
	close(IS),
	
	( Term = proved(LastCon,Edges) ->
		(
		% LastCon tells is if the conjecture was used in the proof.
		% If it wasnt used, the axioms are contradictory which is a problem, therefore we set NewResult = Warning in this case.
		( LastCon = CIdA ->
			NewResult = theorem;
			NewResult = warning
		),
		% update graph
		maplist(term_to_atom,TEdges,Edges),
		add_edges_int(CId,TEdges,In,Out)
		)
		;
		% Else, Term = end_of_file, therefore
		In = Out
	),
	!.

update_graph(PId,Id,_File,_,_,_) :-
	add_error_message(graph,update_graph,PId-Id,'update_graph failed'),
	fail.

add_edges_int(_V,[],In,In) :- !.

add_edges_int(V,[E|Rest],In,Out) :-
	add_edges(In,[V-E],TmpOut),
	add_edges_int(V,Rest,TmpOut,Out).
	


% ------------------------------------- obligation graph -----------------------------------------------------

%%	obligation_graph(+Ob:list,+GraphIn,-GraphOut)
%
%	Takes a list of proof obligations as created by create_obligations and gives a graph that has a vertice for each ID in Ob and
%	edges if two IDs are neighbours in Ob. (Old)
%
%	(New): For each obligations, finds all the Premises that are new in this obligations, i.e. were not available in the last one and links
%	all the new Premises to each entry in Conjecture.


obligation_graph([],Graph,Graph) :- !.

obligation_graph([[_PRSId,Conj,Axioms]|Rest],GraphIn,GraphOut) :-
	!,
	add_premises_link(Conj,Axioms,GraphIn,TmpGraph),
	obligation_graph_intern(Axioms,Rest,TmpGraph,GraphOut).

obligation_graph_intern(_Axioms,[],Graph,Graph) :- !.

obligation_graph_intern(OldAxioms,[[_PRSId,Conj,Axioms]|Rest],GraphIn,GraphOut) :- 
	!,
	% We want to common axioms at the start of the list
	reverse(OldAxioms,OAR),
	reverse(Axioms,AR),
	% Find all the Axioms that are New in This Obligations
	get_new_axioms(OAR,AR,NewAxioms),
	
	% Create edges from the NewAxioms to the Conjectures
	add_premises_link(Conj,NewAxioms,GraphIn,TmpGraph),
	!,
	obligation_graph_intern(Axioms,Rest,TmpGraph,GraphOut).



get_new_axioms([X|RestOld],[X|RestNew],NewAxioms) :- 
	!,
	get_new_axioms(RestOld,RestNew,NewAxioms).

get_new_axioms(_OldAxioms,NewAxioms,NewAxioms).


add_premises_link([],_Axioms,Graph,Graph) :- !.

add_premises_link([id~Id|Rest],Axioms,GraphIn,GraphOut) :-
	add_premises_link_intern(Id,Axioms,GraphIn,TmpGraph),
	!,
	add_premises_link(Rest,Axioms,TmpGraph,GraphOut).

add_premises_link_intern(_Id,[],Graph,Graph) :-	!.

add_premises_link_intern(Id,[id~AxiomId|Rest],GraphIn,GraphOut) :-
	add_edges(GraphIn,[Id-AxiomId],TmpGraph),
	!,
	add_premises_link_intern(Id,Rest,TmpGraph,GraphOut).

/*
% Old Version

obligation_graph([],Graph,Graph) :- !.
% Case One Conjecture
obligation_graph([[_PRSId,Conj,_Axioms]|Rest],GraphIn,GraphOut) :-
	!,
	add_conjectures(Conj,Id,0,N,GraphIn,TmpGraph),
	obligation_graph_intern(Id,Rest,N,_,TmpGraph,GraphOut).
	
add_conjectures([id~Id],Id,N,N,GraphIn,GraphIn) :- !.

add_conjectures([id~Id|Rest],LastId,N,M,GraphIn,GraphOut) :-
	add_conjectures_intern(Id,Rest,LastId,N,M,GraphIn,GraphOut).

add_conjectures_intern(Id,[],Id,N,N,GraphIn,GraphIn) :- !.

add_conjectures_intern(Id,[id~NewId|Rest],LastId,N,O,GraphIn,GraphOut) :-
	add_edges(GraphIn,[NewId-Id],TmpGraph),
	M is N+1,
	add_conjectures_intern(NewId,Rest,LastId,M,O,TmpGraph,GraphOut).

obligation_graph_intern(_Id,[],N,N,GraphIn,GraphIn) :- !.

obligation_graph_intern(Id,[[_PRSId,Conj,_Axioms]|Rest],N,O,GraphIn,GraphOut) :-
	add_conjectures_intern(Id,Conj,LastId,N,M,GraphIn,TmpGraph),
	obligation_graph_intern(LastId,Rest,M,O,TmpGraph,GraphOut).
*/

%%      add_rrefs(+InGraph,+Rrefs:list,+Id,-OutGraph)
%
%       For all elements R of Rrefs, adds the edge R->Id to InGraph

add_rrefs(In,[],_Id,In).

add_rrefs(In,[R | Rest],Id,Out) :-
        R = 'induction',
        !,
        add_rrefs(In,Rest,Id,Out).

add_rrefs(In,[R | Rest],Id,Out) :-
        add_edges(In,[Id-R],Tmp),
        add_rrefs(Tmp,Rest,Id,Out).


%%	accessible_graph(+Ob:list,-Graph)
%
%	Takes a list of proof obligations as created by create_obligations and gives a graph that has a vertice for each Formula in Ob and
%	edges if one Formula is a premise of the other.

accessible_graph([],[]) :- !.

accessible_graph(Ob,Graph) :- 
	accessible_graph_intern(Ob,[],Graph).

accessible_graph_intern([],GraphIn,GraphIn) :-
	!.

accessible_graph_intern([[Id,Conj,Axioms]|Rest],GraphIn,GraphOut) :-
	add_ob(Id,Conj,Axioms,GraphIn,TmpGraph),
	accessible_graph_intern(Rest,TmpGraph,GraphOut).

add_ob(_Id,[],_Axioms,In,In) :- !.

add_ob(Id,[C|Conj],Axioms,In,Out) :-

	% Get Formula Id and create edge to PRSID
	C = id~CId,
	add_edges(In,[Id-CId],Tmp1),

	% Create edges between FormulaId and Axioms
	add_axioms(CId,Axioms,Tmp1,Tmp2),
	
	add_ob(Id,Conj,Axioms,Tmp2,Out).

add_axioms(_CId,[],In,In) :- !.

add_axioms(CId,[Axiom|Rest],In,Out) :-
	Axiom = id~AId,
	add_edges(In,[CId-AId],TmpGraph),
	add_axioms(CId,Rest,TmpGraph,Out).


%%	add_formula_link(+Id,+Premises:List,+GraphIn,-GraphOut)
%
%	For each formula in Premises, adds an edge between the formula and the Id

add_formula_link(_Id,[],GraphIn,GraphIn) :- !.

add_formula_link(Id,[Formula|Rest],GraphIn,GraphOut) :-
	Formula = id~FId,
	add_edges(GraphIn,[Id-FId,FId-Id],TmpGraph),
	add_formula_link(Id,Rest,TmpGraph,GraphOut).

%%	add_derivation_link(+Id,+Premises:List,+GraphIn,-GraphOut)
%
%	For each formula in Premises, adds an edge from the Id to the formula

add_derivation_link(_Id,[],GraphIn,GraphIn) :- !.

add_derivation_link(Id,[Formula|Rest],GraphIn,GraphOut) :-
	Formula = id~Id,
	!,
	add_derivation_link(Id,Rest,GraphIn,GraphOut).

add_derivation_link(Id,[Formula|Rest],GraphIn,GraphOut) :-
	Formula = id~FId,
	add_edges(GraphIn,[Id-FId],TmpGraph),
	add_derivation_link(Id,Rest,TmpGraph,GraphOut).




%%	display_graph(+Graph,+Type,-File)
%
%	Creates a svg of the Graph that is stored in File

display_graph(G,Type,FileOut) :-
	check_src(SRC),
	session_id(Id),
	concat_atom([SRC,'/tmp/',Id,'/Graph',Type,'.dot'],DotFile),
	concat_atom([SRC,'/tmp/',Id,'/Graph',Type,'.svg'],File),
	concat_atom(['../tmp/',Id,'/Graph',Type,'.svg'],FileOut),
	pl_to_dot(G,DotFile),
	concat_atom(['dot -Tsvg ',DotFile,' -o ',File],Call),
	shell(Call).



%%	pl_to_dot(+Graph,+Filename:atom)
%
%	Takes a prolog graph and translates it into the DOT format. Result is saved in Filename

pl_to_dot(Graph,File) :-
	open(File,write,OS),
	write(OS,'digraph G {'),
	nl(OS),
	translate_graph(Graph,OS),
	write(OS,'}'),
	close(OS).

translate_graph([],_OS).

translate_graph([_V-[]|Rest],OS) :-
	!,
	translate_graph(Rest,OS).

translate_graph([V-[E|ERest]|Rest],OS) :-
	term_to_atom(V,VA),
	term_to_atom(E,EA),
	concat_atom(['"',VA,'"  ->  "', EA,'";'],Entry),
	write(OS,Entry),
	nl(OS),
	translate_graph([V-ERest|Rest],OS).
	
