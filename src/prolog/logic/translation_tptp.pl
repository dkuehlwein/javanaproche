:- module(translation_tptp,[prepare_tptp/3]).

:- use_module(library(pldoc)).

%%	prepare_tptp(+Grammar_exp:list(DOBSOD),-TPTP_exp:atom,+Bound_in:list(atom))
%
%	This predicate translates a list of DOBSODs into an atom which is further processed in the module fof_check to create axioms or conjectures in the 
%	TPTP Format.The predicate checks the type of the DOBSOD Grammar_exp, and processes its arguments to create the atom TPTP_exp.
%	It also takes in a list of bound variables in the list Bound_in, and gives out a list of bound variables after processing the predicate. The list
%	of outbound bound variables and inbound variables differ only in the case of the occurence of a quantifier.

prepare_tptp(Grammar_exp,'$false',_Bound_in):-
	Grammar_exp = type~relation ..name~'$false',!.

prepare_tptp(Grammar_exp,'$true',_Bound_in):-
	Grammar_exp = type~relation ..name~'$true',!.

prepare_tptp(Grammar_exp,TPTP_exp,Bound_in):-
	Grammar_exp = type~relation ..args~[LHS,RHS] ..name~'=',
	!,
%	Process the list of arguments Arglist using the predicate prepare_list and use the output to create a relation of the form X(a1,....,aN), where
%	N is the length of Arglist.
	prepare_tptp(LHS,LHSP,Bound_in),
	prepare_tptp(RHS,RHSP,Bound_in),
	concat_atom([LHSP,' = ',RHSP],TPTP_exp).


prepare_tptp(Grammar_exp,TPTP_exp,Bound_in):-
	Grammar_exp = type~logical_symbol ..name~'~' ..args~[A],
	!,
%	Process the argument A
	prepare_tptp(A,NewA,Bound_in),
	concat_atom(['~','(',NewA,')'],TPTP_exp).

prepare_tptp(Grammar_exp,TPTP_exp,Bound_in):-
	Grammar_exp = type~relation ..name~'~=' ..args~[LHS,RHS],
	!,
	prepare_tptp(LHS,LHSP,Bound_in),
	prepare_tptp(RHS,RHSP,Bound_in),
	concat_atom(['~ (',LHSP,' = ',RHSP,')'],TPTP_exp).

prepare_tptp(Grammar_exp,TPTP_exp,Bound_in):-
	Grammar_exp = type~logical_symbol ..args~[A,B] ..name~X,
	!,
%	Process the arguments A and B and create the atom 'AXB', where X is a logical symbol of the type &(and)/|(or)/=>(implies)/<=>(equivalence).
	prepare_tptp(A,NewA,Bound_in),
	prepare_tptp(B,NewB,Bound_in),
	concat_atom(['(',NewA,')',X,'(',NewB,')'],TPTP_exp).

prepare_tptp(Grammar_exp,TPTP_exp,Bound_in):-
	Grammar_exp = type~relation ..args~Arglist ..name~X,
	!,
%	Process the list of arguments Arglist using the predicate prepare_list and use the output to create a relation of the form X(a1,....,aN), where
%	N is the length of Arglist.
	prepare_list(Arglist,Newlist,Bound_in),
	concat_atom([NewArglist,','],Newlist),
	concat_atom(['r',X,'(',NewArglist,')'],TPTP_exp).

prepare_tptp(Grammar_exp,TPTP_exp,Bound_in):-
	Grammar_exp = type~quantifier ..args~[A,B] ..name~X,
	!,
%	Process the arguments A and B, and add the processed list NewA to form the new list of bound variables New_Bound_in, which is passed to the argument
%	list NewB as the list of bound variables. As output, note that the list of outbound bound variables is same as Bound_in.
	append(Bound_in,A,New_Bound_in),
	prepare_list(A,NewA,New_Bound_in),
	concat_atom([Newlist,','],NewA),
	prepare_tptp(B,NewB,New_Bound_in),
	concat_atom([X,'[',Newlist,']',':','(',NewB,')'],TPTP_exp).
		
prepare_tptp(Grammar_exp,TPTP_exp,Bound_in):-
	Grammar_exp = type~variable ..name~Name..dref~Dref,
	!,

%	If the variable has an associated Dref, use this instead of the name
	( var(Dref) ->
		NName = Name;
		concat_atom(['d',Dref],NName)
	),

%	Here, we check if the variable of name Name is already present in the list of bound variables Bound_in. If it is, then we concat 'V' in front of Name
%	to create TPTP-exp. Else, we concat 'v' in front of Name. This is done as TPTP cannot process free variables. Also note, the lists Bound_in and
%	Bound_out are the same.
	(member(Grammar_exp,Bound_in) ->
		concat_atom(['V',NName],TPTP_exp)
		;
		concat_atom(['v',NName],TPTP_exp)
	).

prepare_tptp(Grammar_exp,TPTP_exp,_Bound_in):-
%	Here, we access the constant name Name and concat 'v' in front of it to create TPTP-exp.
	Grammar_exp = type~constant ..name~Name..dref~Dref,
	!,

	( var(Dref) ->
		concat_atom(['v',Name],TPTP_exp);
		concat_atom(['vd',Dref],TPTP_exp)
	).

prepare_tptp(Grammar_exp,TPTP_exp,_Bound_in):-
%	Here, we access the function name Name and concat 'v' in front of it to create TPTP-exp.
	Grammar_exp = type~function ..args~[] ..name~X,
	!,
	concat_atom(['v',X],TPTP_exp).

prepare_tptp(Grammar_exp,TPTP_exp,Bound_in):-
%	Here, we access the function name Name and concat 'v' in front of it to create TPTP-exp.
	Grammar_exp = type~function ..args~Arglist ..name~X,
	!,
	prepare_list(Arglist,Newlist,Bound_in),
	concat_atom([NewArglist,','],Newlist),
	concat_atom(['v',X,'(',NewArglist,')'],TPTP_exp).
	


%%	prepare_list(+List:list(DOBSOD),-Atomlist:list(atom),+Bound_in:list(atom),-Bound_out:list(atom))
%
%	This predicate takes in a list of DOBSODs and a list of bound variables as input, and processes them to give a list of atoms Atomlist, and the
%	list of outbound variables Bound_out.


prepare_list([H|T],Atomlist,Bound_in):-
	!,
	prepare_tptp(H,NewH,Bound_in),
	prepare_list(T,NewT,Bound_in),
	concat_atom([NewH,',',NewT],Atomlist).
prepare_list([],'',_).

