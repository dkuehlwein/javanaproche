:- module(discharge_obligations,[
	discharge_obligations/3,
	discharge_obligations/4,
	check_theorem/0]).

:- use_module(library(ugraphs)).
:- use_module(library(pldoc)).
:- use_module(naproche(error_logger)).
:- use_module(naproche(utils)).
:- use_module(naproche(translation_tptp)).
:- use_module(naproche(graph)).
:- use_module(naproche(stats)).
:- use_module(naproche(output)).

/**	<module> High level proof checking predicate
 *
 * 	This module provides predicates that discharge proof obligations for ATPs.
 *
 */	


%%	discharge_obligations(+Obligations,+GraphIn,-GraphOut)
%
%	Discharges all the obligations that are stored in Obligations.
%	Entries in obligations are of the form [ID, Conjectures, Axioms].
%	Prover settings are loaded from the appropriate global Vars.
% 	input and output, as well as final_output files are created.

discharge_obligations(Ob,GraphIn,GraphOut) :-
	% reset warning counter
	discharge_obligations(Ob,GraphIn,GraphOut,_).

discharge_obligations(Ob,GraphIn,GraphOut,Stats) :-
	% Initialize Counters
	nb_setval(discharge_warnings,0),
	nb_setval(discharge_theorems,0),
	nb_setval(discharge_noproofs,0),

	reverse(Ob,ObListRight),
	% Display Graph,
	%accessible_graph(ObList,ObGraph),
	obligation_graph(ObListRight,[],ObGraph),

	ugraph_union(GraphIn,ObGraph, StartGraph),

	( ostream('HTML') -> 
		(
/*		% Write all the HTML display code
   		display_graph(ObGraph,ob,ObGFile),
		display_graph(StartGraph,start,StartGFile),

		format('
<a href = "~w" > View ObGraph </a> 
<a href = "~w" > View StartGraph </a>'
		,[ObGFile,StartGFile]),
*/		
		dis_ob(ObListRight,StartGraph,GraphOut,Stats,EndResult),
		write_endresult(EndResult),
		nb_getval(discharge_theorems,TCounter),
		write_theorems(TCounter),
		nb_getval(discharge_noproofs,NCounter),
		write_noproofs(NCounter),
		nb_getval(discharge_warnings,WCounter),
		write_inconsistencies(WCounter)
		);
		dis_ob(ObListRight,StartGraph,GraphOut,_Stats,_EndResult)
	).
%   	display_graph(GraphOut,end,EndGFile),
%	format('
%<a href = "~w" > View EndGraph </a>'
%		,[EndGFile]).
		
write_endresult(EndResult) :-
	EndResult = 'Logical check successful',
	!,
	DivId = 'green',
	format('<br/> <div id="endresult"><div id = "~w" > ~w </div> </div> <br/>',[DivId,EndResult]).
write_endresult(EndResult) :-
	DivId = 'red',
	format('<br/> <div id="endresult"><div id = "~w" > ~w </div> </div> <br/>',[DivId,EndResult]).

write_theorems(0) :-
	format('<div id="red"> 0 theorems proved </div> <br/>').
write_theorems(1) :-
	format('<div id="green"> 1 theorem proved </div> <br/>').
write_theorems(N) :-
	format('<div id="green"> ~w theorems proved </div> <br/>',[N]).

write_noproofs(0) :-
	format('<div id="green"> 0 proofs failed </div> <br/>').
write_noproofs(1) :-
	format('<div id="red"> 1 proof failed </div> <br/>').
write_noproofs(N) :-
	format('<div id="red"> ~w proofs failed </div> <br/>',[N]).

write_inconsistencies(0) :-
	format('<div id="green"> 0 inconsistencies found </div> <br/>').
write_inconsistencies(1) :-
	format('<div id="orange"> 1 inconsistency found </div> <br/>').
write_inconsistencies(N) :-
	format('<div id="orange"> ~w inconsistencies found </div> <br/>',[N]).


dis_ob([Ob|Rest],GraphIn,GraphOut,FStats,EndResult) :-
	Ob = [ Id, Conj, Axioms ],
	!,

	% Check one Conjecture at a time
	% This is necessary because most Provers dont support the TPTP standard.
	check_conj(Id,Conj,Axioms,GraphIn,TmpGraphIn,Stat,EndResult),

	!,
	dis_ob(Rest,TmpGraphIn,GraphOut,TmpStats,EndResult),
	append(Stat,TmpStats,FStats).
dis_ob([],Graph,Graph,[],_EndResult).


check_conj(Id,[Conj|Rest],Premises,GraphIn,GraphOut,[FStat|TmpStats],SentenceResult) :-
	% --------------------------  Settings ------------------------------------------------------------
	Conj = id~ConjectureId,

	term_to_atom(Id,IdA),
	term_to_atom(ConjectureId,CIdA),

	check_src(SRCLocation),
	check_time(Time),
	session_id(SessionId),
%	(
%	max_distance(MaxTmp);
%	add_error_message(dischargeError,0,check_conj,'Max Distance not set!')
%	),
	% If max distance is a number add +1, since the test in sort_premises is for equality.
	(integer(Time) -> Time = NTime ; atom_number(Time,NTime)),
%	(integer(MaxTmp) -> succ(MaxTmp,Max); atom_number(MaxTmp,Max)),

	concat_atom([SRCLocation,'/tmp/',SessionId,'/',IdA,'-',CIdA,'.input'],InputFile),
	concat_atom([SRCLocation,'/tmp/',SessionId,'/',IdA,'-',CIdA,'.output'],OutputFile),
	concat_atom(['../tmp/',SessionId,'/',IdA,'-',CIdA,'.input'],WebInputFile),
	concat_atom(['../tmp/',SessionId,'/',IdA,'-',CIdA,'.output'],WebOutputFile),

	% Prepare the conjecture
	prepare_formulae([Conj],[PreparedFormulae]),                
	write_predicates(InputFile,write,[PreparedFormulae]),
	
	% Calculate the Distances
	get_premises_distance(ConjectureId,Premises,GraphIn,DistanceList),
	update_distance_stats(DistanceList,DStat),
	% ------------------------------ Start Iterations -----------------------------------------------
%	StartDist is ceil(DistIn+2),
%	check_conj_intern(ConjectureId,[PreparedFormulae],Premises,GraphIn,500,Max,5,NTime,InputFile,OutputFile,Result,EndTime,EndDist,DStat),
	check_conj_intern(DistanceList,NTime,InputFile,OutputFile,Result,EndTime,EndDist),
	!,
	% -------------------------------- Process Result ------------------------------------

	% Graph Update
	% ProofSummary crashes if it isn't a theorem
	( Result = theorem -> 
		(
		update_graph(Id,ConjectureId,OutputFile,GraphIn,TmpGraphOut,FinalResultTmp,UsedPremisesIds),
		update_stats_used(DStat,UsedPremisesIds,MaxDist,UStat),
		!,
		% If FinalResultTmp=warning, it could be that we are doing a proof by contradiction, if that is the case, we don't want a warning
		( (FinalResultTmp = warning,member_subsumes(contradiction~yes,Premises)) ->
			FinalResult = theorem; FinalResult = FinalResultTmp
		))
		;
		(
		SentenceResult = 'Logical check failed',
		FinalResult = Result,
		update_stats_used(DStat,[],_MaxDist,UStat),
		MaxDist = EndDist,
		GraphIn=TmpGraphOut
		)
	),
	!,

	% Final Stat Update
	update_stats_final(UStat,ConjectureId,FinalResult,FStat),
	write_local_stats(FStat,Id,FStatFile),
	
	% If we have a warning, add +1 to the total warning counter
	increase_result_counter(FinalResult),!,

	% Calculate new start distance
%	NewDistIn is (DistIn*4+MaxDist)/5,

	% File and Web Output
	( ostream('HTML') ->
		create_output(WebInputFile,WebOutputFile,Id,FinalResult,[PreparedFormulae],EndTime,EndDist,MaxDist,FStatFile)
		;		
		create_output(Id,FinalResult,[PreparedFormulae])
	),

	NewPremises = [Conj | Premises],!,
	check_conj(Id,Rest,NewPremises,TmpGraphOut,GraphOut,TmpStats,SentenceResult).
check_conj(_,[],_,Graph,Graph,[],_SentenceResult).

%%	increase_result_counter(+Result)
%
%	Result must be either warning, theorem or noproof.
%	Increases the global variable discharge_Results by 1.

increase_result_counter('warning') :-
		nb_getval(discharge_warnings,N),
		M is N+1,
		nb_setval(discharge_warnings,M).
increase_result_counter('theorem') :-
		nb_getval(discharge_theorems,N),
		M is N+1,
		nb_setval(discharge_theorems,M).
increase_result_counter('noproof') :-
		nb_getval(discharge_noproofs,N),
		M is N+1,
		nb_setval(discharge_noproofs,M).

% Special Case: If there are no initial assumptions, e.i. DistanceList = []
check_conj_intern([],Time,InputFile,OutputFile,Result,Time,0) :- 
	!,
	run_checker(Time,InputFile,OutputFile,Result).
% Normal case: We have initial assumptions
check_conj_intern(DistanceList,MaxTime,InputFile,OutputFile,Result,EndTime,EndDist) :- 
	check_conj_intern(DistanceList,1,EndTime,MaxTime,1,EndDist,0,InputFile,OutputFile,Result).
	
check_conj_intern([[]|DistanceList],TimeIn,TimeOut,MaxTime,DistanceIn,DistanceOut,AxiomsAdded,InputFile,OutputFile,Result) :-
	!,
	DistanceInTmp is DistanceIn + 1,
	check_conj_intern(DistanceList,TimeIn,TimeOut,MaxTime,DistanceInTmp,DistanceOut,AxiomsAdded,InputFile,OutputFile,Result).
check_conj_intern([Premises|DistanceList],TimeIn,TimeOut,MaxTime,DistanceIn,DistanceOut,AxiomsAdded,InputFile,OutputFile,ResultFinal) :-
	!,
	prepare_premises(Premises,PreparedPremises),
	length(Premises,Length),
	AxiomsAddedTmp is AxiomsAdded + Length,
	write_predicates(InputFile,append,PreparedPremises),
	% Only run the checker if we passed the StartDistance
	( (AxiomsAddedTmp > 5; DistanceList=[])  ->
	(
	run_checker(TimeIn,InputFile,OutputFile,Result),
	% If it failed, try again
	( Result = theorem ->
		(
		ResultFinal = Result,
		TimeOut = TimeIn,
		DistanceOut=DistanceIn
		);
		(
		((NewTime is TimeIn+1, NewTime < MaxTime); NewTime=MaxTime),
		DistanceInTmp is DistanceIn + 1,
		check_conj_intern(DistanceList,NewTime,TimeOut,MaxTime,DistanceInTmp,DistanceOut,0,InputFile,OutputFile,ResultNew),
		ResultFinal=ResultNew
		)
	))
	;
	(
	DistanceInTmp is DistanceIn + 1,
	check_conj_intern(DistanceList,TimeIn,TimeOut,MaxTime,DistanceInTmp,DistanceOut,AxiomsAddedTmp,InputFile,OutputFile,ResultFinal)
	)).
% If it can't be checked, we get noproof
check_conj_intern([],Time,Time,_MaxTime,Distance,Distance,_AxiomsAdded,_InputFile,_OutputFile,noproof).


%%	check_theorem 
%
%	This predicate checks whether the final_output has any string of the form "No Proof". If it doesn't, this implies the entire string of arguments has
%	been proved.

check_theorem :-
	check_src(SRCLocation), 
	session_id(SessionId),
	concat_atom([SRCLocation,'/tmp/',SessionId,'/final_output'],FinalFile),
	catch(\+(read_from_file(FinalFile,"NoProof")),_,write('Nothing to check')).

%%	prepare_premises(+Premises:list(DOBSOD),-Prepared_premises:list(atom))
%
%	This predicate is used to convert a list of premises which are in the form of a list of DOBSODs into a list of axioms in the TPTP format. This 
%	is the set of premises which would be further passed on to the checker to check the validity of the conjecture. The input is the list of DOBSODs
%       and the counter I, which is used to uniquely name the axioms in the TPTP Format. These entities are further passed on to the predicate 
%	prepare_tptp which translates them into atoms. The output consists of the list of Prepared Premises in TPTP format.
%
%	Example:
%
%	==
%
%	prepare_premises([type~logical_symbol 
%	                  ..name~ '~' 
%	                  ..arity~1 
%	                  ..args~[type~variable ..name~x ..arity~0],type~variable ..name~y ..arity~0,type~variable ..name~z ..arity~0],
%	                  X
%	                ).                                                                             %Input list     
%	
%	X= [fof(1,axiom,'~(vx)'),fof(2,axiom,'vy'),fof(3,axiom,'vz')]                                  %Output
%	
%	==
 
prepare_premises([H1|T1],Prepared_premises) :- 
	H1 = id~Id,
	term_to_atom(Id,AId),
	concat_atom(['\'',AId,'\''],Name),
	prepare_tptp(H1,H2,[]),
    Head= fof(Name,axiom,H2),
    Prepared_premises=[Head|Tail],
	prepare_premises(T1,Tail).
prepare_premises([],[]).


%%	prepare_formulae(+Formulae:list(gulp),-PreparedFormulae:list(atom))
%
%	This predicate is used to convert a list of formulae which are in the form of a list of DOBSODs into a list of conjecutures in the TPTP format.
%	These conjectures are then passed on to the checker to check their validity. The input is the list of DOBSODs, and the counter J which is used
%	to uniquely name the conjectures inthe TPTP Format. These entities are further passed on to the predicate prepare_tptp which translates them into 
%	atoms. The output consists of the list of Prepared Formulae in TPTP format.
%	
%	Example:
%
%	==
%
%	prepare_formulae([type~logical_symbol 
%	                  ..name~ '~' 
%	                  ..arity~1 
%	                  ..args~[type~variable ..name~x ..arity~0],type~variable ..name~y ..arity~0,type~variable ..name~z ..arity~0],
%                         X
%	                ).
%	X= [fof(1,conjecture,'~(vx)'),fof(2,conjecture,'vy'),fof(3,conjecture,'vz')].
%	
%	==

prepare_formulae([H|T],PreparedFormulae) :-
	H = id~Id,
	term_to_atom(Id,AId),
	concat_atom(['\'',AId,'\''],Name),
	prepare_tptp(H,H2,[]),
	Head= fof(Name,conjecture,H2),
	PreparedFormulae=[Head|Tail],
	prepare_formulae(T,Tail).
prepare_formulae([],[]).


%%	write_predicates(+File:atom,Type,+Prepared_predicates:list(atom))
%
%	Once the axioms and conjectures have been prepared in the TPTP Format, we append them into a single list called Prepare_predicates and pass it onto 
%	this predicate which writes it into the file File.  

write_predicates(File,Type,Prepared_predicates) :-
	open(File,Type,OS),
    write_to_os(OS,Prepared_predicates).

%%	write_to_os(+OS:stream,+List:(atom))
%
%	writes List to OS, each entry followed by a '.' and a newline.

write_to_os(OS,[Head|Tail]) :-
	write(OS,Head),
	write(OS,.),
	nl(OS),
	write_to_os(OS,Tail).
write_to_os(OS,[]) :- 
	close(OS).

%%	read_from_file(+File:atom,+Word:list(int))
%
%	Once the TPTP Checker checks the validity (or the lack of it) of the conjecture, the result is written in the file "output".
%	This predicate is designed to read a "word" or a list of integers from a file "File". 
%	The predicate uses the predicates check_word/3 and sublist/2. The former returns the contents of the file in as a list of ASCII Codes and the latter
%	checks the presence of the given word in the file by checking if the list of integers is a sublist of the list of ASCII Codes. 
%	For our purposes, File is the file "output" and the word to be read is "Theorem" for checking the validity of the fof.


read_from_file(File,Word) :- 
	open(File,read,OS),
	get0(OS,Char),
	check_word(Char,Charlist,OS),
	sublist(Word,Charlist),
	close(OS).


%%	check_word(+Char:int,-Charlist:list(int),+OS)
%
%	This predicate reads in the list of characters from the file stream OS.
%	Here 
%	10 = "Return"
%	32 = "Space Bar"
%	-1 = "End of Stream"
%	In each of these cases, the next list is the empty list, indicating the end of file.

check_word(-1,[],_) :- !.
check_word(end_of_file,[],_) :- !.
check_word(Char,[Char|Chars],OS) :- 
	get(OS,NextChar),
	check_word(NextChar,Chars,OS). 

%%	get_parameters(-Time,-Checker,-SessionId,-ATPLocation,-SRCLocation,-Proxy,-OStream)
%
%	Gets the Parameters for SystemOnTPTP from predicates which are defined in run.pl

get_parameters(Time,Checker,SessionId,ATPLocation,SRCLocation,Proxy,OStream) :-
	check_time(Time),
	check_prover(Checker),
	session_id(SessionId),
%	check_size(Outputsize),
	check_place(ATPLocation),
	check_src(SRCLocation),
	proxy(Proxy),
	ostream(OStream),
	!.

get_parameters(_,_,_,_,_,_,_,_,_) :-
	add_error_message(parameters,0,fof_check,'Could not find parameters for SystemsOnTPTP.
	They should be defined in run.pl').


%%	run_checker(+Checker,+Time,+Proxy,+Location,+InputFile,+File,-Result)
%
%	Runs the Checker

% Extra clause for EP since SystemOnTPTP with -S creates serious overhead.
run_checker(Time,InputFile,OutputFile,Result)  :-
	check_place('local'),
	%check_prover('EP---'),
	!,
	concat_atom(['\"',InputFile,'\"'],QuotedInput),
	concat_atom(['\"',OutputFile,'\"'],QuotedOutput),
	check_src(SRCLocation),
	concat_atom([SRCLocation,'/cgi-bin/TPTP/Systems/EP---1.0/eproof --print-statistics -xAuto -tAuto --cpu-limit=',Time,' --proof-time-unlimited --tstp-in --tstp-out ' ,QuotedInput,' > ',QuotedOutput],Command),
%	concat_atom(['eproof --print-statistics -xAuto -tAuto --cpu-limit=',Time,' --proof-time-unlimited --tstp-in --tstp-out ',QuotedInput,' > ',QuotedOutput],Command),
%	write(Command),
	shell(Command,_),

	% Get Result
	(
	read_from_file(OutputFile,"Theorem") ->
		Result = theorem;
		Result = noproof
	),!.



run_checker(Time,InputFile,OutputFile,Result)  :-
	check_place('local'),
	!,
	concat_atom(['\"',InputFile,'\"'],QuotedInput),
	concat_atom(['\"',OutputFile,'\"'],QuotedOutput),
	check_src(SRCLocation),
	check_prover(Checker),
	concat_atom([SRCLocation,'/cgi-bin/TPTP/SystemExecution/SystemOnTPTP -q3 ',Checker,' ',Time,' -S ' ,QuotedInput,' > ',QuotedOutput],Command),
%	concat_atom([SRCLocation,'/cgi-bin/TPTP/Systems/EP---1.0/eproof --print-statistics -xAuto -tAuto --cpu-limit=',Time,' --proof-time-unlimited --tstp-in --tstp-out ' ,QuotedInput,' > ',QuotedOutput],Command),
%	concat_atom(['eproof --print-statistics -xAuto -tAuto --cpu-limit=',Time,' --proof-time-unlimited --tstp-in --tstp-out ',QuotedInput,' > ',QuotedOutput],Command),
%	write(Command),
	shell(Command,_),

	% Get Result
	(
	read_from_file(OutputFile,"Theorem") ->
		Result = theorem;
		Result = noproof
	),!.

run_checker(Time,InputFile,OutputFile,Result)  :-
	check_place('external'),
	!,
	check_src(SRCLocation),
	check_prover(Checker),
	proxy(Proxy),
	concat_atom(['\"',InputFile,'\"'],QuotedInput),
	concat_atom(['\"',OutputFile,'\"'],QuotedOutput),

	concat_atom(['perl -X ',SRCLocation,'/inc/RemoteSOT.pl'],RemoteSOTCall), 
	concat_atom([RemoteSOTCall,' -S -t ',Time,' -y ',Proxy,' -s ',Checker,' ',QuotedInput,' > ',QuotedOutput],Command),
%	write(Command),
	shell(Command),

	% Get Result
	(
	read_from_file(OutputFile,"Theorem") ->
		Result = theorem;
		Result = noproof
	),!.
		
run_checker(_,_,_,_)  :-
	!,
	add_error_message(parameters,0,fof_check,'Location specification incorrect. Use either "local" or "extern"'),
	fail.


%%	get_premises_distance(+Id,+Premises,+Graph,-DistanceList)
%
%	Calculates the distance of all the Premises from Id in Graph. Stores the Result in DistanceList.
% 	DistanceList is a List of Lists. Each List contains all the premises that have that distance from Id.

get_premises_distance(Id,Premises,Graph,DistanceList) :-
	get_distance(Premises,[Id],[Id],Graph,DistanceList).

%%	get_distance(+Premises:list,+NodesAll,+NodesNew,+Graph,-DistanceList)
%
%	Calculates the distance of the premises. DistanceList is in reverse order.

get_distance([],_NodesAll,_NodesNew,_Graph,[]) :- !.
get_distance(Premises,NodesAll,NodesNew,Graph,[PremisesFound|DistanceList]) :-
	get_next_nodes(NodesNew,Graph,NodesTmp),
	union(NodesAll,NodesTmp,NodesAllTmp),
	subtract(NodesTmp,NodesAll,NodesNewTmp),
	find_premises(Premises,NodesNewTmp,PremisesFound,PremisesLeft),
	!,
	get_distance(PremisesLeft,NodesAllTmp,NodesNewTmp,Graph,DistanceList).

%%	find_premises(+Premises:list,+Nodes:list,-PremisesFound:list,-PremisesLeft:list)
%
%	All Premises whose Ids are in Nodes are put into PremisesFound, the others in PremisesLeft

find_premises([P|Premises],Nodes,[P|PremisesFound],PremisesLeft) :-
	P = id~Id,
	member(Id,Nodes),!,
	find_premises(Premises,Nodes,PremisesFound,PremisesLeft).
find_premises([P|Premises],Nodes,PremisesFound,[P|PremisesLeft]) :-
	find_premises(Premises,Nodes,PremisesFound,PremisesLeft).
find_premises([],_Nodes,[],[]).

%%	get_nodes(+Premises,-Nodes)
%
%	Nodes is the list of the ids of the Premises

get_nodes([id~Id|Rest],[Id|RestNodes]) :-
	get_nodes(Rest,RestNodes).
get_nodes([],[]).


%%	get_next_nodes(+StartNodes:list,+Graph,-NewNodes)
%
%	NewNodes=StartNodes+ all node with distance 1 from NewNodes

get_next_nodes([],_G,[]) :- !.
get_next_nodes(StartNodes,G,NewNodes) :-
	get_next_nodes_intern(StartNodes,G,StartNodes,NewNodes).

get_next_nodes_intern([Node|Rest],G,NodesIn,NodesOut) :-
	neighbours(Node,G,Vertices),	
	union(NodesIn,Vertices,TmpNodes),	
	get_next_nodes_intern(Rest,G,TmpNodes,NodesOut).
get_next_nodes_intern([],_G,NodesIn,NodesIn).


%%	contains_id(+NMiss,+Search,-InSearch,-NotInSeach)

contains_id([N|Rest],Search,[N|In],NotIn) :-
	member(N,Search),
	!,
	contains_id(Rest,Search,In,NotIn).
contains_id([N|Rest],Search,In,[N|NotIn]) :-
	!,
	contains_id(Rest,Search,In,NotIn).
contains_id([],_New,[],[]).




