:- module(stats,[
	write_stats_php/2,
	write_local_stats/3,
	update_stats_final/4,
	update_stats_used/4,
	update_distance_stats/2
	]).

/**	<module> Creating and updating proof statistics
 *
 *	This module contains predicates to create and maintain proof statistics which are used in the output.
 */


%%	update_distance_stats(+DistanceList,-DStat)
%
%	All elements of the Nth entry in DistanceList are added to DStat as id~Id..distance~N

update_distance_stats(DistanceList,DStat) :-
	update_distance_stats(DistanceList,1,DStat).

update_distance_stats([[]|List],N,DStat) :-
	M is N+1,!,
	update_distance_stats(List,M,DStat).
update_distance_stats([Ns|List],N,DStat) :-
	add_list(Ns,N,DStatTmp,DStat),
	M is N+1,!,
	update_distance_stats(List,M,DStatTmp).
update_distance_stats([],_N,[]).

add_list([id~Id|Ids],N,In,[id~Id..distance~N|Out]) :-
	add_list(Ids,N,In,Out).
add_list([],_,In,In).

/*
%%	update_distance_stats(+InNodes,+N,+StatIn,-StatOut)
%
%	Adds all InNodes to StatIn.

update_distance_stats([],_N,StatIn,StatIn) :- !.

update_distance_stats([Id|Rest],N,StatIn,[id~Id..distance~N|StatOut]) :-
	update_distance_stats(Rest,N,StatIn,StatOut).
*/

%%	update_stats_used(+DStat,Used:list,-MaxDistance,-UStat)
%
%	For each x \in Dstat. if id(x) \in Used, set used(x)=yes, else used(x) = no

update_stats_used(DStat,Used,Max,UStat) :-
	update_stats_used_intern(DStat,Used,0,Max,UStat).

update_stats_used_intern([],_,Max,Max,[]) :- !.

update_stats_used_intern([X|Rest],Used,MaxSoFar,Max,[X|URest]) :-
	X = id~Id,
	member(Id,Used),
	!,
	X = used~yes..distance~D,
	(integer(D) -> D = ND ; atom_number(D,ND)),
	NewMaxSoFar is max(ND,MaxSoFar),
	update_stats_used_intern(Rest,Used,NewMaxSoFar,Max,URest).

update_stats_used_intern([X|Rest],Used,MaxSoFar,Max,[X|URest]) :-
	!,
	X = used~no,
	update_stats_used_intern(Rest,Used,MaxSoFar,Max,URest).



%%	update_stats_final(+UStat,+Id,+Result,-FStat)
%
%	FStat = id~Id..result~Result..premises~UStat

update_stats_final(UStat,Id,Result,id~Id..result~Result..premises~UStat).


%%	write_stats_php(+Stats:list,-OutFile)
%
%	writes the statistics into a php variable

write_stats_php(Stats,OutFile) :-
	
	session_id(SessionId),
	check_src(SRC),
	concat_atom([SRC,'/tmp/',SessionId,'/stats.php'],SFile),
	concat_atom(['../tmp/',SessionId,'/stats.php'],OutFile),
	!,

	open(SFile,write,OS),
	format(OS,'<?php ~n$stats = array(~n',[]),
	write_stats_php_intern(OS,Stats),
	format(OS,'); ~n
$pfad = "../../"; 
include($_SERVER["DOCUMENT_ROOT"].\'/naproche/inc/htmlhead.php\'); 
include($_SERVER["DOCUMENT_ROOT"].\'/naproche/inc/header.php\');'
	 ,[]),
	format(OS,'
include($_SERVER["DOCUMENT_ROOT"].\'/naproche/inc/stats.php\'); 
include($_SERVER["DOCUMENT_ROOT"].\'/naproche/inc/footer.html\'); 
showStats($stats);
?>',[]),

	close(OS).	
	
write_stats_php_intern(_OS,[]) :- !.

write_stats_php_intern(OS,[X]) :-
	!,
	X = id~Id..result~Result..premises~P,
	
	format(OS,'array("~w","~w",~t~n array(~n',[Id,Result]),
	write_stats_premises_php(OS,P),
	format(OS,'))~n',[]).


write_stats_php_intern(OS,[X|Rest]) :-
	!,
	X = id~Id..result~Result..premises~P,
	
	format(OS,'array("~w","~w",~n array(~n',[Id,Result]),
	write_stats_premises_php(OS,P),
	format(OS,')),~n',[]),

	write_stats_php_intern(OS,Rest).

write_stats_premises_php(_OS,[]) :- !.


write_stats_premises_php(OS,[X]) :-
	!,
	X = id~Id..used~U..distance~D,
	
	format(OS,'  array("~w","~w",~w)~n',[Id,U,D]).


write_stats_premises_php(OS,[X|Rest]) :-
	!,
	X = id~Id..used~U..distance~D,
	
	format(OS,'  array("~w","~w",~w),~n',[Id,U,D]),
	write_stats_premises_php(OS,Rest).

%%	write_local_stats(+Stats,+Id,-File)
%
%	creates a php file in /tmp/sessionId which contains the local stats

write_local_stats(Stats,PRSId,OutFile) :-

	session_id(SessionId),
	check_src(SRC),
	Stats = id~Id..result~Result..premises~P,

	term_to_atom(Id,IdA),
	term_to_atom(PRSId,PRSIdA),

	concat_atom([SRC,'/tmp/',SessionId,'/',PRSIdA,'-',IdA,'-stat.php'],SFile),
	concat_atom(['../tmp/',SessionId,'/',PRSIdA,'-',IdA,'-stat.php'],OutFile),
	!,
	open(SFile,write,OS),
	format(OS,'<?php ~n$stats = ~n',[]),
	format(OS,'array("~w","~w",~t~n array(~n',[Id,Result]),
	write_stats_premises_php(OS,P),
	format(OS,')); ~n
$pfad = "../../"; 
include($_SERVER["DOCUMENT_ROOT"].\'/naproche/inc/htmlhead.php\'); 
include($_SERVER["DOCUMENT_ROOT"].\'/naproche/inc/header.php\');'
	 ,[]),
	format(OS,'~n 
include($_SERVER["DOCUMENT_ROOT"].\'/naproche/inc/stats.php\'); 
showObligationStats($stats);
?>',[]),

	close(OS).	

