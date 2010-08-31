:- module(output,[
	create_output/3,
	create_output/9]).

/**	<module> All output producing predicates
 *
 * 	This module provides predicates that create the html or command line output
 *
 */

	
%%	create_output(+ATPInFile,+ATPOutFile,+PRSID,+Result, +Formula:list,+Time,+Distance,+UsedDistance, +StatFile)
%
%	Produces HTML output for the webinterface
%
%	@param ATPInFile	The location of the file in which the input for the ATP is saved
%	@param ATPOutFile	The location of the file in which the output of the ATP is saved
%	@param PRSID		The Id of the PRS that is being checked atm
%	@param Result		The Result of the check (can be Theorem or NoProof)
%	@param Formula		The formula which was checked
%	@param Time		The time the prover had to discharge the obligation
%	@param Distance		The maximal distance (<) a premises could have in the proof graph
%	@param UsedDistance	The maximal distance of the used premises
%	@param StatFile		The location of the file which contains the statistics


% output if debug(off)
create_output(HTMLInput,HTMLOutput,PRSID,Result,Formula,_Time,_Distance,_UsedDist,_StatFile) :-
	debug(off),
	!,

	% Write Formula Ids
	create_html_formula(Formula,TmpHTMLFormulae),
	concat_atom(TmpHTMLFormulae, HTMLFormulae),
	strip_id(PRSID,SId),

	format('
	<script language="JavaScript"> 
	var str = "";'
	,[]),
	

	% The actual output
	format('
	str += \'<div class="obligations">\';
	str += \'<table class="~w" border="0" width="100%" cellspacing="0" cellpadding="0"> \';    
	str += \'	<tr>\';
    str += \'		<td width="100%">\';',[Result]),
	format(' 
	str += \'	<table border="0" width="100%" cellspacing="0" cellpadding="0">\';
    str += \'		<tr>\';
    str += \'			<td> <span id="prsid"> ~w </span> </td>\';
	str += \'	<td width="150px"> <span id="highlight"> <span id="blue" onclick="markSentence(~w)">highlight sentence</span> </span> </td>\';
	str += \'	</tr>\';
	str += \'	</table>\';',[PRSID,SId]),
	format('str += \'</td> \';
	str += \'	<td> </td>\';
	str += \'</tr>\';'),

	% Conjecture
	format('
	str += \'<tr>\';
	str += \'	<td>\';
	str += \'	<table border="0" width="100%" cellspacing="0" cellpadding="0" style="margin: 0 0 0 20px"; >\';
	str += \'		<tr>\';'),
	
	format('
	str += \'		<td width="500px"> <span id="formulaid"> ConjectureId: ~w </span> </td>\';',[HTMLFormulae]),
	format('
	str += \'		<td> <span id="ATPIn"> <a target="input" href = "~w" > ATP Input </a> </span> </td>\';
	str += \'		<td> <span id="ATPOut"> <a target="output" href = "~w" > ATP Output </a> </span> </td>\';',[HTMLInput,HTMLOutput]),
	format('
	str += \'		</tr>\';
	str += \'	</table>\';
	str += \'	</td>\';
	str += \'</tr>\';'),


	format('
	str += \'<tr> </tr>\';
	str += \'</table>\';
	str += \'</div>\';'),

	format('
	satz~w.setDetails(str);
	satz~w.setColor("~w");
	</script>',[SId,SId,Result]).


% output if debug(on) 
create_output(HTMLInput,HTMLOutput,PRSID,Result,Formula,Time,Distance,UsedDist,StatFile) :-
	debug(on),
	!,
	
	% Write Formula Ids
	create_html_formula(Formula,TmpHTMLFormulae),
	concat_atom(TmpHTMLFormulae, HTMLFormulae),
	strip_id(PRSID,SId),
	
	format('
	<script language="JavaScript"> 
	var str = "";'
	,[]),
	
	% The actual output
	format('
	str += \'<div class="obligations">\';
	str += \'<table class="~w" border="0" width="100%" cellspacing="0" cellpadding="0"> \';    
	str += \'	<tr>\';
    str += \'		<td width="100%">\';',[Result]),
	format(' 
	str += \'	<table border="0" width="100%" cellspacing="0" cellpadding="0">\';
    str += \'		<tr>\';
    str += \'			<td> <span id="prsid"> ~w </span> </td>\';
	str += \'	<td width="150px"> <span id="highlight"> <span id="blue" onclick="markSentence(~w)">highlight sentence</span> </span> </td>\';
	str += \'	</tr>\';
	str += \'	</table>\';',[PRSID,SId]),
	format('str += \'</td> \';
	str += \'	<td> </td>\';
	str += \'</tr>\';'),

	% Conjecture
	format('
	str += \'<tr>\';
	str += \'	<td>\';
	str += \'	<table border="0" width="100%" cellspacing="0" cellpadding="0" style="margin: 0 0 0 20px"; >\';
	str += \'		<tr>\';'),
	
	format('
	str += \'		<td width="600px"> <span id="formulaid"> ConjectureId: ~w </span> </td>\';',[HTMLFormulae]),
	format('
	str += \'		<td> <span id="ATPIn"> <a target="input" href = "~w" > ATP Input </a> </span> </td>\';
	str += \'		<td> <span id="ATPOut"> <a target="output" href = "~w" > ATP Output </a> </span> </td>\';',[HTMLInput,HTMLOutput]),

	format('
	str += \'	</tr>\';
	str += \'	</table>\';
	str += \'	</td>\';
	str += \'</tr>\';'),

	% Stats
	format('
	str += \'<tr>\';
	str += \'	<td>\';
	str += \'	<table border="0" width="100%" cellspacing="0" cellpadding="0" style="margin: 0 0 0 20px"; >\';
	str += \'		<tr>\';'),

	format('
	str += \'		<span id="stats">\'; 
	str += \'		<td width="150px"> <span id="maxtime"> Max Time: ~w </span> </td>\'; 
	str += \'		<td width="150px"> <span id="maxdist"> Max Distance: ~w </span> </td>\'; 
	str += \'		<td width="150px"> <span id="usedist"> Used Distance: ~w </span> </td>\'; 
	str += \'		<td width="150px"> <span id="details"> Details: <a href = "~w" > Statistics </a> </span> </td>\';
	str += \'		</span>\';',[Time,Distance,UsedDist,StatFile]),

	format('
	str += \'	</tr>\';
	str += \'	</table>\';
	str += \'	</td>\';
	str += \'</tr>\';'),

	format('
	str += \'<tr> </tr>\';
	str += \'</table>\';
	str += \'</div>\';'),

	format('
	satz~w.setDetails(str);
	satz~w.setColor("~w");
	</script>',[SId,SId,Result]).

/*
	% The actual output
	format('
	<div class="obligations">
	<table class="~w" border="0" width="100%" cellspacing="0" cellpadding="0">     
	<tr>
        	<td width="100%">',[HTMLResult]),
	format(' 
		<table border="0" width="100%" cellspacing="0" cellpadding="0">
            	<tr>
                <td> <span id="prsid"> ~w </span> </td>
		<td width="150px"> <span id="highlight"> <span id="blue" onclick=\'markSentence(~w)\'>highlight sentence</span> </span> </td>
		</tr>
		</table>',[PRSID,SId]),
	format('</td> 
		<td> </td>
	</tr>'),

	% Conjecture
	format('
	<tr>
		<td>
		<table border="0" width="100%" cellspacing="0" cellpadding="0" style="margin: 0 0 0 20px"; >
			<tr>'),
	
	format('
			<td> <span id="formulaid"> ConjectureId: ~w </span> </td>',[HTMLFormulae]),
	format('
			</tr>
		</table>
		</td>
	</tr>'),


	% Stats
	format('
	<tr>
		<td>
		<table border="0" width="100%" cellspacing="0" cellpadding="0" style="margin: 0 0 0 20px"; >
			<tr>
			<td>'),
	format('
			<div id="stats">	
			<td width="150px"> <span id="maxtime"> Max Time: ~w </span> </td> 
			<td width="150px"> <span id="maxdist"> Max Distance: ~w </span> </td> 
			<td width="150px"> <span id="usedist"> Used Distance: ~w </span> </td> 
			<td width="150px"> <span id="details"> Details: <a href = "~w" > Statistics </a> </span> </td>
			',[Time,Distance,UsedDist,StatFile]),
	format('
			<td> <span id="ATPIn"> <a href = "~w" > ATP Input </a> </span> </td>
			<td> <span id="ATPOut"> <a href = "~w" > ATP Output </a> </span> </td>
			</div>',[HTMLInput,HTMLOutput]),
	format('
			</td>
			</tr>
		</table>
		</td>
	</tr>'),


	format('
	<tr> </tr>
	</table>
	</div>').
*/
% Old Vers	
/*	format('
	<tr>
	<th> ~w </th>
	<th> <div id=\'blue\' onclick=\'markSentence(~w)\'>highlight sentence</div></th>
	<th> ~w </th>
	<th> <div id="~w"> ~w </div> </th>',
	[PRSID,SId,HTMLFormulae,Color,HTMLResult]),
	format('
	<th> <a href = "~w" > ATP Input </a> </th>
	<th> <a href = "~w" > ATP Output </a> </th>
	<th> <a href = "~w" > Statistics </a> </th>
	</tr>', 
	[HTMLInput,HTMLOutput,StatFile]).
*/

%%	create_output(+PRS_ID,+Result, Formula:list)
%
%	create_output test whether the OutputFile reports or success or a failure.
%	And creates a formated report on OS. 
%
%	@param PRS_ID		The Id of the PRS which was checked
%	@param SessionId	The Id of the PHP Session
%	@param Formula		The formula which was checked
%	@param OutputFile	The file which contains the ATP output
%	@param OS		Outputstream, of OS = [], then the standard stream is used, and HTML output is created
%	@param HTML		trigger for html output
%	@param Result		theorem if its a proof, noproof if it isn't

create_output(PRS_ID, Result,Formula) :-
	!,	
	check_src(SRC),
	session_id(SId),
	concat_atom([SRC,'/tmp/',SId,'/output'],File),

	open(File,write,OS),

	% Write ID
	nl(OS),
	write(OS,PRS_ID),
	tab(OS,5),

	% Write Formula
	write(OS,Formula),
	tab(OS,5),

	% According to result, create output
	(Result = theorem->
	 write(OS,'Theorem');
	 write(OS,'No Proof')
	),
	close(OS).

%%	create_html_formula(+PreparedFormula:list(terms),-HTMLFormulae:list(atoms))
%

create_html_formula([],[]).
create_html_formula([Term | Rest],[Atom | RestHTML]) :-
	Term = fof(Id,_,_),
	term_to_atom(Id,QuotedAtom),
	atom_concat('\'\'\'',QA1,QuotedAtom),
	atom_concat(Atom,'\'\'\'',QA1),
	create_html_formula(Rest,RestHTML).


strip_id(Id,SId) :-
        Id =.. [_,X],
        !,
        strip_id(X,SId).
strip_id(Id,SId) :-
        Id =.. [_,X,_],
        !,
        strip_id(X,SId).
strip_id(Id,Id).

