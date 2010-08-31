% This is the runner file. All high level calls to the Naproche Prolog module
% should be made using the predicates in this file.

:- encoding(utf8).

:- dynamic(message_hook/3).
:- multifile(message_hook/3).

:- ensure_loaded(load).
:- ensure_loaded(user).

% drive(+XMLFile)
%
% This is the main driver predicate. XMLFile is a path to an XML file in the
% preprocessed Naproche file format. Creates the corresponding PRS to the XMLFile and
% checks it.

drive(XMLFile) :-
        reset,
	
	% Get XML into PROLOG format
        naproche_sentences(XMLFile, Sentences),
	write('XML parsed \n'),
%	write(Sentences),

	% Create PRS
        build_prs(PRS,Sentences),
	PRS = prs~X,
	write('PRS created \n'),

	% display_prs
        % display_prs(X, TmpFile).

	write('Creating Obligations \n'),
	create_obligations(X,[],_,[],_,check),

	% Check PRS
	write('Starting check \n'),
        discharge_obligations,
	write('Check finished \n'),

	% Check that all tests succeeded.
        ( fof_check:check_theorem ->
		write('Text valid \n')
		;
		write('Check did not succeed')
	).

display(XMLFile) :-
        reset,
	
	% Get XML into PROLOG format
        naproche_sentences(XMLFile, Sentences),
	write('XML parsed \n'),

	% Create PRS
        build_prs(PRS, Sentences),
	PRS = prs~X,
	write('PRS created \n'),

	reverse_prs(X,RevPRS),
	% display_prs
        display_prs(RevPRS).

	
read_stream(IS,AtomIn,AtomOut) :-
        get_char(IS,Char),
%       write(Char),
        ( (Char = '#'; Char = 'end_of_file') ->
                AtomOut=AtomIn;
                (
                atom_concat(AtomIn,Char,AtomTmp),
                read_stream(IS,AtomTmp,AtomOut)
                )
        ).


:- dynamic(xxx/1).
:- assertz(xxx([])).

blubb(N) :- 
	xxx(X),
	retract(xxx(X)),
	assertz(xxx([X|X])),
	write(N),
	nl,
	M is N+1,
	blubb(M).

% reset
%
% During the parsing and construction of PRS structures a number of assert calls
% are being made to aid memoization and reduce computing time. Those should be
% reverted to the initial state before each call to discourse_to_prs/2. Also,
% the unique integer which is used in a number of places is reset to 0, so we
% don't get very large integers between runs.
reset :-
        retractall(prs:accessibles_before_sentence(_,_)),
        retractall(current_index(_)),
        retractall(prs:opens(_,_)),
        assertz(current_index(1)),
	fof_check:clean,
	setval(refids,[]).

measure(G) :-
        cputime(X), gctime(Y),
        telling(Old), tell('/dev/null'),
        call(G),
        cputime(Now), gctime(NowG),
        GTime is NowG - Y,
        Time is Now-X,
        tell(Old),
        format('Time elapsed (in msec): ~d (~d spent in GC).', [Time, GTime]).

cputime(Time) :- statistics(runtime, [Time,_]).
gctime(Time) :- statistics(garbage_collection, [_,_,Time]).
