:- begin_tests(discharge_obligations).

% ---------------------------------- prepare_premises ----------------------------------

test(prepare_premises):-
	discharge_obligations:prepare_premises([[type~logical_symbol ..name~ '~' ..arity~1 ..args~[type~variable ..name~x ..arity~0]..id~1],
			[type~variable ..name~y ..arity~0..id~2],
			[type~variable ..name~z ..arity~0..id~3]],
            [fof(_,axiom,'~(vx)'),fof(_,axiom,'vy'),fof(_,axiom,'vz')]).

% ---------------------------------- prepare_formulae ----------------------------------

test(prepare_formulae) :-
	discharge_obligations:prepare_formulae([type~logical_symbol 
                                    ..name~ '~' ..arity~1..id~1 
                                    ..args~[type~variable ..name~x ..arity~0],type~variable ..name~y ..arity~0..id~2,type~variable ..name~z ..arity~0..id~3],
                                   [fof(_,conjecture,'~(vx)'),fof(_,conjecture,'vy'),fof(_,conjecture,'vz')]).


% ---------------------------------- fof_check/6 -------------------------------------------
test(check_valid_fof_0) :-
	!,
	discharge_obligations:clean,
	nb_setval(discharge_theorems,0),
	discharge_obligations:check_conj(
		0,
		[type~relation ..name~'=' ..arity~2 ..args~[type~variable ..name~x ..arity~0,type~variable ..name~z ..arity~0]..id~3],
		[type~relation ..name~'=' ..arity~2 ..args~[type~variable ..name~x ..arity~0,type~variable ..name~y ..arity~0]..id~1,
		 type~relation ..name~'=' ..arity~2 ..args~[type~variable ..name~y ..arity~0,type~variable ..name~z ..arity~0]..id~2],
	        [1-[],2-[],3-[1,2]],
		_GraphOut,
		_Stats,
		_EndResult
                 ),
	!.

test(check_valid_fof_1) :-
	!,
	discharge_obligations:clean,
	nb_setval(discharge_theorems,0),
	discharge_obligations:check_conj(
		0,
		[type~relation ..name~'=' ..arity~2 ..args~[type~variable ..name~x ..arity~0,type~variable ..name~z ..arity~0]..id~3],
		[type~relation ..name~'=' ..arity~2 ..args~[type~variable ..name~x ..arity~0,type~variable ..name~y ..arity~0]..id~1,
		 type~relation ..name~'=' ..arity~2 ..args~[type~variable ..name~y ..arity~0,type~variable ..name~z ..arity~0]..id~2],
	        [1-[],2-[],3-[1,2]],
		_GraphOut,
		_Stats,
		_EndResult
        ),
	!.

test(check_valid_fof_2) :-
	!,
	discharge_obligations:clean,
	nb_setval(discharge_theorems,0),
	discharge_obligations:check_conj(
		0,
		[type~relation ..name~'=' ..arity~2 ..args~[type~variable ..name~x ..arity~0,type~variable ..name~z ..arity~0]..id~3],
		[type~relation ..name~'=' ..arity~2 ..args~[type~variable ..name~x ..arity~0,type~variable ..name~y ..arity~0]..id~1,
		 type~relation ..name~'=' ..arity~2 ..args~[type~variable ..name~y ..arity~0,type~variable ..name~z ..arity~0]..id~2],
	        [1-[],2-[],3-[1,2]],
		_GraphOut,
		_Stats,
		_EndResult
        ),
	!.

test(check_valid_fof_3) :-
	!,
	discharge_obligations:clean,
	nb_setval(discharge_theorems,0),
	discharge_obligations:check_conj(
		0,
		[type~relation ..name~'=' ..arity~2 ..args~[type~variable ..name~x ..arity~0,type~variable ..name~z ..arity~0]..id~3],
		[type~relation ..name~'=' ..arity~2 ..args~[type~variable ..name~x ..arity~0,type~variable ..name~y ..arity~0]..id~1,
		 type~relation ..name~'=' ..arity~2 ..args~[type~variable ..name~y ..arity~0,type~variable ..name~z ..arity~0]..id~2],
	        [1-[],2-[],3-[1,2]],
		_GraphOut,
		_Stats,
		_EndResult
        ),
	!.

test('Get Premise Distance') :-
	discharge_obligations:get_premises_distance(a,[id~b,id~c,id~d],[a-[b],b-[c,d],c-[],d-[c]],X),
	X = [[id~b],[id~c, id~d]].


test('Update Used Stats') :-
	discharge_obligations:update_stats_used([id~a..distance~0,id~b..distance~1,id~c..distance~3],[a,c],3,X),
	X = [id~a..used~yes, id~b..used~no, id~c..used~yes].

:- end_tests(discharge_obligations).
