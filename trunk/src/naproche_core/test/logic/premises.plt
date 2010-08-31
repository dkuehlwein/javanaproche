:-begin_tests(premises).

% ----------------------- update_definitions --------------------------
test('New Definition with two vars') :- 
	premises:update_definitions([],                                                                      % Input premises
		0,
		[type~variable ..name~x, type~variable ..name~y],			 % List of Mrefs
		[],
		[type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~x]],  % DOBSOD representation of List_A
		[type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~y]],   % DOBSOD representation of List_B
		_,
		[type~quantifier..name~!..arity~2..args~
			[[type~variable..name~x, type~variable..name~y],
			type~logical_symbol..name~ <=> .. arity~2..args~
				[type~relation..name~ord..arity~1..args~[type~variable..name~x], 
				type~relation..name~ord..arity~1..args~[type~variable..name~y]
				]
			]
		]                                                                       % Output premises
	).

test('New Definition with long formulas') :- 
	premises:update_definitions([],                                                  % Input premises
		0,
		[type~variable ..name~x, type~variable ..name~y],
		[],
		[type~relation ..name~'trans' ..arity~1 ..args~[type~variable ..name~x]],% DOBSOD representation of List_A
		[type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~y]],   % DOBSOD representation of List_B
		X,
		[type~quantifier..arity~2..name~!..args~
			[[type~variable..name~x, type~variable..name~y], 
			type~logical_symbol..arity~2..name~ (<=>)..args~
				[type~relation..arity~1..name~trans..args~
					[type~variable..name~x], 
				type~relation..arity~1..name~ord..args~
					[type~variable..name~y]
				]
			]
		] 									 % Output premises
	),
	X = def(0, 1).


test('Error: Empty definition PRS (A)') :- 
	clear_messages,
	\+ premises:update_definitions([],                                                  	% Input premises
		0,
		[type~variable ..name~x, type~variable ..name~y],
		[],
		[], 										% wrong DOBSOD representation of List_A
		[type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~y]],   	% DOBSOD representation of List_B
		_,
		_
	),
	get_messages(Messages),
	Messages = [message(error, logic, update_definition,'0', 'Definiendum PRS is empty')].


test('Error: Empty definition PRS (B)') :- 
	clear_messages,
	\+ premises:update_definitions([],                                                  	% Input premises
		0,
		[type~variable ..name~x, type~variable ..name~y],
		[],
		[type~relation ..name~'trans' ..arity~1 ..args~[type~variable ..name~x]], 	% DOBSOD representation of List_A
		[],   										% wrong DOBSOD representation of List_B
		_,_
	),
	get_messages(Messages),
	Messages = [message(error, logic, update_definition,'0', 'Definiens PRS is empty')].


% ----------------------- update_assumption --------------------------
test('Assumption: Add one premise') :-
% for all x, y: ord(x) => ord(y)
	premises:update_assumption(
		0,
		[],                                                                       % Input premises
		[type~variable..name~x, type~variable..name~y],					  % List of Mrefs
		[],									% AccAfter
        [type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~x]],   						  % DOBSOD representation of List_A
		[type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~y]..id~ordy],    % DOBSOD representation of List_B
		[],
		X,
	       [type~quantifier..name~!..arity~2..args~							     % Output premises
		    [[type~variable..name~x, type~variable..name~y], 
		      type~logical_symbol..name~ (=>)..arity~2..args~
			  [
			  type~relation..name~ord..arity~1..args~[type~variable..name~x], 
			  type~relation..name~ord..arity~1..args~[type~variable..name~y]
			  ]
	   	    ]..id~ass(0,0)
		]        
	),
	X = [0-[ass(0, 0)], ordy-[], ass(0, 0)-[0, ordy]].
							   

/*
% commented out because there isn't yet a rule how to quantify in the '=>' case

test('Assumption: Add two premises') :-
% for all x, y: ord(x) => ord(y), for all x, z: ord(x)=> ord(z)
	premises:update_assumption([],                                                                       % Input premises
		[type~quantifier..name~!..arity~2..args~
			[[type~variable..name~x, type~variable..name~y], 
			 type~logical_symbol..name~ (=>)..arity~2..args~
				[type~relation..name~ord..arity~1..args~[type~variable..name~x], 
				type~relation..name~ord..arity~1..args~[type~variable..name~y]
				]
			], 
			type~quantifier..name~!..arity~2..args~
				[[type~variable..name~x, type~variable..name~z], 
				type~logical_symbol..name~ => .. arity~2..args~
					[type~relation..name~ord..arity~1..args~[type~variable..name~x], 
					type~relation..name~ord..arity~1..args~[type~variable..name~z]
					]
				]
		],                                                                        % Output premises
		[type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~x]],   % DOBSOD representation of List_A
		[type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~y],
		type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~z]     % DOBSOD representation of List_B
		]
	).	
*/


test('Assumption: Add one premise to existing premise') :-
	premises:update_assumption(0,
				[type~variable ..name~z],                                                 % Input premises
				[type~variable..name~x, type~variable..name~y],			     % List of Mrefs
				[],				%AccAfter
				   [type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~x]..id~ordx],   % DOBSOD representation of List_A
				   [type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~y]..id~ordy],    % DOBSOD representation of List_B
				[],
				X,
	                           [type~quantifier..name~!..arity~2..args~
						[[type~variable..name~x, type~variable..name~y], 
						 type~logical_symbol..name~ (=>)..arity~2..args~
							 [type~relation..name~ord..arity~1..args~[type~variable..name~x], 
							  type~relation..name~ord..arity~1..args~[type~variable..name~y]
							 ]
						]..id~ass(0,0),
					type~variable..name~z
				   ]                                                                        % Output premises
				  ),
	X = [0-[ass(0, 0)], ordy-[], ass(0, 0)-[0, ordy]].

test('quantify & skolemize') :-
% premise: x=x, x=y => x=z
	premises:reset_counter,
	premises:update_assumption(
		0,
		[type~relation ..name~'=' ..arity~2 ..args~
			[type~variable ..name~x..dref~0, type~variable ..name~x..dref~0]
		], 
		[type~variable..name~y..dref~1],	%Variables A 
		[math_id(9,type~variable ..name~x..dref~0)], %AccAfter
		[type~relation..name~'='..args~
			[type~variable..name~x..dref~0, type~variable..name~y..dref~1]
		], 
		[type~relation..name~'='..args~
			[type~variable..name~x..dref~0, type~variable..name~z..dref~2]
		..id~eq],
		[],
		Graph,
		[type~quantifier..arity~2..name~!..args~[
			[type~variable..name~y..dref~1], 
			type~logical_symbol..arity~2..name~ (=>)..args~
				[type~relation..name~ (=)..args~
					[type~variable..name~x..dref~0, type~variable..name~y..dref~1], 
				type~relation..name~ (=)..args~
					[
						type~variable..name~x..dref~0, 
						type~function..arity~1..name~skolem2..args~[type~variable..name~y]
					]
				]
			]..id~ass(0,0),
		type~relation..arity~2..name~ (=)..args~
			[type~variable..name~x, type~variable..name~x]

		] 
	),
	Graph = [0-[ass(0, 0)], eq-[], ass(0, 0)-[0, eq]].


test('Assumption: Add one premise with real formulas') :-
% for all x, y: trans(x) => ord(y)
	premises:update_assumption(
		0,
		[],                                                                       % Input premises
		[type~variable..name~x, type~variable..name~y],				  % List of Mrefs
		[], 					%AccAfter
		[type~relation ..name~'trans' ..arity~1 ..args~[type~variable ..name~x]], % DOBSOD representation of List_A
		[type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~y]..id~ordy],    % DOBSOD representation of List_B
		[],
		Graph,
		[type~quantifier..name~!..arity~2..args~
			[
			[type~variable..name~x, type~variable..name~y], 
			type~logical_symbol..name~ (=>)..arity~2..args~
				[type~relation..name~trans..arity~1..args~[type~variable..name~x],
				type~relation..name~ord..arity~1..args~[type~variable..name~y]
				]
			]..id~ass(0,0)	
		]                                                                        % Output premises
	),
	Graph = [0-[ass(0, 0)], ordy-[], ass(0, 0)-[0, ordy]].

test('Assumption: Accafter check') :-
        premises:update_assumption(
                0,
                [],                                                                       % Input premises
                [type~variable..name~x],                           % List of Mrefs
                [math_id(0,type~variable..name~y)],                                     %AccAfter
                [type~relation ..name~'trans' ..arity~1 ..args~[type~variable ..name~x]], % DOBSOD representation of List_A
                [type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~y]..id~ordy],    % DOBSOD representation of List_B
                [],
                Graph,
                [type~quantifier..name~!..arity~2..args~
                        [
                        [type~variable..name~x],
                        type~logical_symbol..name~ (=>)..arity~2..args~
                                [type~relation..name~trans..arity~1..args~[type~variable..name~x],
                                type~relation..name~ord..arity~1..args~[type~variable..name~y]
                                ]
                        ]..id~ass(0,0)
                ]                                                                        % Output premises
        ),
        Graph = [0-[ass(0, 0)], ordy-[], ass(0, 0)-[0, ordy]].
							   


% ---------------------- make_equivalence_formula -------------------------------------------------
test('Make Equivalence Formula') :-	
	premises:make_equivalence_formula(
		0,
		[type~relation ..name~'trans' ..arity~1 ..args~[type~variable ..name~x]], % DOBSOD representation of PremisesA
		[type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~y]],   % DOBSOD representation of PremisesB
		NewFormula),
	NewFormula = [type~logical_symbol..arity~2..name~ (<=>)..args~[
			type~relation..arity~1..name~trans..args~[type~variable..name~x], 
			type~relation..arity~1..name~ord..args~[type~variable..name~y]				
			]..id~eq(0)
		].

% ----------------------- make_conjunction ---------------------------------------------------
/* This test should fail.. or give true
test('Empty list') :-
    premises:make_conjunction([],                                                                       % Input list
	                          []                                                                        % output conjunct
							 ).
*/
test('Single item') :-
	premises:make_conjunction([type~variable ..name~x],                                                  % Input list
	                          type~variable ..name~x                                                     % Output conjunct
							 ).

test('Conjunct list') :-
	premises:make_conjunction([type~variable ..name~x,                       
	                           type~variable ..name~y,
							   type~variable ..name~z
							  ],                                                                         % Input list
	                          type~logical_symbol ..name~'&' ..arity~2 ..args~
							  [type~variable ..name~x,
							   type~logical_symbol ..name~'&' ..arity~2 ..args~
							   [type~variable ..name~y,type~variable ..name~z]
							  ]                                                                          % Output conjunct
							 ).

test('Split up conjunction') :-
	premises:make_conjunction(L,type~logical_symbol..id~and(1, and(2, 3))..name~ & .. arity~2..args~[type~variable..id~1..name~x, type~logical_symbol..id~and(2, 3)..name~ & .. arity~2..args~[type~variable..id~2..name~y, type~variable..id~3..name~z]]),
	L = [type~variable..id~1..name~x, type~variable..id~2..name~y, type~variable..id~3..name~z].


% ----------------------- negate_formulas --------------------------

test('Empty List') :-
    	premises:negate_formulas(0,
				[],                                                                        % Input list
        	                 []                                                                         % Output negated list
				).

test('Negate: One Element') :-
			premises:negate_formulas(0,[type~variable ..name~x],                                                   % Input list
			[type~logical_symbol..arity~1..name~ ~ .. args~[type~variable..name~x]..id~neg(0)]                          % Output negated list
			).


test('Negate: Several Elements') :-
	premises:negate_formulas(0,
				[type~variable ..name~x,
                               type~variable ..name~y,
                               type~variable ..name~z
                           ],                               % Input list
                          [type~logical_symbol..arity~1..name~ ~ ..
                          args~[type~logical_symbol..arity~2..name~ & ..
                          args~[type~variable..name~x, type~logical_symbol..arity~2..name~ & ..
                          args~[type~variable..name~y, type~variable..name~z]]]..id~neg(0)]         % Output negated list
                          ).



% --------------------- quantify_existentially-------------------------
test('Quantify existentially') :-
	premises:quantify_existentially(
		0,
		[type~variable ..name~x], 
		[type~relation ..name~'trans' ..arity~1 ..args~[type~variable ..name~x], 
		type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~x]], 
		X),
	X = [type~quantifier..name~ ? .. args~[
		[type~variable..name~x], 
		type~logical_symbol..arity~2..name~ & .. args~[
			type~relation..arity~1..name~trans..args~[type~variable..name~x], 
			type~relation..arity~1..name~ord..args~[type~variable..name~x]]]
		..id~qe(0)].

test('Quantify existentially with equality advanced') :-
	premises:quantify_existentially(
		0,
		[type~variable ..name~x], 
		[type~relation ..name~'=' ..arity~1..id~1 ..args~[type~variable ..name~x,type~variable ..name~x], 
		type~relation ..name~'=' ..arity~1 ..id~2..args~[type~variable ..name~x,type~variable ..name~y]], 
		X),
		X = [type~relation..name~'$true'].

test('Quantify existentially with equality advanced 2') :-
	premises:quantify_existentially(
		0,
		[type~variable ..name~x], 
		[type~relation ..name~'=' ..arity~1..id~1 ..args~[type~variable ..name~x,type~variable ..name~x], 
		type~relation ..name~'=' ..arity~1 ..id~2..args~[type~variable ..name~x,type~variable ..name~y],
		type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~x]], 
		X),
		X = [type~relation..id~replace(_G242403)..name~ord..arity~1..args~[type~variable..name~y]].

test('Quantify existentially with equality simple') :-
	premises:quantify_existentially(
		0,
		[type~variable ..name~x], 
		[type~relation ..name~'=' ..arity~1 ..args~[type~variable ..name~x,type~variable ..name~x]], 
		X),
		X = [type~relation..name~'$true'].

test('Quantify existentially with equality simple2') :-
	premises:quantify_existentially(
		0,
		[type~variable ..name~x], 
		[type~relation ..name~'=' ..arity~1 ..args~[type~variable ..name~x,type~variable ..name~y]], 
		X),
		X = [type~relation..name~'$true'].

% --------------------- quantify_universally-------------------------
test('Quantify universally') :-
	premises:quantify_universally(
		0,
		[type~variable ..name~x], 
		[type~relation ..name~'trans' ..arity~1 ..args~[type~variable ..name~x], 
		type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~x]], 
		X),
	X = [type~quantifier..name~'!' .. args~[
		[type~variable..name~x], 
		type~logical_symbol..arity~2..name~ & .. args~[
			type~relation..arity~1..name~trans..args~[type~variable..name~x], 
			type~relation..arity~1..name~ord..args~[type~variable..name~x]]]
		].

test('Quantify universally with equality advanced') :-
	premises:quantify_universally(
		0,
		[type~variable ..name~x], 
		[type~logical_symbol..name~'=>'..id~3..args~[
			type~relation ..name~'=' ..arity~1..id~1 ..args~[type~variable ..name~x,type~variable ..name~x], 
			type~relation ..name~'=' ..arity~1 ..id~2..args~[type~variable ..name~x,type~variable ..name~y]]
		], 
		X),
		X = [type~quantifier..id~qu(0, 3)..name~!..dref~_G34248..arity~2..args~[[type~variable..name~x..dref~_G34280..args~[]], type~logical_symbol..id~3..name~ (=>)..dref~_G34314..args~[type~relation..id~1..name~ (=)..dref~_G34346..arity~1..args~[type~variable..name~x..dref~_G34378..args~[], type~variable..name~x..dref~_G34412..args~[]], type~relation..id~2..name~ (=)..dref~_G34446..arity~1..args~[type~variable..name~x..dref~_G34478..args~[], type~variable..name~y..dref~_G34512..args~[]]]]].

test('Quantify universally with equality advanced 2') :-
	premises:quantify_universally(
		0,
		[type~variable ..name~x], 
		[type~logical_symbol..name~'=>'..id~3..args~[
			type~relation ..name~'=' ..arity~1..id~1 ..args~[type~variable ..name~x,type~variable ..name~y], 
			type~relation ..name~'=' ..arity~1 ..id~2..args~[type~variable ..name~x,type~variable ..name~x]]
		],
		X),
		X = [type~relation..id~replace(2)..name~ (=)..arity~1..args~[type~variable..name~y, type~variable..name~y]].

test('Quantify universally with equality simple') :-
	premises:quantify_universally(
		0,
		[type~variable ..name~x], 
		[type~relation ..name~'=' ..id~1..arity~1 ..args~[type~variable ..name~x,type~variable ..name~x]], 
		X),
		X = [type~quantifier..id~qu(0,1)..name~'!'..arity~2..args~[[type~variable..name~x], type~relation..id~1..name~ (=)..arity~1..args~[type~variable..name~x, type~variable..name~x]]].

test('Quantify universally with equality simple2') :-
	premises:quantify_universally(
		0,
		[type~variable ..name~x], 
		[type~relation ..name~'=' ..id~1..arity~1 ..args~[type~variable ..name~x,type~variable ..name~y]], 
		X),
		X = [type~quantifier..id~qu(0, 1)..name~!..arity~2..args~[[type~variable..name~x], type~relation..id~1..name~ (=)..arity~1..args~[type~variable..name~x, type~variable..name~y]]].


% --------------------- make_induction_formulas -------------------------



test('Make induction formulas, x=x') :-
% x=x
	premises:make_induction_formulas(
		0,
		[type~quantifier..name~'!'..arity~2..id~1..args~[
	            [type~variable..name~x..arity~0],
				type~logical_symbol..name~'=>'..id~2..args~[
					type~relation..name~naturalnumber..id~3..args~[type~variable..name~x],
		       		type~relation..name~ (=)..id~4..arity~2..args~[
					type~variable..name~x..arity~0,
					type~variable..name~x..arity~0
				]
				]
			]
		],
		Formula1,
		FormulaSucc),
	Formula1 = [type~relation..id~replace(4)..name~'='..arity~2..args~[type~constant..name~'1'..arity~0, type~constant..name~'1'..arity~0]],
	FormulaSucc = [type~quantifier..id~qu(ind(0), imp(0))..name~!..arity~2..args~[[type~variable..name~x..arity~0], type~logical_symbol..id~imp(0)..name~ (=>)..arity~2..args~[type~logical_symbol..id~and(nat(0), 4)..name~ & .. arity~2..args~[type~relation..id~nat(0)..name~naturalnumber..args~[type~variable..name~x..arity~0], type~relation..id~4..name~ (=)..arity~2..args~[type~variable..name~x..arity~0, type~variable..name~x..arity~0]], type~relation..id~replace(4)..name~ (=)..arity~2..args~[type~function..name~succ..arity~1..args~[type~variable..name~x..arity~0], type~function..name~succ..arity~1..args~[type~variable..name~x..arity~0]]]]].

test('Make induction formulas II, Ord(x)') :-
	premises:make_induction_formulas(
		0,
		[type~quantifier..name~!..arity~2..args~[
	               	[type~variable..name~x..arity~0],
				type~logical_symbol..name~'=>'..args~[
					type~relation..name~naturalnumber..args~[type~variable..name~x],
		       		type~relation..name~(ord)..arity~1..args~[type~variable..name~x..arity~0]
				]
			]
		],
		Formula1,
		FormulaSucc),
	Formula1 = [type~relation..arity~1..name~ord..args~[type~constant..name~'1'..arity~0]],
	FormulaSucc = [type~quantifier..name~!..args~[
			[type~variable..arity~0..name~x], 
			type~logical_symbol..arity~2..name~ (=>)..args~[
				type~logical_symbol..name~'&'..args~[
				type~relation..name~naturalnumber..args~[type~variable..name~x],
				type~relation..arity~1..name~ord..args~[type~variable..arity~0..name~x]
				],
				type~relation..arity~1..name~ord..args~[type~function..arity~1..name~succ..args~[type~variable..arity~0..name~x]]]]].



test('Make induction formulas III, Equivalence') :-
% !y: f(x)=y <=> g(y)=c
	premises:make_induction_formulas(	
		0,
		[type~quantifier..arity~2..name~!..args~[
			[type~variable..arity~0..name~y],
			type~logical_symbol..name~'=>'..args~[
				type~relation..name~naturalnumber..args~[type~variable..name~y],
				type~logical_symbol..arity~2..name~ (<=>)..args~[
					type~relation..arity~2..name~ (=)..args~[
						type~function..name~f..arity~1..args~[type~variable..arity~0..name~x],
						type~variable..arity~0..name~y],
					type~relation..arity~2..name~ (=)..args~[
						type~function..name~g..arity~1..args~[type~variable..arity~0..name~y],
						type~constant..arity~0..name~c]
						]
				]
			]],
		Formula1,
		FormulaSucc),
	Formula1 = [type~logical_symbol..arity~2..name~ (<=>)..args~[
		type~relation..arity~2..name~ (=)..args~[
			type~function..arity~1..name~f..args~[
				type~variable..arity~0..name~x], 
			type~constant..arity~0..name~'1'], 
		type~relation..arity~2..name~ (=)..args~[
			type~function..arity~1..name~g..args~[
				type~constant..arity~0..name~'1'], 
			type~constant..arity~0..name~c]]],
	FormulaSucc = [
			type~logical_symbol..arity~2..name~ (=>)..args~[
				type~logical_symbol..arity~2..name~'&'..args~[
					type~relation..name~naturalnumber..args~[type~function..arity~1..name~f..args~[type~variable..arity~0..name~x]], 
					type~relation..arity~2..name~ (=)..args~[type~function..arity~1..name~g..args~[
						type~function..arity~1..name~f..args~[type~variable..arity~0..name~x]
						], 
						type~constant..arity~0..name~c
					]					
				],
				type~logical_symbol..arity~2..name~ (<=>)..args~[
					type~relation..arity~2..name~ (=)..args~[
						type~function..arity~1..name~f..args~[type~variable..arity~0..name~x], 
						type~function..arity~1..name~succ..args~[type~function..arity~1..name~f..args~[type~variable..arity~0..name~x]]
					], 
					type~relation..arity~2..name~ (=)..args~[
						type~function..arity~1..name~g..args~[
							type~function..arity~1..name~succ..args~[type~function..arity~1..name~f..args~[type~variable..arity~0..name~x]]], 
						type~constant..arity~0..name~c
					]					
				]
			]
		    ].


test('Replace in Formula') :-
	findall(X,premises:replace_in_formula(
		type~variable..name~x..arity~0,
		type~quantifier..name~!..arity~2..args~[
	               	[type~variable..name~x..arity~0],
		       	type~relation..name~ (=)..arity~2..args~[
				type~variable..name~x..arity~0,
				type~variable..name~x..arity~0
				]
			],
		type~constant..name~c..arity~0,
		X),Xs),
	Xs = 	[type~quantifier..arity~2..name~!..args~[
			[type~constant..arity~0..name~c], 
			type~relation..arity~2..name~ (=)..args~[
				type~constant..arity~0..name~c, 
				type~constant..arity~0..name~c]
			]].

test('Replace Variables') :-
	premises:replace_variables(
		type~variable..name~x,
		[
			type~variable..name~x,
			type~function..name~f..arity~2..args~[
				type~variable..name~x,
				type~constant..name~c],
			type~variable..name~y
		],
		type~variable..name~z,NewFormula), 
	NewFormula = [type~variable..name~z, type~function..arity~2..name~f..args~[type~variable..name~z, type~constant..name~c], type~variable..name~y].
	


%------------------------ freevar -------------------------------------

test('Relation') :-
    premises:freevar(type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~x],              % Input DOBSOD        
	                 [type~variable ..name~x],                                                           % Output Free Variables
					 []																					% Skolem Vars
					).
					
test('Function') :-
    premises:freevar(type~function ..name~'g' ..arity~2 ..args~
	                 [type~variable ..name~x,type~variable ..name~y],                                   % Input DOBSOD
	                 [type~variable..name~x, type~variable..name~y],                                     % Output Free Variables
					 []																					% Skolem Vars
					).
					
test('Logical Symbol') :-
    premises:freevar(type~logical_symbol ..name~'=>' ..arity~2 ..args~
	                 [type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~x],
					  type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~y]
					 ],                                                                                 % Input DOBSOD
					 [type~variable..name~x, type~variable..name~y],                                     % Output Free Variables
					 []																					% Skolem Vars
					).

test('skolem free var') :-
    premises:freevar(type~function ..name~'skolem5' ..arity~1 ..args~[type~variable ..name~x],          % Input DOBSOD        
	                 [],                                                           						% Output Free Variables
					 [type~function ..name~'skolem5' ..arity~1 ..args~[type~variable ..name~x]]			% Skolem Vars
					).
%------------------------ plural -----------------------------------------				


test('sort_premises_for_the 1') :-
    premises:reset_counter,
    PIn = [],
	PInAndA = [
    type~relation..name~ (=)..args~[
        type~variable..name~b..arity~0..the~yes,
        type~variable..name~b..arity~0..the~yes]..
    arity~2..the~yes],
	premises:sort_premises_for_the(id,[],PIn,PInAndA,[],GOut,PInAndThe,PA,check),!,
	GOut = [id-[replace(X)], replace(X)-[]],
	PInAndThe = [id~replace(X)..args~[args~[]..name~skolem0..type~function..arity~0, args~[]..name~skolem0..type~function..arity~0]..name~ (=)..type~relation..arity~2],
	PA = [].

test('sort_premises_for_the 2') :-
    premises:reset_counter,
    PIn = [],
	PInAndA = [
    type~relation..name~ (=)..args~[
        type~variable..name~c..arity~0,
        type~variable..name~c..arity~0]..
    arity~2..id~fc,
    type~relation..name~ (=)..args~[
        type~variable..name~b..arity~0..the~yes,
        type~variable..name~b..arity~0..the~yes]..
    arity~2..the~yes..id~fb],
	premises:sort_premises_for_the(id,[],PIn,PInAndA,[],GOut,PInAndThe,PA,check),!,
	GOut = [id-[replace(fb)], replace(fb)-[]],
	PInAndThe = [id~replace(fb)..args~[args~[]..name~skolem0..type~function..arity~0, args~[]..name~skolem0..type~function..arity~0]..name~ (=)..type~relation..arity~2],
	PA = [args~[name~c..type~variable..arity~0, name~c..type~variable..arity~0]..name~ (=)..type~relation..arity~2].

test('sort_premises_for_the 3') :-
    premises:reset_counter,
    PIn = [
    type~relation..name~ (=)..args~[
        type~variable..name~x..arity~0,
        type~variable..name~x..arity~0]..
    arity~2..id~f1],
    PInAndA = [
    type~relation..name~ (=)..args~[
        type~variable..name~c..arity~0,
        type~variable..name~c..arity~0]..
    arity~2..id~f4,
    type~relation..name~ ord..args~[
        type~variable..name~b..arity~0..the~yes,
        type~variable..name~b..arity~0..the~yes]..
    arity~2..the~yes..id~f3,
	type~relation..name~ord..args~[
		type~variable..name~a..arity~0,
		type~variable..name~a..arity~0]..
    arity~2..id~f2,
    type~relation..name~ (=)..args~[
        type~variable..name~x..arity~0,
		type~variable..name~x..arity~0]..
    arity~2..id~f1],
	premises:sort_premises_for_the(id,[],PIn,PInAndA,[],GOut,PInAndThe,PA,check),!,
	GOut = [id-[qu(sort(1), replace(imp(sort(1))))], qu(sort(1), replace(imp(sort(1))))-[]],
	PInAndThe = [type~quantifier..id~qu(sort(1), replace(imp(sort(1))))..name~!..dref~_G15195..arity~2..args~[[type~variable..name~a..dref~G15230..arity~0..args~[]], type~logical_symbol..id~replace(imp(sort(1)))..name~ (=>)..dref~_G15267..arity~2..args~[type~relation..id~replace(f2)..name~ord..dref~_G15302..arity~2..args~[type~variable..name~a..dref~G15230..arity~0..args~[], type~variable..name~a..dref~G15230..arity~0..args~[]], type~relation..id~replace(f3)..name~ord..dref~_G15403..arity~2..args~[type~function..name~skolem0..dref~G15438..arity~1..args~[type~variable..name~a..dref~G15230..arity~0..args~[]], type~function..name~skolem0..dref~G15438..arity~1..args~[type~variable..name~a..dref~G15230..arity~0..args~[]]]]], type~relation..id~f1..name~ (=)..dref~_G15557..arity~2..args~[type~variable..name~x..dref~_G15592..arity~0..args~[], type~variable..name~x..dref~_G15629..arity~0..args~[]]],
	PA = [type~relation..id~f4..name~ (=)..arity~2..args~[type~variable..name~c..arity~0, type~variable..name~c..arity~0], type~relation..id~f2..name~ord..arity~2..args~[type~variable..name~a..dref~G15230..arity~0..args~[], type~variable..name~a..dref~G15230..arity~0..args~[]]] .


test('sort_premises_for_the, not the~yes case 1') :-
    PIn = [],
    PInAndA = [
	    type~relation..name~ord..args~[
	        type~variable..name~b..arity~0,
	        type~variable..name~b..arity~0]..
	    arity~2..the~yes],
    premises:sort_premises_for_the(id,[],PIn,PInAndA,[],GOut,PInAndThe,PA,check),
	GOut = [id-[qu(sort(1), G17875)], qu(sort(1), G17875)-[]],
	PInAndThe = [type~quantifier..id~qu(sort(1), G17875)..name~!..arity~2..args~[[type~variable..name~b..arity~0], type~relation..id~G17875..name~ord..arity~2..args~[type~variable..name~b..arity~0, type~variable..name~b..arity~0]..the~yes]],
	PA = [].


test('sort_premises_for_the nocheck') :-
    premises:reset_counter,
    PIn = [
    type~relation..name~ (=)..args~[
        type~variable..name~x..arity~0,
        type~variable..name~x..arity~0]..
    arity~2..id~f1],
    PInAndA = [
    type~relation..name~ (=)..args~[
        type~variable..name~c..arity~0,
        type~variable..name~c..arity~0]..
    arity~2..id~f4,
    type~relation..name~ (=)..args~[
        type~variable..name~b..arity~0..the~yes,
        type~variable..name~b..arity~0..the~yes]..
    arity~2..the~yes..id~f3,
	type~relation..name~ (=)..args~[
		type~variable..name~a..arity~0,
		type~variable..name~a..arity~0]..
    arity~2..id~f2,
    type~relation..name~ (=)..args~[
        type~variable..name~x..arity~0,
		type~variable..name~x..arity~0]..
    arity~2..id~f1],
	premises:sort_premises_for_the(id,[],PIn,PInAndA,[],GOut,PInAndThe,PA,nocheck),!,
	GOut = [],
	PInAndThe = [id~imp(0)..args~[id~f2..args~[args~[]..dref~G19087..name~a..type~variable..arity~0, args~[]..dref~G19124..name~a..type~variable..arity~0]..dref~G19050..name~ (=)..type~relation..arity~2, id~f3..args~[args~[]..dref~_G19196..name~b..type~variable..arity~0..the~yes, args~[]..dref~_G19233..name~b..type~variable..arity~0..the~yes]..dref~_G19159..name~ (=)..type~relation..arity~2..the~yes]..dref~_G19015..name~ (=>)..type~logical_symbol..arity~2..the~yes, id~f1..args~[args~[]..dref~_G19290..name~x..type~variable..arity~0, args~[]..dref~_G19327..name~x..type~variable..arity~0]..dref~_G19253..name~ (=)..type~relation..arity~2],
	PA = [id~f4..args~[name~c..type~variable..arity~0, name~c..type~variable..arity~0]..name~ (=)..type~relation..arity~2, id~f2..args~[args~[]..dref~G19087..name~a..type~variable..arity~0, args~[]..dref~G19124..name~a..type~variable..arity~0]..dref~G19050..name~ (=)..type~relation..arity~2] .
	

test('sort_premises_for_the, error case') :-
    premises:reset_counter,
	clear_messages,
    PIn = [
    type~relation..name~ (=)..args~[
        type~variable..name~x..arity~0,
		type~variable..name~x..arity~0]..
    arity~2],
    PInAndA = [
    type~relation..name~ (=)..args~[
        type~variable..name~c..arity~0,
        type~variable..name~c..arity~0]..
    arity~2,
    type~relation..name~ (=)..args~[
        type~variable..name~b..arity~0,
        type~variable..name~b..arity~0]..
    arity~2..the~yes,
	type~relation..name~ (=)..args~[
		type~variable..name~a..arity~0,
		type~variable..name~a..arity~0]..
    arity~2],
	\+ premises:sort_premises_for_the(id,[],PIn,PInAndA,[],_GOut,_PInAndThe,_PA,check),
	get_messages(Messages),
	Messages = [message(error, logic, sort_premises_for_the, id, 'Wrong Input. PremisesInAndA != [_|PremisesIn]')].



:-end_tests(premises).
