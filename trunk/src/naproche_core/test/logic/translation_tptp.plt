:- begin_tests(translation_tptp).

%	------------------------------negation----------------------------------

test(negation):-
	translation_tptp:prepare_tptp(type~logical_symbol 
				      ..name~ '~' 
                                      ..arity~1 
                                      ..args~[type~variable ..name~x ..arity~0],
                                      '~(vx)',
                                      []
                                     ).

%	------------------------------and---------------------------------------

test(x_and_y):-
	translation_tptp:prepare_tptp(type~logical_symbol
                                      ..name~ '&' 
                                      ..arity~2 
                                      ..args~[type~variable ..name~x ..arity~0, type~variable ..name~y ..arity~0],
                                      '(vx)&(vy)',
                                      []
                                     ).

%	------------------------------or----------------------------------------


test(x_or_y):-
	translation_tptp:prepare_tptp(type~logical_symbol 
                                      ..name~ '|' 
                                      ..arity~2 
                                      ..args~[type~variable ..name~x ..arity~0, type~variable ..name~y ..arity~0],
                                      '(vx)|(vy)',
                                      []
                                     ).

%	------------------------------implies------------------------------------

test(x_implies_y):-
	translation_tptp:prepare_tptp(type~logical_symbol 
                                      ..name~ '=>' 
                                      ..arity~2 
                                      ..args~[type~variable ..name~x ..arity~0, type~variable ..name~y ..arity~0],
                                      '(vx)=>(vy)',
                                      []
                                     ).

%	------------------------------bi-implication-----------------------------

test(x_equivalent_to_y):-
	translation_tptp:prepare_tptp(type~logical_symbol 
                                      ..name~ '<=>' 
                                      ..arity~2 
                                      ..args~[type~variable ..name~x ..arity~0, type~variable ..name~y ..arity~0],
                                      '(vx)<=>(vy)',
                                      []
                                     ).

%	------------------------------relation-----------------------------------

test(relation):-
	translation_tptp:prepare_tptp(type~relation 
                                      ..name~ 'R' 
                                      ..arity~2 
                                      ..args~[type~variable ..name~x ..arity~0, type~variable ..name~y ..arity~0],
                                      'rR(vx,vy)',
                                      []
                                     ).

%	------------------------------quantifier----------------------------------

test(for_all):-
	translation_tptp:prepare_tptp(type~quantifier 
                                      ..name~ '!' 
                                      ..arity~2 
                                      ..args~[[type~variable ..name~x ..arity~0], type~variable ..name~y ..arity~0],
                                      '![Vx]:(vy)',
                                      []
                                     ).

test(there_exists):-
	translation_tptp:prepare_tptp(type~quantifier 
                                      ..name~ '?' 
                                      ..arity~2 
                                      ..args~[[type~variable ..name~x ..arity~0], type~variable ..name~y ..arity~0],
                                      '?[Vx]:(vy)',
                                      []
                                     ).

%	------------------------------variable------------------------------------

test(variable_free):-
	translation_tptp:prepare_tptp(type~variable 
                                      ..name~ 'x' 
                                      ..arity~0,
                                      'vx',
                                      []
                                     ).

test(variable_bound):-
	translation_tptp:prepare_tptp(type~variable 
                                      ..name~ 'x' 
                                      ..arity~0,
                                      'Vx',
                                      [type~variable..name~ 'x'..arity~0]
                                     ).

%	------------------------------constant------------------------------------

test(constant):-
	translation_tptp:prepare_tptp(type~constant 
                                      ..name~ 'c'  
                                      ..arity~0,
                                      'vc',
                                      []
                                     ).

%	------------------------------a combined example-------------------------

test(example):-
	translation_tptp:prepare_tptp(type~logical_symbol
                                      ..name~'&'
                                      ..arity~2
                                      ..args~[type~quantifier
                                              ..name~!
                                              ..arity~2
                                              ..args~[[type~variable
                                                       ..name~x
                                                       ..arity~0
                                                      ],
                                                      type~relation
                                                      ..name~'='
                                                      ..arity~2
                                                      ..args~[type~variable
                                                              ..name~x
                                                              ..arity~0,
                                                              type~variable
                                                              ..name~x
                                                              ..arity~0
                                                             ]
                                                      ], 
                                               type~logical_symbol
                                               ..name~'~'
                                               ..arity~1
                                               ..args~[type~relation
                                                       ..name~'Ord'
                                                       ..arity~1
                                                       ..args~[type~variable
                                                               ..name~y
                                                               ..arity~0
                                                              ]
                                                      ]
                                               ],
                                         '(![Vx]:(Vx = Vx))&(~(rOrd(vy)))',
                                         []
                                        ).

:- end_tests(translation_tptp).

