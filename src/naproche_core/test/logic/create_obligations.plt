:-begin_tests(create_obligations).
:-use_module(naproche(error_logger)).



% assumption marker
:- op(901, xfx, user:(=>)).

% implication marker
:- op(901, xfx, user:(==>)).

% definition marker
:- op(901, xfx, user:(:=)).

% disjunction marker
:- op(901, xfx, user:(v)).

%equivalence marker
:- op(901, xfx, user:(<=>)).

% exclusive disjunction marker
:- op(902, fx, user:(o)).


% ---------------------- Sample PRS for testing purposes ----------------


p(exclusive_or_four,PRS) :-
% (?x,y x<y) >< x=y >< y<x >< xry 
	!,
	dobsod('x less than y',DOBSOD_x_less_y),
	dobsod('y less than x',DOBSOD_y_less_x),
	phrase(fo_grammar:fo_formula(DOBSOD_x_equals_y,_),[x,'=',y]),!,
	phrase(fo_grammar:fo_formula(DOBSOD_x_r_y,_),[x,r,y]),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_y,_),[y]),!,
	PRS = id~5..
	      drefs~[]..
              mrefs~[]..
              conds~[
	        ><(
		[id~3..
	      	drefs~[5, 6, 7]..
              	mrefs~[DOBSOD_x_less_y, DOBSOD_x, DOBSOD_y]..
              	conds~[math_id(5, DOBSOD_x_less_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y), holds(5)]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(5, DOBSOD_x_less_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y)]
		,
		id~4..
	      	drefs~[8]..
             	mrefs~[DOBSOD_x_equals_y]..
              	conds~[math_id(8, DOBSOD_x_equals_y), holds(8)]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(5, DOBSOD_x_less_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y), math_id(8, DOBSOD_x_equals_y)]
		,
		id~5..
	      	drefs~[9]..
             	mrefs~[DOBSOD_y_less_x]..
              	conds~[math_id(9, DOBSOD_y_less_x),holds(9)]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(5, DOBSOD_x_less_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y), math_id(8, DOBSOD_x_equals_y), math_id(9, DOBSOD_y_less_x)]
		,
		id~6..
	      	drefs~[10]..
             	mrefs~[DOBSOD_x_r_y]..
              	conds~[math_id(10, DOBSOD_x_r_y), holds(10)]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(5, DOBSOD_x_less_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y), math_id(8, DOBSOD_x_equals_y), math_id(9, DOBSOD_y_less_x), math_id(10, DOBSOD_x_r_y)]
		])
              ]..
	      rrefs~[]..
	      accbefore~[]..
	      accafter~[].

p(exclusive_or_three,PRS) :-
% (?x,y x<y) >< (?x,y x=y) >< (?x,y y<x) 
	!,
	dobsod('x less than y',DOBSOD_x_less_y),
	dobsod('y less than x',DOBSOD_y_less_x),
	phrase(fo_grammar:fo_formula(DOBSOD_x_equals_y,_),[ord,'(',x,',',y,')']),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_y,_),[y]),!,
	PRS = id~5..
	      drefs~[]..
              mrefs~[]..
              conds~[
	        ><(
		[id~3..
	      	drefs~[5, 6, 7]..
              	mrefs~[DOBSOD_x_less_y, DOBSOD_x, DOBSOD_y]..
              	conds~[math_id(5, DOBSOD_x_less_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y), holds(5)]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(5, DOBSOD_x_less_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y)]
		,
		id~4..
	      	drefs~[8,6,7]..
             	mrefs~[DOBSOD_x_equals_y, DOBSOD_x, DOBSOD_y]..
              	conds~[math_id(8, DOBSOD_x_equals_y), holds(8)]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(5, DOBSOD_x_less_y), math_id(6, DOBSOD_x),math_id(7, DOBSOD_y), math_id(8, DOBSOD_x_equals_y)]
		,
		id~5..
	      	drefs~[9,6,7]..
             	mrefs~[DOBSOD_y_less_x, DOBSOD_x, DOBSOD_y]..
              	conds~[math_id(9, DOBSOD_y_less_x),holds(9)]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(5, DOBSOD_x_less_y), math_id(6, DOBSOD_x),math_id(7, DOBSOD_y), math_id(8, DOBSOD_x_equals_y), math_id(9, DOBSOD_y_less_x)]
		])
              ]..
	      rrefs~[]..
	      accbefore~[]..
	      accafter~[].

p(exclusive_or_two,PRS) :-
% ?x not x=x >< ?x x=x
	!,
	dobsod('not ord(x,x)',DOBSOD_not_x_x),
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	phrase(fo_grammar:fo_formula(DOBSOD_x_x,_),[ord,'(',x,',',x,')']),!,
	PRS = id~5..
	      drefs~[]..
              mrefs~[]..
              conds~[
	        ><(
		[id~3..
	      	drefs~[5, 6]..
              	mrefs~[DOBSOD_not_x_x, DOBSOD_x]..
              	conds~[math_id(5, DOBSOD_not_x_x), math_id(6, DOBSOD_x), holds(5)]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(5, DOBSOD_not_x_x), math_id(6, DOBSOD_x)]
		,
		id~4..
	      	drefs~[8,7]..
             	mrefs~[DOBSOD_x_x, DOBSOD_x]..
              	conds~[math_id(8, DOBSOD_x_x),holds(8)]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(5, DOBSOD_not_x_x), math_id(7, DOBSOD_x),math_id(8, DOBSOD_x_x)]
		])
              ]..
	      rrefs~[]..
	      accbefore~[]..
	      accafter~[].

p(exclusivity_of_four_cases,PRS) :-
% (?x,y x<y) <> (?x,y x=y) <> (?x,y y<x) <> (?x,y xry) 
	!,
	dobsod('x less than y',DOBSOD_x_less_y),
	dobsod('y less than x',DOBSOD_y_less_x),
	phrase(fo_grammar:fo_formula(DOBSOD_x_equals_y,_),[x,'=',y]),!,
	phrase(fo_grammar:fo_formula(DOBSOD_x_r_y,_),[x,r,y]),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_y,_),[y]),!,
	PRS = id~5..
	      drefs~[]..
              mrefs~[]..
              conds~[
	        <>(
		[id~3..
	      	drefs~[5, 6, 7]..
              	mrefs~[DOBSOD_x_less_y, DOBSOD_x, DOBSOD_y]..
              	conds~[math_id(5, DOBSOD_x_less_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y), holds(5)]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(5, DOBSOD_x_less_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y)]
		,
		id~4..
	      	drefs~[8,6,7]..
             	mrefs~[DOBSOD_x_equals_y]..
              	conds~[math_id(8, DOBSOD_x_equals_y), holds(8)]..
	      	rrefs~[]..
	      	accbefore~[math_id(5, DOBSOD_x_less_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y)]..
	      	accafter~[math_id(5, DOBSOD_x_less_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y), math_id(8, DOBSOD_x_equals_y)]
		,
		id~5..
	      	drefs~[9,6,7]..
             	mrefs~[DOBSOD_y_less_x]..
              	conds~[math_id(9, DOBSOD_y_less_x),holds(9)]..
	      	rrefs~[]..
	      	accbefore~[math_id(5, DOBSOD_x_less_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y), math_id(8, DOBSOD_x_equals_y)]..
	      	accafter~[math_id(5, DOBSOD_x_less_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y), math_id(8, DOBSOD_x_equals_y), math_id(9, DOBSOD_y_less_x)]
		,
		id~6..
	      	drefs~[10,6,7]..
             	mrefs~[DOBSOD_x_r_y]..
              	conds~[math_id(10, DOBSOD_x_r_y), holds(10)]..
	      	rrefs~[]..
	      	accbefore~[math_id(5, DOBSOD_x_less_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y), math_id(8, DOBSOD_x_equals_y), math_id(9, DOBSOD_y_less_x)]..
	      	accafter~[math_id(5, DOBSOD_x_less_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y), math_id(8, DOBSOD_x_equals_y), math_id(9, DOBSOD_y_less_x), math_id(10, DOBSOD_x_r_y)]
		])
              ]..
	      rrefs~[]..
	      accbefore~[]..
	      accafter~[].

p(exclusivity_of_three_cases,PRS) :-
% (?x,y x<y) <> (?x,y x=y) <> (?x,y y<x) 
	!,
	dobsod('x less than y',DOBSOD_x_less_y),
	dobsod('y less than x',DOBSOD_y_less_x),
	phrase(fo_grammar:fo_formula(DOBSOD_x_equals_y,_),[ord,'(',x,',',y,')']),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_y,_),[y]),!,
	PRS = id~5..
	      drefs~[]..
              mrefs~[]..
              conds~[
	        <>(
		[id~3..
	      	drefs~[5, 6, 7]..
              	mrefs~[DOBSOD_x_less_y, DOBSOD_x, DOBSOD_y]..
              	conds~[math_id(5, DOBSOD_x_less_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y), holds(5)]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(5, DOBSOD_x_less_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y)]
		,
		id~4..
	      	drefs~[8,6,7]..
             	mrefs~[DOBSOD_x_equals_y, DOBSOD_x, DOBSOD_y]..
              	conds~[math_id(8, DOBSOD_x_equals_y), holds(8)]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(5, DOBSOD_x_less_y), math_id(6, DOBSOD_x),math_id(7, DOBSOD_y), math_id(8, DOBSOD_x_equals_y)]
		,
		id~5..
	      	drefs~[9,6,7]..
             	mrefs~[DOBSOD_y_less_x, DOBSOD_x, DOBSOD_y]..
              	conds~[math_id(9, DOBSOD_y_less_x),holds(9)]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(5, DOBSOD_x_less_y), math_id(6, DOBSOD_x),math_id(7, DOBSOD_y), math_id(8, DOBSOD_x_equals_y), math_id(9, DOBSOD_y_less_x)]
		])
              ]..
	      rrefs~[]..
	      accbefore~[]..
	      accafter~[].

p(exclusivity_of_two_cases,PRS) :-
% (?x not ord(x,x)) <> (?x ord(x,x))
	!,
	dobsod('not ord(x,x)',DOBSOD_not_x_x),
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	phrase(fo_grammar:fo_formula(DOBSOD_x_x,_),[ord,'(',x,',',x,')']),!,
	PRS = id~5..
	      drefs~[]..
              mrefs~[]..
              conds~[
	        <>(
		[id~3..
	      	drefs~[5, 6]..
              	mrefs~[DOBSOD_not_x_x, DOBSOD_x]..
              	conds~[math_id(5, DOBSOD_not_x_x), math_id(6, DOBSOD_x), holds(5)]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(5, DOBSOD_not_x_x), math_id(6, DOBSOD_x)]
		,
		id~4..
	      	drefs~[8,6]..
             	mrefs~[DOBSOD_x_x, DOBSOD_x]..
              	conds~[math_id(8, DOBSOD_x_x),holds(8)]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(5, DOBSOD_not_x_x), math_id(6, DOBSOD_x),math_id(8, DOBSOD_x_x)]
		])
              ]..
	      rrefs~[]..
	      accbefore~[]..
	      accafter~[].


% Empty PRS
p(empty,PRS) :- 
	!,
	PRS = id~1..
	      drefs~[]..
	      mrefs~[]..
	      conds~[]..
	      rrefs~[]..
	      accbefore~[]..
	      accafter~[].

% Math_id PRS
p(math_id,PRS) :- 
	!,
	phrase(fo_grammar:fo_term(DOBSOD,_),[x]),
	PRS = id~2..
	      drefs~[5]..
	      mrefs~[DOBSOD]..
	      conds~[math_id(5, DOBSOD)]..
	      rrefs~[]..
	      accbefore~[]..
	      accafter~[math_id(5, DOBSOD)].


% holds + math_id PRS
% Formula: x = x
p(holds,PRS) :- 
	!,
	phrase(fo_grammar:fo_formula(DOBSOD_expr,_),[x,'=',x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_term,_),[x]),!,
	PRS = id~3..
	      drefs~[5, 6]..
              mrefs~[DOBSOD_expr, DOBSOD_term]..
              conds~[math_id(6, DOBSOD_term), math_id(5, DOBSOD_expr), holds(5)]..
	      rrefs~[]..
	      accbefore~[]..
	      accafter~[math_id(6, DOBSOD_term), math_id(5, DOBSOD_expr)].

% wrong PRS
% Formula: not x=x
p(wrong,PRS) :- 
	!,
	dobsod('not x=x', DOBSOD_expr),	
	phrase(fo_grammar:fo_term(DOBSOD_term,_),[x]),!,
	PRS = id~3..
	      drefs~[5, 6]..
              mrefs~[DOBSOD_expr, DOBSOD_term]..
              conds~[math_id(6, DOBSOD_term), math_id(5, DOBSOD_expr), holds(5)]..
	      rrefs~[]..
	      accbefore~[]..
	      accafter~[math_id(6, DOBSOD_term), math_id(5, DOBSOD_expr)].


% Disjunction
% y=y | x=x
p(disjunction,PRS) :- 
	!,
	phrase(fo_grammar:fo_formula(DOBSOD_y_y,_),[y,'=',y]),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_y,_),[y]),!,
	phrase(fo_grammar:fo_formula(DOBSOD_x_x,_),[x,'=',x]),!,
	PRS = id~5..
	      drefs~[]..
              mrefs~[DOBSOD_x, DOBSOD_y]..
              conds~[
		id~3..
	      	drefs~[5]..
              	mrefs~[DOBSOD_y_y]..
              	conds~[math_id(5, DOBSOD_y_y),holds(5)]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(5, DOBSOD_y_y)]
		v
		id~4..
	      	drefs~[8]..
             	mrefs~[DOBSOD_x_x]..
              	conds~[math_id(8, DOBSOD_x_x),holds(8)]..
	      	rrefs~[]..
	      	accbefore~[math_id(5, DOBSOD_y_y)]..
	      	accafter~[math_id(5, DOBSOD_y_y), math_id(8, DOBSOD_x_x)]
              ]..
	      rrefs~[]..
	      accbefore~[]..
	      accafter~[math_id(5,DOBSOD_x), math_id(6,DOBSOD_y)].


% Disjunction, A is empty
% _ | x=x
p(disjunction_A_empty,PRS) :- 
	!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	phrase(fo_grammar:fo_formula(DOBSOD_x_x,_),[ord,'(',x,')']),!,
	PRS = id~5..
	      drefs~[]..
              mrefs~[]..
              conds~[
		id~3..
	      	drefs~[]..
              	mrefs~[]..
              	conds~[]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[]
		v
		id~4..
	      	drefs~[5,8]..
             	mrefs~[DOBSOD_x_x, DOBSOD_x]..
              	conds~[math_id(8, DOBSOD_x_x),holds(8)]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(5,DOBSOD_x),math_id(8, DOBSOD_x_x)]
              ]..
	      rrefs~[]..
	      accbefore~[]..
	      accafter~[].

% Disjunction, B is empty
% x=y | _
p(disjunction_B_empty,PRS) :- 
	!,
	phrase(fo_grammar:fo_formula(DOBSOD_x_y,_),[ord,'(',x,',',y,')']),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_y,_),[y]),!,
	PRS = id~5..
	      drefs~[]..
              mrefs~[]..
              conds~[
		id~3..
	      	drefs~[5, 6, 7]..
              	mrefs~[DOBSOD_x_y, DOBSOD_x, DOBSOD_y]..
              	conds~[math_id(5, DOBSOD_x_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y),holds(5)]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(5, DOBSOD_x_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y)]
		v
		id~4..
	      	drefs~[]..
             	mrefs~[]..
              	conds~[]..
	      	rrefs~[]..
	      	accbefore~[math_id(5, DOBSOD_x_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y)]..
	      	accafter~[math_id(5, DOBSOD_x_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y)]
              ]..
	      rrefs~[]..
	      accbefore~[]..
	      accafter~[].

p(contradiction,PRS) :- 
	!,
	PRS = id~1..
	      drefs~[]..
	      mrefs~[]..
	      conds~[contradiction]..
	      rrefs~[]..
	      accbefore~[]..
	      accafter~[].


% Definition PRS
% Formula x = y := x = x
p(definition,PRS) :-
	!,
	phrase(fo_grammar:fo_formula(DOBSOD_x_y,_),[x,'=',y]),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_y,_),[y]),!,
	phrase(fo_grammar:fo_formula(DOBSOD_x_x,_),[x,'=',x]),!,
	PRS = id~5..
	      drefs~[]..
              mrefs~[]..
              conds~[
		id~3..
	      	drefs~[5, 6, 7]..
              	mrefs~[DOBSOD_x_y, DOBSOD_x, DOBSOD_y]..
              	conds~[math_id(5, DOBSOD_x_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y),holds(5)]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(5, DOBSOD_x_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y)]
		:=
		id~4..
	      	drefs~[8]..
             	mrefs~[DOBSOD_x_x]..
              	conds~[math_id(8, DOBSOD_x_x),holds(8)]..
	      	rrefs~[]..
	      	accbefore~[math_id(5, DOBSOD_x_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y)]..
	      	accafter~[math_id(5, DOBSOD_x_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y), math_id(8, DOBSOD_x_x)]
              ]..
	      rrefs~[]..
	      accbefore~[]..
	      accafter~[].


% Formula !x,y: x=y ==>x=y
p(assumption,PRS) :-
	!,
	phrase(fo_grammar:fo_formula(DOBSOD_expr,_),[ord,'(',x,',',y,')']),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_y,_),[y]),!,
	PRS =
	id~proof_1..
	drefs~[]..
	mrefs~[]..
	conds~[
		id~1..
		drefs~[73, 74, 75]..
		mrefs~[DOBSOD_expr, DOBSOD_x, DOBSOD_y]..
		conds~[math_id(73, DOBSOD_expr), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y),holds(73)]..
		rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(73, DOBSOD_expr), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y)]
		==>
		id~consec_76..
		drefs~[]..
		mrefs~[]..
		conds~[
			id~2..
			drefs~[]..
			mrefs~[DOBSOD_expr]..
			conds~[holds(73)]..
			rrefs~[]..
	      		accbefore~[math_id(73, DOBSOD_expr), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y)]..
	      		accafter~[math_id(73, DOBSOD_expr), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y)]
		]..
		rrefs~[]..
		accbefore~[math_id(73, DOBSOD_expr), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y)]..
	      	accafter~[math_id(73, DOBSOD_expr), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y)]
	]..
	rrefs~[]..
	accbefore~[]..
	accafter~[].


p(assumption_two_conds,PRS) :-
%! x,y,z: (x=y & y=z) => x=z
	!,
	phrase(fo_grammar:fo_formula(DOBSOD_x_y,_),[x,'=',y]),!,
	phrase(fo_grammar:fo_formula(DOBSOD_y_z,_),[y,'=',z]),!,
	phrase(fo_grammar:fo_formula(DOBSOD_x_z,_),[x,'=',z]),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_y,_),[y]),!,
	phrase(fo_grammar:fo_term(DOBSOD_z,_),[z]),!,
	PRS =
	id~proof_1..
	drefs~[]..
	mrefs~[]..
	conds~[
		id~1..
		drefs~[73, 74, 75, 76, 77]..
		mrefs~[DOBSOD_x_y, DOBSOD_x, DOBSOD_y, DOBSOD_z]..
		conds~[math_id(73, DOBSOD_x_y), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y), math_id(76, DOBSOD_y_z), math_id(77, DOBSOD_z), holds(73), holds(76)]..
		rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(73, DOBSOD_x_y), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y), math_id(76, DOBSOD_y_z), math_id(77, DOBSOD_z)]
		==>
		id~consec_76..
		drefs~[]..
		mrefs~[]..
		conds~[
			id~2..
			drefs~[81]..
			mrefs~[]..
			conds~[math_id(81, DOBSOD_x_z),holds(81)]..
			rrefs~[]..
	      		accbefore~[math_id(73, DOBSOD_x_y), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y), math_id(76, DOBSOD_y_z), math_id(77, DOBSOD_z)]..
	      		accafter~[math_id(73, DOBSOD_x_y), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y), math_id(76, DOBSOD_y_z), math_id(77, DOBSOD_z), math_id(81, DOBSOD_x_z)]
		]..
		rrefs~[]..
		accbefore~[]..
	      	accafter~[math_id(73, DOBSOD_x_y), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y), math_id(76, DOBSOD_y_z), math_id(77, DOBSOD_z), math_id(81, DOBSOD_x_z)]
	]..
	rrefs~[]..
	accbefore~[]..
	accafter~[math_id(73, DOBSOD_x_y), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y), math_id(76, DOBSOD_y_z), math_id(77, DOBSOD_z), math_id(81, DOBSOD_x_z)].

p(assumption_contradiction,PRS) :-
	!,
	phrase(fo_grammar:fo_formula(DOBSOD_expr,_),[x,'=',y]),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_y,_),[y]),!,
	PRS =
	id~proof_1..
	drefs~[]..
	mrefs~[]..
	conds~[
		id~1..
		drefs~[73, 74, 75]..
		mrefs~[DOBSOD_expr, DOBSOD_x, DOBSOD_y]..
		conds~[math_id(73, DOBSOD_expr), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y), holds(73)]..
		rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(73, DOBSOD_expr), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y)]
		==>
		id~consec_76..
		drefs~[]..
		mrefs~[]..
		conds~[
			id~2..
			drefs~[81, 82]..
			mrefs~[DOBSOD_expr, DOBSOD_x, DOBSOD_y]..
			conds~[math_id(81, DOBSOD_expr), holds(81)]..
			rrefs~[]..
	      		accbefore~[math_id(73, DOBSOD_expr), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y)]..
	      		accafter~[math_id(73, DOBSOD_expr), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y), math_id(81, DOBSOD_expr)]
			,
			contradiction
		]..
		rrefs~[]..
	      	accbefore~[math_id(73, DOBSOD_expr), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y)]..
	      	accafter~[math_id(73, DOBSOD_expr), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y), math_id(81, DOBSOD_expr)]
	]..
	rrefs~[]..
	accbefore~[]..
	accafter~[].

% Antecendent PRS is empty
% For all x x=x
p(assumption_a_empty,PRS) :-
	!,
	phrase(fo_grammar:fo_formula(DOBSOD_expr,_),[x,'=',x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	PRS =
	id~0..
	drefs~[]..
	mrefs~[]..
	conds~[
		id~restrictor(0)..
		drefs~[0]..
		mrefs~[DOBSOD_x]..
		conds~[math_id(0, DOBSOD_x)]..
		rrefs~[]..
		accbefore~[]..
		accafter~[math_id(0, DOBSOD_x)]
		==>
		id~scope(0)..
		drefs~[1]..
		mrefs~[DOBSOD_expr]..
		conds~[ math_id(1, DOBSOD_expr), holds(1)]..
		rrefs~[]..
		accbefore~[math_id(0, DOBSOD_x)]..
		accafter~[math_id(0, DOBSOD_x), math_id(1, DOBSOD_expr)]
	]..
	rrefs~[]..
	accbefore~[]..
	accafter~[].

% Antecendent PRS is empty
% For all x,y: x=x, y=y
p(assumption_a_empty_two_conds,PRS) :-
	!,
	phrase(fo_grammar:fo_formula(DOBSOD_expr_x,_),[x,'=',x]),!,
	phrase(fo_grammar:fo_formula(DOBSOD_expr_y,_),[y,'=',y]),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_y,_),[y]),!,
	PRS =
	id~0..
	drefs~[]..
	mrefs~[]..
	conds~[
		id~restrictor(0)..
		drefs~[0, 1]..
		mrefs~[DOBSOD_x, DOBSOD_y]..
		conds~[math_id(0, DOBSOD_x), math_id(1, DOBSOD_y)]..
		rrefs~[]..
		accbefore~[]..
		accafter~[math_id(0, DOBSOD_x), math_id(1, DOBSOD_y)]
		==>
		id~scope(0)..
		drefs~[2, 3]..
		mrefs~[DOBSOD_expr_x, DOBSOD_expr_y]..
		conds~[math_id(2, DOBSOD_expr_x), holds(2), math_id(3, DOBSOD_expr_y), holds(3)]..
		rrefs~[]..
		accbefore~[math_id(0, DOBSOD_x), math_id(1, DOBSOD_y)]..
		accafter~[math_id(0, DOBSOD_x), math_id(1, DOBSOD_y), math_id(2, DOBSOD_expr_x), math_id(3, DOBSOD_expr_y)]
	]..
	rrefs~[]..
	accbefore~[]..
	accafter~[].

% Formula not x = y
p(negation,PRS) :-
	!,
	phrase(fo_grammar:fo_formula(DOBSOD_expr,_),[ord,'(',x,',',y,')']),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_y,_),[y]),!,
	PRS =
	id~proof_1..
	drefs~[]..
	mrefs~[]..
	conds~[
		neg(
		id~0..
		drefs~[0, 1, 2]..
		mrefs~[DOBSOD_expr, DOBSOD_x, DOBSOD_y]..
		conds~[math_id(0, DOBSOD_expr), math_id(1, DOBSOD_x), math_id(2, DOBSOD_y), holds(0)]..
		rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(0, DOBSOD_expr), math_id(1, DOBSOD_x), math_id(2, DOBSOD_y)])
	]..
	rrefs~[]..
	accbefore~[]..
	accafter~[].


% Formula for all
% For all x x=x
p(for_all,PRS) :-
	!,
	phrase(fo_grammar:fo_formula(DOBSOD_expr,_),[x,'=',x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	PRS =
	id~0..
	drefs~[]..
	mrefs~[]..
	conds~[
		id~restrictor(0)..
		drefs~[0]..
		mrefs~[DOBSOD_x]..
		conds~[math_id(0, DOBSOD_x)]..
		rrefs~[]..
		accbefore~[]..
		accafter~[math_id(0, DOBSOD_x)]
		=>
		id~scope(0)..
		drefs~[1]..
		mrefs~[DOBSOD_expr]..
		conds~[ math_id(1, DOBSOD_expr), holds(1)]..
		rrefs~[]..
		accbefore~[math_id(0, DOBSOD_x)]..
		accafter~[math_id(0, DOBSOD_x), math_id(1, DOBSOD_expr)]
	]..
	rrefs~[]..
	accbefore~[]..
	accafter~[].

	
% Formula Implies
% x=y => y=x

p(implies,PRS) :-
% !x,y: x=y => y=x
	!,
	phrase(fo_grammar:fo_formula(DOBSOD_x_y,_),[ord,'(',x,',',y,')']),!,
	phrase(fo_grammar:fo_formula(DOBSOD_y_x,_),[ord,'(',y,',',x,')']),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_y,_),[y]),!,

	PRS =
	id~proof_1..
	drefs~[]..
	mrefs~[]..
	conds~[
		id~0..
		drefs~[]..
		mrefs~[]..
		conds~[
			id~antecedent_10..
			drefs~[6, 7, 8]..
			mrefs~[DOBSOD_x_y, DOBSOD_x, DOBSOD_y]..
			conds~[math_id(6, DOBSOD_x_y), math_id(7, DOBSOD_x), math_id(8, DOBSOD_y),holds(6) ]..
			rrefs~[]..
			accbefore~[]..
			accafter~[math_id(6, DOBSOD_x_y), math_id(7, DOBSOD_x), math_id(8, DOBSOD_y)]
			=>
			id~succedent_11..
			drefs~[9]..
			mrefs~[DOBSOD_y_x]..
			conds~[math_id(9, DOBSOD_y_x), holds(9)]..
			rrefs~[]..
			accbefore~[math_id(6, DOBSOD_x_y), math_id(7, DOBSOD_x), math_id(8, DOBSOD_y)]..
			accafter~[math_id(6, DOBSOD_x_y), math_id(7, DOBSOD_x), math_id(8, DOBSOD_y), math_id(9, DOBSOD_y_x)]
			]..
		rrefs~[]..
		accbefore~[]..
		accafter~[]
	]..
	rrefs~[]..
	accbefore~[]..
	accafter~[].

p(implication_two_conds,PRS) :-
%! x,y,z: (x=y & y=z) => x=z
	!,
	phrase(fo_grammar:fo_formula(DOBSOD_x_y,_),[x,'=',y]),!,
	phrase(fo_grammar:fo_formula(DOBSOD_y_z,_),[y,'=',z]),!,
	phrase(fo_grammar:fo_formula(DOBSOD_x_z,_),[x,'=',z]),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_y,_),[y]),!,
	phrase(fo_grammar:fo_term(DOBSOD_z,_),[z]),!,
	PRS =
	id~proof_1..
	drefs~[]..
	mrefs~[]..
	conds~[
		id~1..
		drefs~[73, 74, 75, 76, 77]..
		mrefs~[DOBSOD_x_y, DOBSOD_x, DOBSOD_y, DOBSOD_z]..
		conds~[math_id(73, DOBSOD_x_y), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y), math_id(76, DOBSOD_y_z), math_id(77, DOBSOD_z), holds(73), holds(76)]..
		rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(73, DOBSOD_x_y), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y), math_id(76, DOBSOD_y_z), math_id(77, DOBSOD_z)]
		=>
		id~consec_76..
		drefs~[]..
		mrefs~[]..
		conds~[
			id~2..
			drefs~[81]..
			mrefs~[]..
			conds~[math_id(81, DOBSOD_x_z),holds(81)]..
			rrefs~[]..
	      		accbefore~[math_id(73, DOBSOD_x_y), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y), math_id(76, DOBSOD_y_z), math_id(77, DOBSOD_z)]..
	      		accafter~[math_id(73, DOBSOD_x_y), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y), math_id(76, DOBSOD_y_z), math_id(77, DOBSOD_z), math_id(81, DOBSOD_x_z)]
		]..
		rrefs~[]..
		accbefore~[]..
	      	accafter~[math_id(73, DOBSOD_x_y), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y), math_id(76, DOBSOD_y_z), math_id(77, DOBSOD_z), math_id(81, DOBSOD_x_z)]
	]..
	rrefs~[]..
	accbefore~[]..
	accafter~[math_id(73, DOBSOD_x_y), math_id(74, DOBSOD_x), math_id(75, DOBSOD_y), math_id(76, DOBSOD_y_z), math_id(77, DOBSOD_z), math_id(81, DOBSOD_x_z)].


p(implies2,PRS) :-
% !x x=x => ?y x=y
	!,
	build_prs(PRS,[
	sentence(1,['let',math([x,=,x])]),
	sentence(2,[math([x,=,y])])
	]),!.

% Reverse Implication
% x=y <= y=x
p(reverse_implies,PRS) :-
	!,
	phrase(fo_grammar:fo_formula(DOBSOD_x_y,_),[x,'=',y]),!,
	phrase(fo_grammar:fo_formula(DOBSOD_y_x,_),[y,'=',x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_y,_),[y]),!,

	PRS =
	id~proof_1..
	drefs~[]..
	mrefs~[]..
	conds~[
		id~0..
		drefs~[]..
		mrefs~[]..
		conds~[
			id~succedent_10..
			drefs~[6]..
			mrefs~[DOBSOD_x_y, DOBSOD_x, DOBSOD_y]..
			conds~[math_id(6, DOBSOD_x_y), math_id(7, DOBSOD_x), math_id(8, DOBSOD_y),holds(6) ]..
			rrefs~[]..
			accbefore~[]..
			accafter~[math_id(6, DOBSOD_x_y), math_id(7, DOBSOD_x), math_id(8, DOBSOD_y)]
			<=
			id~antecedent_11..
			drefs~[9]..
			mrefs~[DOBSOD_y_x]..
			conds~[math_id(9, DOBSOD_y_x), holds(9)]..
			rrefs~[]..
			accbefore~[math_id(6, DOBSOD_x_y), math_id(7, DOBSOD_x), math_id(8, DOBSOD_y)]..
			accafter~[math_id(6, DOBSOD_x_y), math_id(7, DOBSOD_x), math_id(8, DOBSOD_y), math_id(9, DOBSOD_y_x)]
			]..
		rrefs~[]..
		accbefore~[]..
		accafter~[]
	]..
	rrefs~[]..
	accbefore~[]..
	accafter~[].


% Theorem PRS
p(theorem,PRS) :-
	!,
	phrase(fo_grammar:fo_formula(DOBSOD_expr,_),[x,'=',x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	PRS =
	id~proof_1..
	drefs~[]..
	mrefs~[]..
	conds~[
		theorem(theorem,
			id~goal_2..
			drefs~[]..
			mrefs~[]..
			conds~[
				id~1..
				drefs~[0, 1]..
				mrefs~[DOBSOD_expr, DOBSOD_x]..
				conds~[math_id(1, DOBSOD_x), math_id(0, DOBSOD_expr), holds(0) ]..
				rrefs~[]..
				accbefore~[]..
				accafter~[math_id(1, DOBSOD_x), math_id(0, DOBSOD_expr)]
			]..
			rrefs~[]..
			accbefore~[]..
			accafter~[math_id(1, DOBSOD_x), math_id(0, DOBSOD_expr)]
			,
			id~body_2..
			drefs~[]..
			mrefs~[]..
			conds~[
				id~3..
				drefs~[]..
				mrefs~[DOBSOD_expr, DOBSOD_x]..
				conds~[math_id(3, DOBSOD_x), math_id(4, DOBSOD_expr),holds(4)]..
				rrefs~[]..
				accbefore~[]..
				accafter~[math_id(3, DOBSOD_x), math_id(4, DOBSOD_expr)]
			]..
			rrefs~[]..
			accbefore~[]..
			accafter~[math_id(3, DOBSOD_x), math_id(4, DOBSOD_expr)]			 
		)
	]..
	rrefs~[]..
	accbefore~[]..
	accafter~[]
	.

p(lemma,PRS) :-
	!,
	phrase(fo_grammar:fo_formula(DOBSOD_expr,_),[x,'=',x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	PRS =
	id~proof_1..
	drefs~[]..
	mrefs~[]..
	conds~[
		theorem(lemma,
			id~goal_2..
			drefs~[]..
			mrefs~[]..
			conds~[
				id~1..
				drefs~[0, 1]..
				mrefs~[DOBSOD_expr, DOBSOD_x]..
				conds~[math_id(0, DOBSOD_expr), math_id(1, DOBSOD_x),holds(0)]..
				rrefs~[]..
				accbefore~[]..
				accafter~[math_id(0, DOBSOD_expr), math_id(1, DOBSOD_x)]
			]..
			rrefs~[]..
			accbefore~[]..
			accafter~[math_id(0, DOBSOD_expr), math_id(1, DOBSOD_x)]
			,
			id~body_2..
			drefs~[]..
			mrefs~[]..
			conds~[
				id~3..
				drefs~[]..
				mrefs~[DOBSOD_expr, DOBSOD_x]..
				conds~[math_id(0, DOBSOD_expr), holds(0)]..
				rrefs~[]..
				accbefore~[]..
				accafter~[math_id(0, DOBSOD_expr)]
			]..
			rrefs~[]..
			accbefore~[]..
			accafter~[] 
			)
	]..
	rrefs~[]..
	accbefore~[]..
	accafter~[]
	.

	
p(there_isno,PRS) :-
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,	
	phrase(fo_grammar:fo_formula(DOBSOD_expr,_),[x,'=',x]),!,
	PRS =id~proof_1
	..drefs~[]
	..mrefs~[]
	..conds~[
		neg(
			id~0
	               	..drefs~[]
		   	..mrefs~[]
			..conds~[
				id~prefix_thereexists162
				..drefs~[160]
				..mrefs~[DOBSOD_x]
				..conds~[math_id(160, DOBSOD_x)]
				..rrefs~[]
				..accbefore~[]
				..accafter~[math_id(160, DOBSOD_x)]
				=>
				id~matrix_163
				..drefs~[161]
				..mrefs~[DOBSOD_expr,DOBSOD_x]
				..conds~[math_id(161,DOBSOD_expr),holds(161)]
				..rrefs~[]
				..accbefore~[math_id(160, DOBSOD_x)]
				..accafter~[math_id(160, DOBSOD_x), math_id(161,DOBSOD_expr)]
			]
			..rrefs~[]
			..accbefore~[]
			..accafter~[]
		)
	]
	..rrefs~[]
	..accbefore~[]
	..accafter~[]
	.

p(contradiction_check,PRS) :-
	phrase(fo_grammar:fo_formula(DOBSOD_not,_),['\neg',x,'=',x]),!,
	phrase(fo_grammar:fo_formula(DOBSOD_eq,_),[x,'=',x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	PRS = id~proof_1..
	drefs~[]..
	mrefs~[]..
	conds~[
		id~0..
		drefs~[29, 30]..
		mrefs~[DOBSOD_not, DOBSOD_x]..
		conds~[math_id(29, DOBSOD_not), math_id(30, DOBSOD_x), holds(29)]..
		rrefs~[]..
		accbefore~[]..
		accafter~[math_id(29, DOBSOD_not), math_id(30, DOBSOD_x)]
		==>
		id~consec_31..
		drefs~[]..
		mrefs~[]..
		conds~[
			id~1..
			drefs~[34]..
			mrefs~[DOBSOD_eq, DOBSOD_x]..
			conds~[math_id(34, DOBSOD_eq),holds(34) ]..
			rrefs~[]..
			accbefore~[math_id(29, DOBSOD_not), math_id(30, DOBSOD_x)]..
			accafter~[math_id(29, DOBSOD_not), math_id(30, DOBSOD_x), math_id(34, DOBSOD_eq)]
			,
			id~2..
			drefs~[]..
			mrefs~[]..
			conds~[
				contradiction
			]..
			rrefs~[]..
			accbefore~[math_id(29, DOBSOD_not), math_id(30, DOBSOD_x), math_id(34, DOBSOD_eq)]..
			accafter~[math_id(29, DOBSOD_not), math_id(30, DOBSOD_x), math_id(34, DOBSOD_eq)]
		]..
		rrefs~[]..
		accbefore~[math_id(29, DOBSOD_not), math_id(30, DOBSOD_x)]..
		accafter~[math_id(29, DOBSOD_not), math_id(30, DOBSOD_x), math_id(34, DOBSOD_eq)]
	]..
	rrefs~[]..
	accbefore~[]..
	accafter~[].

% Predicate
p(predicate,PRS):-
% ord(x)
	!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),
	PRS = id~10..
	      drefs~[1]..
	      mrefs~[DOBSOD_x]..
	      conds~[math_id(1, DOBSOD_x), predicate(1,[ord])]..
	      rrefs~[]..
	      accbefore~[]..
	      accafter~[math_id(1, DOBSOD_x)].

p(predicate_two_args,PRS):-
% x greater than y
	!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),
	phrase(fo_grammar:fo_term(DOBSOD_y,_),[y]),
	PRS = id~0..
	      drefs~[1,2]..
	      mrefs~[DOBSOD_x, DOBSOD_y]..
	      conds~[math_id(1, DOBSOD_x), math_id(2, DOBSOD_y), predicate(1,2,['>'])]..
	      rrefs~[]..
	      accbefore~[]..
	      accafter~[math_id(1, DOBSOD_x), math_id(2, DOBSOD_y)].

	
% Induction
% For all x x=x
p(induction,PRS) :-
	!,
	phrase(fo_grammar:fo_formula(DOBSOD_expr,_),[ord,'(',x,',',x,')']),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	% Formula1 and FormulaSucc are the formulas for the induction
        Formula1 = type~relation..arity~2..name~ ord..args~[type~constant..name~'1', type~constant..name~'1'..arity~0],
        FormulaSucc = type~quantifier..name~!..args~[
	                        [type~variable..arity~0..name~x],
	                        type~logical_symbol..arity~2..name~ (=>)..args~[
									type~logical_symbol..name~'&'..args~[
									type~relation..name~naturalnumber..args~[type~variable..arity~0..name~x],
	                                type~relation..arity~2..name~ ord..args~[type~variable..arity~0..name~x, type~variable..arity~0..name~x]
									],
	                                type~relation..arity~2..name~ ord..args~[
	                                type~function..arity~1..name~succ..args~[type~variable..arity~0..name~x],
	                                type~function..arity~1..name~succ..args~[type~variable..arity~0..name~x]
	                                ]
	                        ]
		           ],
	% PRS = (1=1 & (x=x => succ(x)=succ(x))) => !x (x=x)
	PRS = 	id~0..
		drefs~[]..
		mrefs~[]..
		conds~[	
			id~1..
			drefs~[1,2]..
			mrefs~[Formula1,FormulaSucc]..
			conds~[
				math_id(1,Formula1),
				holds(1),
				math_id(2,FormulaSucc),
				holds(2)
			]..
			rrefs~[]..
			accbefore~[]..
			accafter~[
				math_id(1,Formula1),
				math_id(2,FormulaSucc)
			]
			==>
			id~2..
			drefs~[]..
			mrefs~[]..
			conds~[
				id~restrictor(2)..
				drefs~[3]..
				mrefs~[DOBSOD_x]..
				conds~[math_id(3, DOBSOD_x),predicate(3, [natural, number])]..
				rrefs~[]..
				accbefore~[]..
				accafter~[math_id(3, DOBSOD_x)]
				=>
				id~scope(2)..
				drefs~[4]..
				mrefs~[DOBSOD_expr]..
				conds~[ math_id(4, DOBSOD_expr), holds(4)]..
				rrefs~[]..
				accbefore~[math_id(3, DOBSOD_x)]..
				accafter~[math_id(3, DOBSOD_x), math_id(4, DOBSOD_expr)]
			]..
			rrefs~[induction]..
			accbefore~[]..
			accafter~[]
		]..
		rrefs~[]..
		accbefore~[]..
		accafter~[math_id(1,Formula1), math_id(2,FormulaSucc)].
	



% conjunct and comma_conjunct tests
p(conjunct,PRS) :-
	!,
	PRS = id~0..
	conds~[
		id~1..
		conds~[
			id~conjunct1(1)..
			conds~[
				math_id(1, args~[]..name~x..type~variable..arity~0..dref~1), 
				predicate(1, [even])
			]..
			drefs~[1]..
			mrefs~[	args~[]..name~x..type~variable..arity~0..dref~1	]..
			rrefs~[]..
			accbefore~[]..
			accafter~[math_id(1, args~[]..name~x..type~variable..arity~0..dref~1)]
			, 
			id~conjunct2(1)..
			conds~[
				predicate(1, [prime])
			]..
			drefs~[]..
			mrefs~[]..
			rrefs~[]..
			accbefore~[math_id(1, args~[]..name~x..type~variable..arity~0..dref~1)]..
			accafter~[math_id(1, args~[]..name~x..type~variable..arity~0..dref~1)]
		]..
		drefs~[]..
		mrefs~[]..
		rrefs~[]..
		accbefore~[]..
		accafter~[
			math_id(1, args~[]..name~x..type~variable..arity~0..dref~1)
		]
	]..
	drefs~[]..
	mrefs~[]..
	rrefs~[]..
	accbefore~[]..
	accafter~[math_id(1, args~[]..name~x..type~variable..arity~0..dref~1)].


p(comma_conjunct,PRS) :-
	!,
	PRS = id~0..
	conds~[
		id~1..conds~[
			id~comma_conjunct1(1)..
			conds~[
				math_id(1, args~[]..name~x..type~variable..arity~0..dref~1), 
				predicate(1, [even])
			]..
			drefs~[1]..
			mrefs~[args~[]..name~x..type~variable..arity~0..dref~1]..
			rrefs~[]..
			accbefore~[]..
			accafter~[math_id(1, args~[]..name~x..type~variable..arity~0..dref~1)]
			, 
			id~comma_conjunct2(1)..
			conds~[
				predicate(1, [prime])
			]..
			drefs~[]..
			mrefs~[]..
			rrefs~[]..
			accbefore~[math_id(1, args~[]..name~x..type~variable..arity~0..dref~1)]..
			accafter~[math_id(1, args~[]..name~x..type~variable..arity~0..dref~1)]
		]..
		drefs~[]..
		mrefs~[]..
		rrefs~[]..
		accbefore~[]..
		accafter~[math_id(1, args~[]..name~x..type~variable..arity~0..dref~1)]
	]..
	drefs~[]..
	mrefs~[]..
	rrefs~[]..
	accbefore~[]..
	accafter~[math_id(1, args~[]..name~x..type~variable..arity~0..dref~1)].



p('conjunct with no free var, check',PRS) :-
	!,
	PRS = 
	id~0..conds~[
		id~1..conds~[
			id~restrictor(1)..conds~[
				math_id(1, args~[]..name~x..type~variable..arity~0..dref~1)
			]..
			drefs~[1]..
			mrefs~[args~[]..name~x..type~variable..arity~0..dref~1]..
			rrefs~[]..
			accbefore~[]..
			accafter~[math_id(1, args~[]..name~x..type~variable..arity~0..dref~1)]
			=>
			id~scope(1)..conds~[
				id~conjunct1(scope(1))..conds~[
					math_id(2, args~[args~[]..name~x..type~variable..arity~0..dref~1,args~[]..name~x..type~variable..arity~0..dref~1]..name~ (=)..type~relation..arity~2), 
					holds(2)]..
				drefs~[2]..
				mrefs~[args~[args~[]..name~x..type~variable..arity~0..dref~1, args~[]..name~x..type~variable..arity~0..dref~1]..name~ (=)..type~relation..arity~2]..
				rrefs~[]..
				accbefore~[math_id(1, args~[]..name~x..type~variable..arity~0..dref~1)]..
				accafter~[math_id(1, args~[]..name~x..type~variable..arity~0..dref~1), math_id(2, args~[args~[]..name~x..type~variable..arity~0..dref~1, args~[]..name~x..type~variable..arity~0..dref~1]..name~ (=)..type~relation..arity~2)]
				, 
				id~conjunct2(scope(1))..conds~[
					holds(2)]..
				drefs~[]..
				mrefs~[]..
				rrefs~[]..
				accbefore~[math_id(1, args~[]..name~x..type~variable..arity~0..dref~1), math_id(2, args~[args~[]..name~x..type~variable..arity~0..dref~1, args~[]..name~x..type~variable..arity~0..dref~1]..name~ (=)..type~relation..arity~2)]..
				accafter~[math_id(1, args~[]..name~x..type~variable..arity~0..dref~1), math_id(2, args~[args~[]..name~x..type~variable..arity~0..dref~1, args~[]..name~x..type~variable..arity~0..dref~1]..name~ (=)..type~relation..arity~2)]
			]..
			drefs~[]..
			mrefs~[]..
			rrefs~[]..
			accbefore~[math_id(1, args~[]..name~x..type~variable..arity~0..dref~1)]..
			accafter~[math_id(1, args~[]..name~x..type~variable..arity~0..dref~1), math_id(2, args~[args~[]..name~x..type~variable..arity~0..dref~1, args~[]..name~x..type~variable..arity~0..dref~1]..name~ (=)..type~relation..arity~2)]]..
		drefs~[]..
		mrefs~[]..
		rrefs~[]..
		accbefore~[]..
		accafter~[]]..
	drefs~[]..
	mrefs~[]..
	rrefs~[]..
	accbefore~[]..
	accafter~[].










% --------------------- Wrong test PRSs for error messages -------------------
% Missing math_id PRS
p(missing_id,PRS) :- 
	!,
	phrase(fo_grammar:fo_formula(DOBSOD_expr,_),[x,'=',x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_term,_),[x]),!,
	PRS = id~3..
	      drefs~[5, 6]..
              mrefs~[DOBSOD_expr, DOBSOD_term]..
              conds~[math_id(6, DOBSOD_term), math_id(5, DOBSOD_expr), holds(5)]..
	      rrefs~[]..
	      accbefore~[]..
	      accafter~[math_id(6, DOBSOD_term)].

p(unknown_condition,PRS) :-
	!,
	PRS = id~1..
	      drefs~[]..
	      mrefs~[]..
	      conds~[hrsga]..
	      rrefs~[]..
	      accbefore~[]..
	      accafter~[].

p(no_formula,PRS) :-
	!,
	PRS = id~1..
	      drefs~[]..
	      mrefs~[]..
	      conds~[holds(1)]..
	      rrefs~[]..
	      accbefore~[]..
	      accafter~[math_id(1, type~variable)].

% Empty definiendum PRS
p(definiendum_empty,PRS) :-
	!,
	PRS = id~5..
	      drefs~[]..
              mrefs~[]..
              conds~[
		id~3..
	      	drefs~[]..
              	mrefs~[]..
              	conds~[]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[]
		:=
		id~4..
	      	drefs~[]..
             	mrefs~[]..
              	conds~[]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[]
              ]..
	      rrefs~[]..
	      accbefore~[]..
	      accafter~[].


% Empty definiens PRS
p(definiens_empty,PRS) :-
	!,
	phrase(fo_grammar:fo_formula(DOBSOD_x_y,_),[x,'=',y]),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_y,_),[y]),!,
	PRS = id~5..
	      drefs~[]..
              mrefs~[]..
              conds~[
		id~3..
	      	drefs~[5, 6, 7]..
              	mrefs~[DOBSOD_x_y, DOBSOD_x, DOBSOD_y]..
              	conds~[math_id(5, DOBSOD_x_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y),holds(5)]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[math_id(5, DOBSOD_x_y), math_id(6, DOBSOD_x), math_id(7, DOBSOD_y)]
		:=
		id~4..
	      	drefs~[]..
             	mrefs~[]..
              	conds~[]..
	      	rrefs~[]..
	      	accbefore~[]..
	      	accafter~[]
              ]..
	      rrefs~[]..
	      accbefore~[]..
	      accafter~[].

% Empty Assumption PRS
p(empty_assumption,PRS) :-
	!,
	PRS =
	id~proof_1..
	drefs~[]..
	mrefs~[]..
	conds~[
		id~1..
		drefs~[]..
		mrefs~[]..
		conds~[]..
		rrefs~[]..
	      	accbefore~[]..
	     	accafter~[]
		==>
		id~consec_76..
		drefs~[]..
		mrefs~[]..
		conds~[
			id~2..
			drefs~[]..
			mrefs~[]..
			conds~[]..
			rrefs~[]..
	      		accbefore~[]..
	     		accafter~[]
		]..
		rrefs~[]..
	      	accbefore~[]..
	      	accafter~[]
	]..
	rrefs~[]..
	accbefore~[]..
	accafter~[].


% Reverse Implication: Antecedent PRS is empty
p(reverse_implies_antecedent_empty,PRS) :-
	!,
	phrase(fo_grammar:fo_formula(DOBSOD_x_y,_),[x,'=',y]),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_y,_),[y]),!,

	PRS =
	id~proof_1..
	drefs~[]..
	mrefs~[]..
	conds~[
		id~0..
		drefs~[]..
		mrefs~[]..
		conds~[
			id~succedent_10..
			drefs~[6, 7, 8]..
			mrefs~[DOBSOD_x_y, DOBSOD_x, DOBSOD_y]..
			conds~[math_id(6, DOBSOD_x_y), math_id(7, DOBSOD_x), math_id(8, DOBSOD_y),holds(6) ]..
			rrefs~[]..
			accbefore~[]..
			accafter~[math_id(6, DOBSOD_x_y), math_id(7, DOBSOD_x), math_id(8, DOBSOD_y)]
			<=
			id~antecedent_11..
			drefs~[9]..
			mrefs~[DOBSOD_y, DOBSOD_x]..
			conds~[]..
			rrefs~[]..
			accbefore~[math_id(6, DOBSOD_x_y)]..
			accafter~[math_id(6, DOBSOD_x_y), math_id(7, DOBSOD_x), math_id(8, DOBSOD_y)]
			]..
		rrefs~[]..
		accbefore~[]..
		accafter~[]
	]..
	rrefs~[]..
	accbefore~[]..
	accafter~[].

% Reverse Implication: Succedent PRS is empty
p(reverse_implies_succedent_empty,PRS) :-
	!,
	phrase(fo_grammar:fo_formula(DOBSOD_y_x,_),[y,'=',x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_y,_),[y]),!,
	PRS =
	id~proof_1..
	drefs~[]..
	mrefs~[]..
	conds~[
		id~0..
		drefs~[]..
		mrefs~[]..
		conds~[
			id~succedent_10..
			drefs~[7, 8]..
			mrefs~[DOBSOD_x, DOBSOD_y]..
			conds~[math_id(7, DOBSOD_x), math_id(8, DOBSOD_y)]..
			rrefs~[]..
			accbefore~[]..
			accafter~[math_id(7, DOBSOD_x), math_id(8, DOBSOD_y)]
			<=
			id~antecedent_11..
			drefs~[9]..
			mrefs~[DOBSOD_y_x]..
			conds~[math_id(9, DOBSOD_y_x), holds(9)]..
			rrefs~[]..
			accbefore~[math_id(7, DOBSOD_x), math_id(8, DOBSOD_y)]..
			accafter~[math_id(7, DOBSOD_x), math_id(8, DOBSOD_y), math_id(9, DOBSOD_y_x)]
			]..
		rrefs~[]..
		accbefore~[]..
		accafter~[]
	]..
	rrefs~[]..
	accbefore~[]..
	accafter~[].



% Equivalence: B is empty
% !x x=x <=> ?y _
p(equivalence_empty,PRS) :-
	!,
	phrase(fo_grammar:fo_formula(DOBSOD_x_x,_),[x,'=',x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	phrase(fo_grammar:fo_term(DOBSOD_y,_),[y]),!,

	PRS =
	id~proof_1..
	drefs~[]..
	mrefs~[]..
	conds~[
		id~0..
		drefs~[]..
		mrefs~[]..
		conds~[
			id~succedent_10..
			drefs~[6, 7]..
			mrefs~[DOBSOD_x_x, DOBSOD_x]..
			conds~[math_id(6, DOBSOD_x_x), math_id(7, DOBSOD_x),holds(6) ]..
			rrefs~[]..
			accbefore~[]..
			accafter~[math_id(6, DOBSOD_x_x), math_id(7, DOBSOD_x), math_id(8, DOBSOD_y)]
			<=>
			id~antecedent_11..
			drefs~[8]..
			mrefs~[DOBSOD_y]..
			conds~[math_id(8, DOBSOD_y)]..
			rrefs~[]..
			accbefore~[math_id(6, DOBSOD_x_x), math_id(7, DOBSOD_x), math_id(8, DOBSOD_y)]..
			accafter~[math_id(6, DOBSOD_x_x), math_id(7, DOBSOD_x), math_id(8, DOBSOD_y)]
			]..
		rrefs~[]..
		accbefore~[]..
		accafter~[]
	]..
	rrefs~[]..
	accbefore~[]..
	accafter~[].



% -------------------------------- DOBSODS ---------------------------------

dobsod('x=x',X) :-
	X = type~relation..name~ (=)..args~[
		type~variable..name~x..arity~0, 
		type~variable..name~x..arity~0]..
	arity~2.

dobsod('! x,y x=y <=> x=x',X) :-
	X = type~quantifier..name~!..arity~2..args~[
		[type~variable..name~x..arity~0, 
		type~variable..name~y..arity~0], 
		type~logical_symbol..name~ <=> .. arity~2..args~[
			type~relation..name~ (=)..arity~2..args~[
				type~variable..name~x..arity~0, 
				type~variable..name~y..arity~0], 
			type~relation..name~ (=)..arity~2..args~[
				type~variable..name~x..arity~0, 
				type~variable..name~x..arity~0]
			]
		].

dobsod('for all x,y ord(x,y) => ord(x,y)',X) :-
	X = type~quantifier..name~!..arity~2..args~[
			[type~variable..name~x..arity~0, 
			 type~variable..name~y..arity~0], 
		type~logical_symbol..name~ (=>)..arity~2..args~[
			type~relation..name~ord..arity~2..args~[
				type~variable..name~x..arity~0, 
				type~variable..name~y..arity~0], 
			type~relation..name~ord..arity~2..args~[
				type~variable..name~x..arity~0, 
				type~variable..name~y..arity~0
			]
		]
	].

dobsod('not ord(x,y)',X) :-
	X = type~logical_symbol..arity~1..name~ ~ .. args~[
		type~quantifier..name~ ? .. args~[
			[type~variable..arity~0..name~x, 
			type~variable..arity~0..name~y], 
			type~relation..arity~2..name~ord..args~[
				type~variable..arity~0..name~x, 
				type~variable..arity~0..name~y]
			]
		].


dobsod('for all x,y ord(x,y) => ord(y,x)',X) :-
	X = type~quantifier..arity~2..name~!..args~[
		[type~variable..arity~0..name~x, type~variable..arity~0..name~y], 
		type~logical_symbol..arity~2..name~ (=>)..args~[
			type~relation..arity~2..name~ord..args~[
				type~variable..arity~0..name~x, 
				type~variable..arity~0..name~y]
		, 
		type~relation..arity~2..name~ord..args~[
			type~variable..arity~0..name~y, 
			type~variable..arity~0..name~x
			]
		]
	].


dobsod('\u2200x x=x',X) :-
	X = type~quantifier..name~!..arity~2..args~[
		[type~variable..name~x..arity~0]
		, 
		type~relation..name~ (=)..arity~2..args~[
			type~variable..name~x..arity~0, 
			type~variable..name~x..arity~0
		]
	].

dobsod('?x x=x',X) :-
	X = type~quantifier..name~'?'..arity~2..args~[
		[type~variable..name~x..arity~0]
		, 
		type~relation..name~ (=)..arity~2..args~[
			type~variable..name~x..arity~0, 
			type~variable..name~x..arity~0
		]
	].

dobsod('?x ord(x)',X) :-
	X = type~quantifier..name~'?'..arity~2..args~[
		[type~variable..name~x..arity~0]
		, 
		type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~x ..arity~0]
	].


dobsod('?x,y greater(x,y)',X) :-
	X = type~quantifier..name~'?'..arity~2..args~[
		[type~variable..name~x..arity~0,
		type~variable..name~y..arity~0]
		, 
		type~relation..arity~2..name~greater..args~[type~variable..arity~0..name~x, type~variable..arity~0..name~y]
	].

dobsod('not x=y',X) :-
	X = type~logical_symbol..name~ '~' .. arity~1..args~[
		type~relation..name~ '='..arity~2..args~[
			type~variable..name~x..arity~0, 
			type~variable..name~y..arity~0
		]
	].

dobsod('not ord(x,x)',X) :-
	X = type~logical_symbol..name~ '~' .. arity~1..args~[
		type~relation..name~ord..arity~2..args~[
			type~variable..name~x..arity~0, 
			type~variable..name~x..arity~0
		]
	].

dobsod('for all x : not x=x',X) :-
	X = type~quantifier..name~!..arity~2..args~[
		[type~variable..name~x..arity~0], 
		type~logical_symbol..name~ '~' .. arity~1..args~[
			type~relation..name~(=)..arity~2..args~[
				type~variable..name~x..arity~0, 
				type~variable..name~x..arity~0
			]
		]
	].

dobsod('there is no x such that x=x',X) :-
     X = type~logical_symbol ..name ~'~' ..arity~1 ..args~ 
	  [type~quantifier ..name~'?' ..arity~2 ..args~
	   [[type~variable ..name~x ..arity~0], 
	    type~relation ..name~'=' ..arity~2 ..args~
		 [type~variable ..name~x ..arity~0, 
		  type~variable ..name~x ..arity~0
		 ]
		]
	   ].
	
dobsod('~ ~ x=x',X) :-
	X = type~logical_symbol..name~ ~ .. arity~1..args~[
		type~logical_symbol..name~ ~ .. arity~1..args~[
			type~relation..name~ (=)..arity~2..args~[
				type~variable..name~x..arity~0, 
				type~variable..name~x..arity~0
			]
		]
	].

dobsod('!x ~ ~ x=x',X) :-
	X = type~quantifier..name~!..args~[
		[type~variable..arity~0..name~x], 
		type~logical_symbol..arity~1..name~ ~ .. args~[
			type~logical_symbol..arity~1..name~ ~ .. args~[
			type~relation..arity~2..name~ (=)..args~[
				type~variable..arity~0..name~x, 
				type~variable..arity~0..name~x
				]	
			]
		]
	    ].

dobsod('ord(x)',X) :-
	X = type~relation ..name~'ord' ..arity~1 ..args~[type~variable ..name~x ..arity~0].

dobsod('greater(x,y)', X) :-
	X = type~relation..arity~2..name~greater..args~[type~variable..arity~0..name~x, type~variable..arity~0..name~y].

dobsod('f(x)=x*x', X) :-
	X = type~relation ..name~'=' ..arity~2 ..args~[
		type~function..name~'f(x)',
		type~function ..arity~2 ..name~'mul' ..args~[type~variable ..name~x ..arity~0, type~variable ..name~x ..arity~0]].

dobsod('x less than y',X) :-
	X = type~relation..name~less..arity~2..args~[
		type~variable..name~x..arity~0,
		type~variable..name~y..arity~0].

dobsod('y less than x',X) :-
	X = type~relation..name~less..arity~2..args~[
		type~variable..name~y..arity~0,
		type~variable..name~x..arity~0].

dobsod_list('not ord(x,x) xor ord(x,x)',X):-
	X = 	[
		type~logical_symbol..arity~2..args~[
			type~quantifier..args~[
				[type~variable..arity~0..args~[]..name~x], 
				type~logical_symbol..arity~1..args~[
					type~relation..arity~2..args~[
						type~variable..arity~0..name~x, 
						type~variable..arity~0..name~x
					]..name~ ord
				]..name~ ~
			]..name~ ?, 
			type~quantifier..args~[
			[type~variable..arity~0..args~[]..name~x], 
				type~relation..arity~2..args~[
					type~variable..arity~0..args~[]..name~x, 
					type~variable..arity~0..args~[]..name~x
				]..name~ ord
			]..name~ ?
		]..name~ (|),
		type~logical_symbol..arity~2..args~[
			type~logical_symbol..arity~1..args~[
				type~quantifier..args~[
				[type~variable..arity~0..args~[]..name~x], 
				type~logical_symbol..arity~1..args~[
					type~relation..arity~2..args~[
						type~variable..arity~0..args~[]..name~x, 
						type~variable..arity~0..args~[]..name~x
					]..name~ ord
				]..name~ ~		
				]..name~ ?
			]..name~ ~,		
			type~logical_symbol..arity~1..args~[
				type~quantifier..args~[
				[type~variable..arity~0..args~[]..name~x], 
					type~relation..arity~2..args~[
						type~variable..arity~0..args~[]..name~x, 
						type~variable..arity~0..args~[]..name~x
					]..name~ ord
				]..name~ ?
			]..name~ ~
		]..name~ (|) 
].

dobsod_list('(?x not ord(x,x)) <> (?x ord(x,x))',X):-
	X = 	[type~logical_symbol..arity~2..args~[
			type~logical_symbol..arity~1..args~[
			type~quantifier..args~[
				[type~variable..arity~0..args~[]..name~x], 
				type~logical_symbol..arity~1..args~[
					type~relation..arity~2..args~[
						type~variable..arity~0..name~x, 
						type~variable..arity~0..name~x
					]..name~ ord
				]..name~ ~
			]..name~ ? 
			]..name~ ~,
			type~logical_symbol..arity~1..args~[
				type~quantifier..args~[
					[type~variable..arity~0..args~[]..name~x], 
					type~relation..arity~2..args~[
						type~variable..arity~0..args~[]..name~x, 
						type~variable..arity~0..args~[]..name~x
					]..name~ ord
				]..name~ ?
			]..name~ ~
		]..name~ (|)].

dobsod_list('x<y xor ord(x,y) xor y<x', X) :-
	X = 	[
		type~logical_symbol..arity~2..args~[
			type~logical_symbol..arity~2..args~[
				type~quantifier..args~[
					[type~variable..arity~0..args~[]..name~x, type~variable..arity~0..args~[]..name~y], 
					type~relation..arity~2..args~[
						type~variable..arity~0..name~x, 
						type~variable..arity~0..name~y
					]..name~less
				]..name~ ?, 
				type~quantifier..args~[
					[type~variable..arity~0..args~[]..name~x, type~variable..arity~0..args~[]..name~y], 
					type~relation..arity~2..args~[
						type~variable..arity~0..args~[]..name~x, 
						type~variable..arity~0..args~[]..name~y
					]..name~ ord
				]..name~ ?
			]..name~ (|), 
			type~quantifier..args~[
				[type~variable..arity~0..args~[]..name~x, type~variable..arity~0..args~[]..name~y], 
				type~relation..arity~2..args~[
					type~variable..arity~0..name~y, 
					type~variable..arity~0..name~x
				]..name~less
			]..name~ ?
		]..name~ (|),
		type~logical_symbol..arity~2..args~[
			type~logical_symbol..arity~1..args~[
			type~quantifier..args~[
				[type~variable..arity~0..args~[]..name~x, type~variable..arity~0..args~[]..name~y], 
				type~relation..arity~2..args~[
					type~variable..arity~0..args~[]..name~x, 
					type~variable..arity~0..args~[]..name~y
				]..name~ ord	
			]..name~ ? 
			]..name~ ~,
			type~logical_symbol..arity~1..args~[
				type~quantifier..args~[
					[type~variable..arity~0..args~[]..name~x, type~variable..arity~0..args~[]..name~y], 
					type~relation..arity~2..args~[
						type~variable..arity~0..name~y, 
						type~variable..arity~0..name~x
					]..name~less
				]..name~ ?
			]..name~ ~
		]..name~ (|), 
		type~logical_symbol..arity~2..args~[
			type~logical_symbol..arity~1..args~[
			type~quantifier..args~[
				[type~variable..arity~0..args~[]..name~x, type~variable..arity~0..args~[]..name~y], 
				type~relation..arity~2..args~[
					type~variable..arity~0..name~x, 
					type~variable..arity~0..name~y
				]..name~less
			]..name~ ?
			]..name~ ~,
			type~logical_symbol..arity~1..args~[
				type~quantifier..args~[
					[type~variable..arity~0..args~[]..name~x, type~variable..arity~0..args~[]..name~y], 
					type~relation..arity~2..args~[
						type~variable..arity~0..name~y, 
						type~variable..arity~0..name~x
					]..name~less
				]..name~ ?
			]..name~ ~
		]..name~ (|), 
		type~logical_symbol..arity~2..args~[
			type~logical_symbol..arity~1..args~[
			type~quantifier..args~[
				[type~variable..arity~0..args~[]..name~x, type~variable..arity~0..args~[]..name~y], 
				type~relation..arity~2..args~[
					type~variable..arity~0..name~x, 
					type~variable..arity~0..name~y
				]..name~less
			]..name~ ? 
			]..name~ ~,
			type~logical_symbol..arity~1..args~[
				type~quantifier..args~[
					[type~variable..arity~0..args~[]..name~x, type~variable..arity~0..args~[]..name~y], 
					type~relation..arity~2..args~[
						type~variable..arity~0..args~[]..name~x, 
						type~variable..arity~0..args~[]..name~y
					]..name~ ord
				]..name~ ?
			]..name~ ~
		]..name~ (|) 
].


dobsod_list('(?x,y x<y) <> (?x,y ord(x,y)) <> (?x,y y<x)', X) :-
	X = 	[
		type~logical_symbol..arity~2..args~[
			type~logical_symbol..arity~1..args~[
			type~quantifier..args~[
				[type~variable..arity~0..args~[]..name~x, type~variable..arity~0..args~[]..name~y], 
				type~relation..arity~2..args~[
					type~variable..arity~0..args~[]..name~x, 
					type~variable..arity~0..args~[]..name~y
				]..name~ ord
			]..name~ ? 
			]..name~ ~,
			type~logical_symbol..arity~1..args~[
				type~quantifier..args~[
					[type~variable..arity~0..args~[]..name~x,type~variable..arity~0..args~[]..name~y], 
					type~relation..arity~2..args~[
						type~variable..arity~0..name~y, 
						type~variable..arity~0..name~x
					]..name~less
				]..name~ ?
			]..name~ ~
		]..name~ (|), 
		type~logical_symbol..arity~2..args~[
			type~logical_symbol..arity~1..args~[
			type~quantifier..args~[
				[type~variable..arity~0..args~[]..name~x, type~variable..arity~0..args~[]..name~y], 
				type~relation..arity~2..args~[
					type~variable..arity~0..name~x, 
					type~variable..arity~0..name~y
				]..name~less
			]..name~ ? 
			]..name~ ~,
			type~logical_symbol..arity~1..args~[
				type~quantifier..args~[
					[type~variable..arity~0..args~[]..name~x, type~variable..arity~0..args~[]..name~y], 
					type~relation..arity~2..args~[
						type~variable..arity~0..name~y, 
						type~variable..arity~0..name~x
					]..name~less
				]..name~ ?
			]..name~ ~
		]..name~ (|),
		type~logical_symbol..arity~2..args~[
			type~logical_symbol..arity~1..args~[
			type~quantifier..args~[
				[type~variable..arity~0..args~[]..name~x, type~variable..arity~0..args~[]..name~y], 
				type~relation..arity~2..args~[
					type~variable..arity~0..name~x, 
					type~variable..arity~0..name~y
				]..name~less
			]..name~ ?
			]..name~ ~,
			type~logical_symbol..arity~1..args~[
				type~quantifier..args~[
					[type~variable..arity~0..args~[]..name~x, type~variable..arity~0..args~[]..name~y], 
					type~relation..arity~2..args~[
						type~variable..arity~0..args~[]..name~x, 
						type~variable..arity~0..args~[]..name~y
					]..name~ ord
				]..name~ ?
			]..name~ ~
		]..name~ (|) 
].




% ---------------------- create_obligations:check_prs ---------------------------------------
% ------------------------ TESTS -----------------------------------------


% Empty PRS is valid, both checked and unchecked
test('Empty PRS, checked') :- 
	p(empty,PRS),
	create_obligations:check_prs(PRS,[],[],GOut,[],[],[],[],check),
	discharge_obligations([],GOut,_).

test('Empty PRS, unchecked') :- 
	p(empty,PRS),
	create_obligations:check_prs(PRS,[],[],_,[],[],[],[],nocheck).



% holds(X)
test('holds PRS, unchecked') :-
	p(holds,PRS),
	!,
	DOBSOD = type~relation..id~qe(3)..name~'$true',
	create_obligations:check_prs(PRS,[],
		[],
		_Graph,
		[],
		_Ob,
		[],
		[DOBSOD],	
		nocheck).

test('holds PRS, checked') :-
	p(holds,PRS),
	!,
	create_obligations:check_prs(PRS,[],
		[],
		Graph,
		[],
		Ob,
		[],
		[DOBSOD],
		check),
		DOBSOD = type~relation..id~holds(3, 5, 0)..name~'='..arity~2..args~[type~variable..name~x..arity~0, type~variable..name~x..arity~0..args~[]],
	discharge_obligations(Ob,Graph,_).


% Disjunction
test('Disjunction PRS, nocheck') :-
	p(disjunction,PRS),
	!,
	create_obligations:check_prs(PRS,[],
		[],
		_Graph,
		[],
		_Ob,
		[],
		X,
		nocheck),
	X = [type~logical_symbol..arity~2..name~ (|)..args~[
		type~relation..arity~2..name~ (=)..args~[
			type~variable..arity~0..name~y, 
			type~variable..arity~0..name~y], 
		type~relation..arity~2..name~ (=)..args~[
			type~variable..arity~0..name~x, 
			type~variable..arity~0..name~x
		]
	     ]].


test('Disjunction PRS, check') :-
	clear_messages,
	p(disjunction,PRS),
	!,
	create_obligations:check_prs(PRS,[],
		[],
		Graph,
		[],
		Ob,
		[],
		X,
		check),
	X = [type~logical_symbol..arity~2..name~ (|)..args~[
		type~relation..arity~2..name~ (=)..args~[
			type~variable..arity~0..name~y, 
			type~variable..arity~0..name~y], 
		type~relation..arity~2..name~ (=)..args~[
			type~variable..arity~0..name~x, 
			type~variable..arity~0..name~x
		]
	     ]],
	discharge_obligations(Ob,Graph,_).

test('Disjunction PRS, A is empty, nocheck') :-
	p(disjunction_A_empty,PRS),
	!,
	create_obligations:check_prs(PRS,[],
		[],
		_Graph,
		[],
		_Ob,
		[],
		X,
		nocheck),
	X = [type~quantifier..id~qe(4)..name~ ? .. args~[
			[type~variable..name~x..arity~0..args~[]], 
			type~relation..id~holds(4, 8, 0)..name~ord..args~[
				type~variable..name~x..arity~0..args~[]]
			]
		].

test('Disjunction PRS, B is empty, nocheck') :-
	p(disjunction_B_empty,PRS),
	!,
	create_obligations:check_prs(PRS,[],
		[],
		_Graph,
		[],
		_Ob,
		[],
		X,
		nocheck),
	X = [type~quantifier..name~ ? .. args~[
		[type~variable..arity~0..name~x, 
		type~variable..arity~0..name~y], 
		type~relation..arity~2..name~ ord..args~[
			type~variable..arity~0..name~x, 
			type~variable..arity~0..name~y]]].


% Definition
test('Definition PRS, nocheck') :-
	p(definition,PRS),
	dobsod('! x,y x=y <=> x=x',DOBSOD),
	create_obligations:check_prs(PRS,[],
	[],
	_Graph,
	[],
	_Ob,
	[],
	[DOBSOD],
	nocheck).

test('Definition PRS, check') :-
	p(definition,PRS),
	dobsod('! x,y x=y <=> x=x',DOBSOD),
	create_obligations:check_prs(PRS,[],
	[],
	Graph,
	[],
	Ob,
	[],
	[DOBSOD],
	check),
	discharge_obligations(Ob,Graph,_).


% Function
test('Function PRS, unchecked') :- 
	premises:reset_counter,
	build_prs(PRS,[	
	sentence(1,['define',math([f,'(',x,')']),'to','be',math([x,*,x])])
	]),!,
	create_obligations:check_prs(PRS,[],[],_Graph,[],_Ob,[],X,check),
	X = [type~quantifier..arity~2..name~!..args~[
		[type~variable..arity~0..name~x], 
		type~relation..arity~2..name~ (=)..args~[
			type~function..name~f..args~[type~variable..name~x..dref~1..arity~0..args~[]],
			type~function..arity~2..name~mul..args~[
				type~variable..arity~0..name~x, 
				type~variable..arity~0..name~x]
			]
		]
	   ].



% Assumptions
test('Assumption PRS (with conditions in A and skolemized variables), check') :-
% Formula !x,y: x=y ==> x=skolem(1,x,y) & y=skolem(1,x,y) & r(x,skolem(2,x,y))
% (i.e. !x,y: x=y ==> ?z,w: x=z & y=z & r(w))
	premises:reset_counter,
	build_prs(PRS,[
	sentence(1,['let',math([x,=,y])]),
	sentence(2,['then',math([x,=,z])]),
	sentence(3,[math([y,=,z])]),
	sentence(4,[math([r,'(',w,')'])])
	]),!,
	create_obligations:check_prs(PRS,[],
	[],
	_Graph,
	[],
	_Ob,
	[],
	X,
	check),
	X = 	[
		type~quantifier..arity~2..name~!..args~[
			[type~variable..arity~0..name~x, type~variable..arity~0..name~y], 
			type~logical_symbol..arity~2..name~ (=>)..args~[
				type~relation..arity~2..name~ (=)..args~[
					type~variable..arity~0..name~x, 
					type~variable..arity~0..name~y], 
			type~relation..arity~2..name~r..args~[
					type~function..arity~2..name~skolem7..args~[type~variable..arity~0..name~x, type~variable..arity~0..name~y]
				]
			]
		],
		type~quantifier..arity~2..name~!..args~[
			[type~variable..arity~0..name~x, type~variable..arity~0..name~y], 
			type~logical_symbol..arity~2..name~ (=>)..args~[
				type~relation..arity~2..name~ (=)..args~[
					type~variable..arity~0..name~x, 
					type~variable..arity~0..name~y], 
				type~relation..arity~2..name~ (=)..args~[
					type~variable..arity~0..name~y, 
					type~function..arity~2..name~skolem4..args~[type~variable..arity~0..name~x, type~variable..arity~0..name~y]
				]
			]
		], 
		type~quantifier..arity~2..name~!..args~[
			[type~variable..arity~0..name~x, type~variable..arity~0..name~y], 
			type~logical_symbol..arity~2..name~ (=>)..args~[type~relation..arity~2..name~ (=)..args~[
				type~variable..arity~0..name~x, 
				type~variable..arity~0..name~y], 
			type~relation..arity~2..name~ (=)..args~[
				type~variable..arity~0..name~x, 
				type~function..arity~2..name~skolem4..args~[type~variable..arity~0..name~x, type~variable..arity~0..name~y]
			]
		]] 
		].
	

test('Assumption PRS (without conditions in A and skolemized variables), check') :-
% Formula !x,y: x=skolem(1,x,y) & y=skolem(1,x,y) & r(x,skolem(2,x,y))
% (i.e. !x,y: ?z,w: x=z & y=z & r(w))
	premises:reset_counter,
	build_prs(PRS,[
	sentence(1,['fix',math([x,',',y])]),
	sentence(2,[math([x,=,z])]),
	sentence(3,[math([y,=,z])]),
	sentence(4,[math([r,'(',w,')'])])
	]),!,
	create_obligations:check_prs(PRS,[],
		[],
		_Graph,
		[],
		_Ob,
		[],
		X,
		check),
	X = 	[
		type~quantifier..arity~2..name~!..args~[
			[type~variable..arity~0..name~x, type~variable..arity~0..name~y], 
			type~relation..arity~2..name~r..args~[
				type~function..arity~2..name~skolem7..args~[type~variable..arity~0..name~x, type~variable..arity~0..name~y]
			]
		],
		type~quantifier..arity~2..name~!..args~[
			[type~variable..arity~0..name~x, type~variable..arity~0..name~y], 
			type~relation..arity~2..name~ (=)..args~[
				type~variable..arity~0..name~y, 
				type~function..arity~2..name~skolem4..args~[type~variable..arity~0..name~x, type~variable..arity~0..name~y]
			]
		], 
		type~quantifier..arity~2..name~!..args~[
			[type~variable..arity~0..name~x, type~variable..arity~0..name~y], 
			type~relation..arity~2..name~ (=)..args~[
				type~variable..arity~0..name~x, 
				type~function..arity~2..name~skolem4..args~[type~variable..arity~0..name~x, type~variable..arity~0..name~y]
			]
		] 
		].


test('Assumption PRS, nocheck') :-
	dobsod('for all x,y ord(x,y) => ord(x,y)',DOBSOD),
	p(assumption,PRS),
	create_obligations:check_prs(PRS,[],
		[],
		_Graph,
		[],
		_Ob,
		[],
		[DOBSOD],
		nocheck).

test('Assumption PRS, check') :-
	p(assumption,PRS),
	dobsod('for all x,y ord(x,y) => ord(x,y)',DOBSOD),
	create_obligations:check_prs(PRS,[],
	[],
	Graph,
	[],
	Ob,
	[],
	[DOBSOD],
	check),
	discharge_obligations(Ob,Graph,_).

test('Assumption PRS with two conditions, check') :-
% !x,y,z: x=y & y=z => x=z
	utils:clean,
	p(assumption_two_conds,PRS),
	create_obligations:check_prs(PRS,[], [],Graph,[],Ob,[], X, check),
	X = [type~quantifier..arity~2..name~!..args~[
		[type~variable..arity~0..name~x, type~variable..arity~0..name~y, type~variable..arity~0..name~z], 
		type~logical_symbol..arity~2..name~ (=>)..args~[
			type~logical_symbol..arity~2..name~ & .. args~[
				type~relation..arity~2..name~ (=)..args~[
					type~variable..arity~0..name~y, 
					type~variable..arity~0..name~z], 
				type~relation..arity~2..name~ (=)..args~[
					type~variable..arity~0..name~x, 
					type~variable..arity~0..name~y]], 
			type~relation..arity~2..name~ (=)..args~[
				type~variable..arity~0..name~x, 
				type~variable..arity~0..name~z]]]],
	discharge_obligations(Ob,Graph,_).

test('Assumption PRS with empty antecedent PRS, check') :-
	clear_messages,
	p(assumption_a_empty,PRS),
	dobsod('\u2200x x=x',DOBSOD), 
	create_obligations:check_prs(PRS,[],
	[],
	_Graph,
	[],
	_Ob,
	[],
	[DOBSOD],
	check).

test('Assumption PRS with empty antecedent PRS, two conditions, nocheck') :-
% !x,y: x=x
% !x,y: y=y
	clear_messages,
	p(assumption_a_empty_two_conds,PRS),
	create_obligations:check_prs(PRS,[],
	[],
	_Graph,
	[],
	_Ob,
	[],
	X,
	nocheck),
	X = [
	id~qu(cond(0, 0), and(holds(scope(0), 3, 0), holds(scope(0), 2, 0)))..args~[
		[
		args~[]..dref~_G5446474..name~x..type~variable..arity~0, 
		args~[]..dref~_G5446576..name~y..type~variable..arity~0
		], 
		id~and(holds(scope(0), 3, 0), holds(scope(0), 2, 0))..args~[
			id~holds(scope(0), 3, 0)..args~[
				args~[]..dref~G5447494..name~y..type~variable..arity~0, 
				args~[]..dref~G5447494..name~y..type~variable..arity~0
			]..name~ (=)..type~relation..arity~2, 
			id~holds(scope(0), 2, 0)..args~[
				args~[]..dref~G5447170..name~x..type~variable..arity~0, 
				args~[]..dref~G5447170..name~x..type~variable..arity~0
			]..name~ (=)..type~relation..arity~2
		]..name~ & .. type~logical_symbol..arity~2
	]..name~!..type~quantifier..arity~2].
	

% Negation
% All tests with nocheck right now as we don't have good/short examples

test('Negation PRS, nocheck') :-
	dobsod('not ord(x,y)',DOBSOD),
	p(negation,PRS),
	create_obligations:check_prs(PRS,[],
	[],
	_Graph,
	[],
	_Ob,
	[],
	[DOBSOD],
	nocheck).	


% Implication

test('Implies PRS, nocheck') :-
% x=y => y=x
	p(implies,PRS),
	dobsod('for all x,y ord(x,y) => ord(y,x)',DOBSOD),
	create_obligations:check_prs(PRS,[],
	[],
	_Graph,
	[],
	_Ob,
	[],
	[DOBSOD],
	nocheck).

test('Implies PRS, check') :-
	p(implies,PRS),
	dobsod('for all x,y ord(x,y) => ord(y,x)',DOBSOD),
	create_obligations:check_prs(PRS,[],
	[],
	Graph,
	[],
	Ob,
	[],
	[DOBSOD],
	check),
	discharge_obligations(Ob,Graph,_).	


test('Implies PRS II, check') :-
% !x x=x => ?y x=y
	premises:reset_counter,
	p(implies2, PRS),
	create_obligations:check_prs(PRS,[],
		[],
		_Graph,
		[],
		_Ob,
		[],
		X,
		check),
	X = [type~quantifier..arity~2..name~'!'..args~[
		[type~variable..arity~0..name~x], 
		type~logical_symbol..arity~2..name~ (=>)..args~[
			type~relation..arity~2..name~ (=)..args~[
				type~variable..arity~0..name~x, 
				type~variable..arity~0..name~x], 
			type~relation..arity~2..name~ (=)..args~[
				type~variable..arity~0..name~x, 
				args~[
					args~[]..name~x..type~variable..arity~0
				]..name~skolem3..type~function..arity~1]
			]			
		]
	   ].
		

test('Implies PRS II discharge, check') :-
% !x x=x => ?y x=y
	utils:clean,
	premises:reset_counter,
	p(implies2, PRS),
	create_obligations:check_prs(PRS,[],
		[],
		Graph,
		[],
		Ob,
		[],
		X,
		check),
	X = [type~quantifier..arity~2..name~'!'..args~[
		[type~variable..arity~0..name~x], 
		type~logical_symbol..arity~2..name~ (=>)..args~[
			type~relation..arity~2..name~ (=)..args~[
				type~variable..arity~0..name~x, 
				type~variable..arity~0..name~x], 
			type~relation..arity~2..name~ (=)..args~[
				type~variable..arity~0..name~x, 
				args~[
					args~[]..name~x..type~variable..arity~0
				]..name~skolem3..type~function..arity~1]
			]			
		]
	   ],
	discharge_obligations(Ob,Graph,_).


test('Implication PRS with two conditions, check') :-
% !x,y,z: x=y & y=z => x=z
	utils:clean,
	premises:reset_counter,
	p(implication_two_conds,PRS),
	create_obligations:check_prs(PRS,[],[],Graph,[],Ob, [], X, check),
	X = [type~quantifier..arity~2..name~!..args~[
		[type~variable..arity~0..name~x, type~variable..arity~0..name~y, type~variable..arity~0..name~z], 
		type~logical_symbol..arity~2..name~ (=>)..args~[
			type~logical_symbol..arity~2..name~ & .. args~[
				type~relation..arity~2..name~ (=)..args~[
					type~variable..arity~0..name~y, 
					type~variable..arity~0..name~z], 
				type~relation..arity~2..name~ (=)..args~[
					type~variable..arity~0..name~x, 
					type~variable..arity~0..name~y]], 
			type~relation..arity~2..name~ (=)..args~[
				type~variable..arity~0..name~x, 
				type~variable..arity~0..name~z]]]],
	discharge_obligations(Ob,Graph,_).



%Reverse Implication
% x=y <= y=x
test('Reverse Implication PRS, nocheck') :-
	p(reverse_implies,PRS),
	create_obligations:check_prs(PRS,[],
	[],
	_Graph,
	[],
	_Ob,
	[],
	X,
	nocheck),
	X = [
		type~logical_symbol..arity~2..name~ ('=>')..args~[
			type~relation..arity~2..name~ (=)..args~[
				type~variable..arity~0..name~y, 
				type~variable..arity~0..name~x], 
			type~relation..arity~2..name~ (=)..args~[
				type~variable..arity~0..name~x, 
				type~variable..arity~0..name~y]
			]
	    ].

test('Reverse Implication PRS, check') :-
	utils:clean,
	p(reverse_implies,PRS),
	create_obligations:check_prs(PRS,[],
	[],
	Graph,
	[],
	Ob,
	[],
	X,
	check),
	X = [
		type~logical_symbol..arity~2..name~ (=>)..args~[
			type~relation..arity~2..name~ (=)..args~[
				type~variable..arity~0..name~y, 
				type~variable..arity~0..name~x], 
			type~relation..arity~2..name~ (=)..args~[
				type~variable..arity~0..name~x, 
				type~variable..arity~0..name~y]
			]
	    ],
	discharge_obligations(Ob,Graph,_).


test('Reverse Implication PRS with Mrefs in B, nocheck') :-
	build_prs(PRS,[
	sentence(1,[math([a]),'is','a','point','if',math([a]),'is','a','point'])
	]),!,
	create_obligations:check_prs(PRS,[],
	[],
	_Graph,
	[],
	_Ob,
	[],
	X,
	nocheck),
	X = [type~quantifier..id~qu(cond(1, 0), imp(conditional(1)))..name~!..dref~_G229099..arity~2..args~[[type~variable..name~a..dref~1..arity~0..args~[]..alph~latin..cap~small], type~logical_symbol..id~imp(conditional(1))..name~ (=>)..dref~_G229167..arity~2..args~[type~relation..id~replace(pred(prot(1), 0))..name~point..dref~_G229202..arity~1..args~[type~variable..name~_G229237..dref~1..args~[]], type~relation..id~replace(pred(apod(1), 0))..name~point..dref~_G229274..arity~1..args~[type~variable..name~_G229309..dref~1..args~[]]]]] .


% For all
test('For all PRS, check') :-
	clear_messages,
	p(for_all,PRS),
	dobsod('\u2200x x=x',DOBSOD), 
	create_obligations:check_prs(PRS,[],
	[],
	Graph,
	[],
	Ob,
	[],
	[DOBSOD],
	nocheck),
	discharge_obligations(Ob,Graph,_).
		

% Equivalence
% !x,y (x=y <=> (y=x))
test('Equivalence, unchecked') :-
	build_prs(PRS,[
	sentence(1,[math([x]),'is','a','point','iff',math([y]),'is','a','point'])
	]),!,
	create_obligations:check_prs(PRS,[],[],_Graph,[],_Ob,[],POut,nocheck),
	POut = [type~logical_symbol..id~eq(cond(1, 0))..name~ (<=>)..dref~_G169893..arity~2..args~[type~quantifier..id~qe(left_cond(1))..name~ ? .. dref~_G169928..args~[[type~variable..name~_G169963..dref~2..args~[]], type~relation..id~replace(pred(left_cond(1), 1))..name~point..dref~_G170000..arity~1..args~[type~variable..name~_G170035..dref~2..args~[]]], type~quantifier..id~qe(right_cond(1))..name~ ? .. dref~_G170072..args~[[type~variable..name~_G170107..dref~4..args~[]], type~relation..id~replace(pred(right_cond(1), 1))..name~point..dref~_G170144..arity~1..args~[type~variable..name~_G170179..dref~4..args~[]]]]] .


test('Equivalence, checked') :-
	utils:clean,
	build_prs(PRS,[
	sentence(1,[math([x]),'is','a','point','iff',math([y]),'is','a','point'])
	]),!,
	create_obligations:check_prs(PRS,[],[],Graph,[],Ob,[],_POut,check),
	discharge_obligations(Ob,Graph,_).


test('Equivalence II, checked') :-
% ! x (x=x <=> ?y x=y)
	utils:clean,
	clear_messages,
	build_prs(PRS,[
	sentence(1,[math([n]),'divides','a','prime','number','iff',math([n,=,'1']),'or',math([n]),'is','prime'])
	]),!,
	create_obligations:check_prs(PRS,[],[],_Graph,[],Ob,[],POut,check),
	Ob = [[left_cond(1), [id~eq(cond(conditional(1), 0))..args~[id~qe(left_cond(1))..args~[[args~[]..dref~2..type~variable], id~and(pred(left_cond(1), 2), and(pred(left_cond(1), 1), pred(left_cond(1), 0)))..args~[id~pred(left_cond(1), 2)..args~[args~[]..dref~1..name~n..type~variable..arity~0, args~[]..dref~2..type~variable]..name~divide..type~relation..arity~2, id~and(pred(left_cond(1), 1), pred(left_cond(1), 0))..args~[id~pred(left_cond(1), 1)..args~[args~[]..dref~2..type~variable]..name~number..type~relation..arity~1, id~pred(left_cond(1), 0)..args~[args~[]..dref~2..type~variable]..name~prime..type~relation..arity~1]..name~ & .. type~logical_symbol..arity~2]..name~ & .. type~logical_symbol..arity~2]..dref~_G44488..name~ ? .. type~quantifier, id~dis(right_cond(1))..args~[id~holds(disjunct1(right_cond(1)), 3, 0)..args~[args~[]..dref~1..name~n..type~variable..arity~0, args~[]..name~'1'..type~constant..arity~0]..name~ (=)..type~relation..arity~2, id~pred(disjunct2(right_cond(1)), 0)..args~[args~[]..dref~1..name~n..type~variable..arity~0]..name~prime..type~relation..arity~1]..name~ (|)..type~logical_symbol..arity~2]..name~ (<=>)..type~logical_symbol..arity~2], []]],
	POut = [id~ass(cond(1, 0), 0)..args~[[args~[]..dref~1..name~n..type~variable..arity~0], id~eq(cond(conditional(1), 0))..args~[id~qe(left_cond(1))..args~[[args~[]..dref~2..type~variable], id~and(pred(left_cond(1), 2), and(pred(left_cond(1), 1), pred(left_cond(1), 0)))..args~[id~pred(left_cond(1), 2)..args~[args~[]..dref~1..name~n..type~variable..arity~0, args~[]..dref~2..type~variable]..name~divide..type~relation..arity~2, id~and(pred(left_cond(1), 1), pred(left_cond(1), 0))..args~[id~pred(left_cond(1), 1)..args~[args~[]..dref~2..type~variable]..name~number..type~relation..arity~1, id~pred(left_cond(1), 0)..args~[args~[]..dref~2..type~variable]..name~prime..type~relation..arity~1]..name~ & .. type~logical_symbol..arity~2]..name~ & .. type~logical_symbol..arity~2]..name~ ? .. type~quantifier, id~dis(right_cond(1))..args~[id~holds(disjunct1(right_cond(1)), 3, 0)..args~[args~[]..dref~1..name~n..type~variable..arity~0, args~[]..name~'1'..type~constant..arity~0]..name~ (=)..type~relation..arity~2, id~pred(disjunct2(right_cond(1)), 0)..args~[args~[]..dref~1..name~n..type~variable..arity~0]..name~prime..type~relation..arity~1]..name~ (|)..type~logical_symbol..arity~2]..name~ (<=>)..type~logical_symbol..arity~2]..name~!..type~quantifier..arity~2].

% Predicate
test('Predicate, unchecked'):-
% ord(x)
	p(predicate,PRS),
	!,
	dobsod('?x ord(x)', DOBSOD),
	create_obligations:check_prs(PRS,[],[],_Graph,[],_Ob,[],[DOBSOD],nocheck).

test('Predicate with two arguments, unchecked'):-
% x > y
	p(predicate_two_args,PRS),
	!,
	dobsod('?x,y greater(x,y)',DOBSOD),
	create_obligations:check_prs(PRS,[], [],_Graph,[],_Ob,[], [DOBSOD], nocheck).

% Theorem
test('Theorem PRS, nocheck') :-
	dobsod('x=x',DOBSOD),
	p(theorem,PRS),
	create_obligations:check_prs(PRS,[],
	[],
	_Graph,
	[],
	_Ob,
	[],
	[DOBSOD],
	nocheck).

test('Theorem PRS, check') :-
	p(theorem,PRS),
	dobsod('x=x',DOBSOD),
	create_obligations:check_prs(PRS,[],
	[],
	Graph,
	[],
	Ob,
	[],
	[DOBSOD],
	check),
	discharge_obligations(Ob,Graph,_).

% Lemma
test('Lemma PRS, nocheck') :-
	p(lemma,PRS),
	dobsod('x=x',DOBSOD),
	create_obligations:check_prs(PRS,[],
	[],
	_Graph,
	[],
	_Ob,
	[],
	[DOBSOD],
	nocheck).

test('Lemma PRS, check') :-
	p(lemma,PRS),
	dobsod('x=x',DOBSOD),
	create_obligations:check_prs(PRS,[],
	[],
	Graph,
	[],
	Ob,
	[],
	[DOBSOD],
	check),
	discharge_obligations(Ob,Graph,_).

test('Contradiction, check') :-
	p(contradiction,PRS),
	create_obligations:check_prs(PRS,[],
	[],
	_Graph,
	[],
	_Ob,
	[],
	[type~relation..arity~0..name~'$false'],
	check).

test('Contradicting assumption, check') :-
	p(assumption_contradiction,PRS),
	create_obligations:check_prs(		
		PRS,[],
		[],
		_Graph,
		[],
		_Ob,
		[],
		X,
		check),
	X = [type~quantifier..name~!..args~[
		[type~variable..arity~0..name~x, type~variable..arity~0..name~y], 
		type~logical_symbol..arity~1..name~ ~ .. args~[
			type~relation..arity~2..name~ (=)..args~[
				type~variable..arity~0..name~x, 
				type~variable..arity~0..name~y]
				]
			]
		].


	
/*
test('there is no x such that x=x') :-
	p(there_isno,PRS),
	dobsod('there is no x such that x=x',X),
	create_obligations:check_prs(PRS,
	[],
	[X],
	nocheck).
*/	

test('Contradiction 2, check') :-
	p(contradiction_check ,PRS),
	dobsod('!x ~ ~ x=x',X),
	create_obligations:check_prs(PRS,[],
	[],
	_Graph,
	[],
	_Ob,
	[],
	[X],
	check).

test('Contradiction 2, check') :-
	p(contradiction_check ,PRS),
	dobsod('!x ~ ~ x=x',X),
	create_obligations:check_prs(
		PRS,[],
		[],
		Graph,
		[],
		Ob,
		[],
		[X],
		check),
	discharge_obligations(Ob,Graph,_).

% Exclusive Disjunction

test('Exclusive or with two formulas, unchecked') :-
% x=x >< not x=x (das ist hier die Frage)
	dobsod_list('not ord(x,x) xor ord(x,x)',X),
	p(exclusive_or_two,PRS),
	create_obligations:check_prs(PRS,[],[],_Graph,[],_Ob,[],X,nocheck).

test('Exclusive or with two formulas, checked') :-
% x=x >< not x=x 
	utils:clean,
	dobsod_list('not ord(x,x) xor ord(x,x)',X),
	p(exclusive_or_two,PRS),
	create_obligations:check_prs(PRS,[],[],Graph,[],Ob,[],X,check),
	discharge_obligations(Ob,Graph,_).


test('Exclusive or with three formulas, unchecked') :-
% x<y >< x=y >< y<x
	dobsod_list('x<y xor ord(x,y) xor y<x', X),
	p(exclusive_or_three,PRS),
	create_obligations:check_prs(PRS,[],[],_Graph,[],_Ob,[],X,nocheck).

% Case Exclusivity

test('Case Exclusivity for two formulas, unchecked') :-
% x=x <> not x=x (Oberchecker)
	dobsod_list('(?x not ord(x,x)) <> (?x ord(x,x))',X),
	p(exclusivity_of_two_cases,PRS),
	create_obligations:check_prs(PRS,[],[],_Graph,[],_Ob,[],X,nocheck).

test('Case Exclusivity for two formulas, checked') :-
% x=x <> not x=x 
	utils:clean,
	dobsod_list('(?x not ord(x,x)) <> (?x ord(x,x))',X),
	p(exclusivity_of_two_cases,PRS),
	create_obligations:check_prs(PRS,[],[],Graph,[],Ob,[],X,check),
	discharge_obligations(Ob,Graph,_).


test('Case Exclusivity for three formulas, unchecked') :-
% x<y <> x=y <> y<x
	dobsod_list('(?x,y x<y) <> (?x,y ord(x,y)) <> (?x,y y<x)', X),
	p(exclusivity_of_three_cases,PRS),
	create_obligations:check_prs(PRS,[],[],_Graph,[],_Ob,[],X,nocheck).



% Induction
test('Induction PRS, unchecked') :- 
% !x ord(x,x)
	p(induction,PRS),
	create_obligations:check_prs(PRS,[],[],_Graph,[],_Ob,[],X,nocheck),
	X = [type~logical_symbol..id~imp(cond(0, 0))..name~ (=>)..arity~2..args~[type~logical_symbol..id~and(holds(1, 2, 0), holds(1, 1, 0))..name~ & .. arity~2..args~[type~quantifier..id~holds(1, 2, 0)..name~!..args~[[type~variable..name~x..arity~0], type~logical_symbol..name~ (=>)..arity~2..args~[type~logical_symbol..name~ & .. args~[type~relation..name~naturalnumber..args~[type~variable..name~x..arity~0], type~relation..name~ord..arity~2..args~[type~variable..name~x..arity~0, type~variable..name~x..arity~0]], type~relation..name~ord..arity~2..args~[type~function..name~succ..arity~1..args~[type~variable..name~x..arity~0], type~function..name~succ..arity~1..args~[type~variable..name~x..arity~0]]]], type~relation..id~holds(1, 1, 0)..name~ord..arity~2..args~[type~constant..name~'1', type~constant..name~'1'..arity~0]], type~quantifier..id~qu(cond(2, 0), imp(cond(2, 0)))..name~!..arity~2..args~[[type~variable..name~x..dref~3..arity~0..args~[]..alph~latin..cap~small], type~logical_symbol..id~imp(cond(2, 0))..name~ (=>)..arity~2..args~[type~relation..id~pred(restrictor(2), 1)..name~naturalnumber..arity~1..args~[type~variable..name~x..dref~3..arity~0..args~[]..alph~latin..cap~small], type~relation..id~holds(scope(2), 4, 0)..name~ord..arity~_G123880..args~[type~variable..name~x..dref~3..arity~0..args~[]..alph~latin..cap~small, type~variable..name~x..dref~3..arity~0..args~[]..alph~latin..cap~small]]]]].

test('Induction PRS, checked') :- 
	utils:clean,
	p(induction,PRS),
	create_obligations:check_prs(PRS,[],[],Graph,[],Ob,[],X,check),
	X = [type~logical_symbol..id~ass(cond(0, 0), 0)..name~ (=>)..arity~2..args~[type~logical_symbol..id~and(holds(1, 2, 0), holds(1, 1, 0))..name~ & .. arity~2..args~[type~quantifier..id~holds(1, 2, 0)..name~!..args~[[type~variable..name~x..arity~0], type~logical_symbol..name~ (=>)..arity~2..args~[type~logical_symbol..name~ & .. args~[type~relation..name~naturalnumber..args~[type~variable..name~x..arity~0], type~relation..name~ord..arity~2..args~[type~variable..name~x..arity~0, type~variable..name~x..arity~0]], type~relation..name~ord..arity~2..args~[type~function..name~succ..arity~1..args~[type~variable..name~x..arity~0], type~function..name~succ..arity~1..args~[type~variable..name~x..arity~0]]]], type~relation..id~holds(1, 1, 0)..name~ord..arity~2..args~[type~constant..name~'1', type~constant..name~'1'..arity~0]], type~quantifier..id~qu(cond(2, 0), imp(cond(2, 0)))..name~!..arity~2..args~[[type~variable..name~x..dref~3..arity~0..args~[]..alph~latin..cap~small], type~logical_symbol..id~imp(cond(2, 0))..name~ (=>)..arity~2..args~[type~relation..id~pred(restrictor(2), 1)..name~naturalnumber..arity~1..args~[type~variable..name~x..dref~3..arity~0..args~[]..alph~latin..cap~small], type~relation..id~holds(scope(2), 4, 0)..name~ord..arity~_G28353..args~[type~variable..name~x..dref~3..arity~0..args~[]..alph~latin..cap~small, type~variable..name~x..dref~3..arity~0..args~[]..alph~latin..cap~small]]]]],
	discharge_obligations(Ob,Graph,_).


% ---------------------- check_neg_prs ---------------------------------------  


test('Negated PRS, unchecked') :-
% not (?x x=x)
	p(holds,PRS),
	create_obligations:check_neg_prs(neg(PRS),[],
		[],
		_,
		[],
		_Ob,
		[],
		X,	% Premises_end, Formula as DOBSOD
		nocheck),
		X = [type~logical_symbol..id~neg(3)..name~'~' ..arity~1..args~[type~relation..id~qe(3)..name~'$true'..args~[]]] .


test('Negated PRS, checked') :-
% not (?x x=x)
	utils:clean,
	p(holds,PRS),
	create_obligations:check_neg_prs(neg(PRS),[],
		[],
		Graph,
		[],
		Ob,
		[],
		X,	% Premises_end, Formula as DOBSOD
		check),
	!,
	X = [type~logical_symbol..id~neg(3)..name~'~' ..arity~1..args~[type~relation..id~qe(3)..name~'$true'..args~[]]],
	discharge_obligations(Ob,Graph,_).




% -------------------- check_conditions --------------------------------------

test('check_conditions: empty cond') :-
	create_obligations:check_conditions(0,[],[],_,_,G,G,Ob,Ob,Y,Y,nocheck).

test('check_conditions: empty PRS') :-
	p(empty,PRS),
	create_obligations:check_conditions(0,[],[PRS],0,_,[],_,[],[],[],[],nocheck).
	
test('check_conditions: math_id PRS') :-
	p(math_id,PRS),
	!,
	create_obligations:check_conditions(0,[],[PRS],0,_,_,_,[],[],Y,Y,nocheck).

test('check_conditions: math_id') :-
	phrase(fo_grammar:fo_term(DOBSOD_x,_),[x]),!,
	create_obligations:check_conditions(0,[],[math_id(5, DOBSOD_x)],0,_,[],_,[],[],[],[],nocheck).


% ----------------- check error messages -------------------------------------

% Missing Math_Id
test('Error case: Missing Math ID, unchecked') :-
	clear_messages,
	p(missing_id,PRS),
	\+ create_obligations:check_prs(PRS,[],
		[],
		_Graph,
		[],
		_Ob,
		[],
		_,
		nocheck),
	get_error_messages(Messages),
	Messages = [message(error, logic, check_prs, '3', 'Could not check PRS'), message(error, logic, check_conditions, '5', 'Could not find corresponding Formula')].


% Unknown condition
test('Error case: Unknown condition, unchecked') :-
	clear_messages,
	p(unknown_condition,PRS),		
	\+ create_obligations:check_prs(PRS,[],
		[],
		_Graph,
		[],
		_Ob,
		[],
		_,
		nocheck),
	get_error_messages(Messages),
	Messages = [message(error, logic, check_prs, '1', 'Could not check PRS'), message(error, logic, check_conditions, hrsga, 'Unknown condition')]	.


% Argument for 'holds' is not a Formula
test('Error case: No Formula, unchecked') :-
	clear_messages,
	p(no_formula,PRS),		
	\+ create_obligations:check_prs(PRS,[],
		[],
		_Graph,
		[],
		_Ob,
		[],
		_,
		nocheck),
	get_error_messages(Messages),
	Messages = [message(error, logic, check_prs, '1', 'Could not check PRS'), message(error, logic, check_conditions, '1', 'Argument for holds is not a Formula')].

% Definiendum PRS is empty
test('Error case: Empty definiendum PRS, nocheck') :-
	clear_messages,
	p(definiendum_empty,PRS),
	\+ create_obligations:check_prs(PRS,[],
	[],
	_Graph,
	[],
	_Ob,
	[],
	[],
	nocheck),
	get_messages(Messages),
	Messages = [message(error, logic, check_prs, '5', 'Could not check PRS'), message(error, logic, check_conditions, '3', 'Definiendum PRS is empty')].

% Definiens PRS is empty
test('Error case: Empty definiens PRS, nocheck') :-
	clear_messages,
	p(definiens_empty,PRS),
	\+ create_obligations:check_prs(PRS,[],
	[],
	_Graph,
	[],
	_Ob,
	[],
	[],
	nocheck),
	get_messages(Messages),
	Messages = [message(error, logic, check_prs, '5', 'Could not check PRS'), message(error, logic, check_conditions, '4', 'Definiens PRS is empty')].

% Error case: Assumption PRS is empty
test('Error case: Empty assumption PRS, nocheck') :-
	clear_messages,
	p(empty_assumption,PRS),
	create_obligations:check_prs(PRS,[],
	[],
	_Graph,
	[],
	_Ob,
	[],
	[],
	nocheck).

% Equivalence PRS is empty
test('Error case: Empty Equivalence, unchecked') :-
	clear_messages,
	p(equivalence_empty,PRS),
	\+ create_obligations:check_prs(PRS,[],[],_Graph,[],_Ob,[],_,check),
	get_messages(Messages),
	Messages = [
		message(error, logic, check_prs, proof_1, 'Could not check PRS'), 
		message(error, logic, check_prs, '0', 'Could not check PRS'), 
		message(error, logic, check_conditions, antecedent_11, 'Equivalence PRS B is empty')].

% Reverse Implication: Antecedent PRS is empty
test('Error case: Reverse Implication PRS, antecedent PRS is empty') :-
% x=y <= _
	clear_messages,
	p(reverse_implies_antecedent_empty,PRS),
	\+ create_obligations:check_prs(
		PRS,[],
		[],
		_Graph,
		[],
		_Ob,
		[],
		_,
		nocheck),
	get_messages(Messages),
	Messages = [message(error, logic, check_prs, proof_1, 'Could not check PRS'), 
			message(error, logic, check_prs, '0', 'Could not check PRS'), 
			message(error, logic, check_conditions, antecedent_11, 'Antecedent PRS is empty')].

% Reverse Implication: Antecedent PRS is empty
test('Error case: Reverse Implication PRS, succedent PRS is empty') :-
% _ <= y=x
	clear_messages,
	p(reverse_implies_succedent_empty,PRS),
	\+ create_obligations:check_prs(
		PRS,[],
		[],
		_Graph,
		[],
		_Ob,
		[],
		_,
		nocheck),
	get_messages(Messages),
	Messages = [message(error, logic, check_prs, proof_1, 'Could not check PRS'), 
		message(error, logic, check_prs, '0', 'Could not check PRS'), 
		message(error, logic, check_conditions, succedent_10, 'Succedent PRS is empty')].


%------------------------- check sentence_prs ------------------------------------------------------------------------------------------------------------------

test('Sentence PRS, unchecked') :-
	% Formula: x = x
	p(holds,PRS),
	create_obligations:check_sentence_prs(
		PRS,[],
		[],
		_,
		[],
		_Ob,
		[],
		X,
		check),
	X = [type~relation..arity~2..name~ (=)..args~[type~variable..arity~0..name~x, type~variable..arity~0..name~x]].

% ------------------------- test conjunct prs --------------------------------------------------------------------------


test('conjunct PRS, check') :-
	p(conjunct,PRS),
	create_obligations:check_prs(PRS,[],
	[],
	Graph,
	[],
	Ob,
	[],
	X,
	check),

	Graph = [0-[1], 1-[0, and(pred(conjunct2(1), 0), pred(conjunct1(1), 1))], conjunct1(1)-[and(pred(conjunct2(1), 0), pred(conjunct1(1), 1)), pred(conjunct1(1), 1)], conjunct2(1)-[and(pred(conjunct2(1), 0), pred(conjunct1(1), 1)), pred(conjunct2(1), 0)], qe(and(conjunct1(1), conjunct2(1)))-[and(pred(conjunct2(1), 0), pred(conjunct1(1), 1))], and(pred(conjunct2(1), 0), pred(conjunct1(1), 1))-[1, conjunct1(1), conjunct2(1), qe(and(conjunct1(1), conjunct2(1)))], pred(conjunct1(1), 1)-[conjunct1(1)], pred(conjunct2(1), 0)-[conjunct2(1)]],
	Ob = [
	[1, 
		[id~qe(and(conjunct1(1), conjunct2(1)))..args~[
			[args~[]..dref~1..name~x..type~variable..arity~0], 
			id~and(pred(conjunct2(1), 0), pred(conjunct1(1), 1))..args~[
				id~pred(conjunct2(1), 0)..args~[
					args~[]..dref~1..name~x..type~variable..arity~0
				]..name~prime..type~relation..arity~1, 
				id~pred(conjunct1(1), 1)..args~[
					args~[]..dref~1..name~x..type~variable..arity~0
				]..name~even..type~relation..arity~1
			]..name~ & .. type~logical_symbol..arity~2
		]..name~ ? .. type~quantifier], []]],

	X = [id~and(pred(conjunct2(1), 0), pred(conjunct1(1), 1))..args~[id~pred(conjunct2(1), 0)..args~[args~[]..dref~1..name~x..type~variable..arity~0]..dref~_G52637..name~prime..type~relation..arity~1, id~pred(conjunct1(1), 1)..args~[args~[]..dref~1..name~x..type~variable..arity~0]..dref~_G52699..name~even..type~relation..arity~1]..dref~_G52605..name~ & .. type~logical_symbol..arity~2] .



test('comma_conjunct PRS, check') :-
	p(comma_conjunct,PRS),
	create_obligations:check_prs(PRS,[],
	[],
	Graph,
	[],
	Ob,
	[],
	X,
	check),
	Graph = [0-[1], 1-[0, and(pred(comma_conjunct2(1), 0), pred(comma_conjunct1(1), 1))], comma_conjunct1(1)-[and(pred(comma_conjunct2(1), 0), pred(comma_conjunct1(1), 1)), pred(comma_conjunct1(1), 1)], comma_conjunct2(1)-[and(pred(comma_conjunct2(1), 0), pred(comma_conjunct1(1), 1)), pred(comma_conjunct2(1), 0)], qe(and(comma_conjunct1(1), comma_conjunct2(1)))-[and(pred(comma_conjunct2(1), 0), pred(comma_conjunct1(1), 1))], and(pred(comma_conjunct2(1), 0), pred(comma_conjunct1(1), 1))-[1, comma_conjunct1(1), comma_conjunct2(1), qe(and(comma_conjunct1(1), comma_conjunct2(1)))], pred(comma_conjunct1(1), 1)-[comma_conjunct1(1)], pred(comma_conjunct2(1), 0)-[comma_conjunct2(1)]], 

	Ob = [[1, [id~qe(and(comma_conjunct1(1), comma_conjunct2(1)))..args~[[args~[]..dref~1..name~x..type~variable..arity~0], id~and(pred(comma_conjunct2(1), 0), pred(comma_conjunct1(1), 1))..args~[id~pred(comma_conjunct2(1), 0)..args~[args~[]..dref~1..name~x..type~variable..arity~0]..name~prime..type~relation..arity~1, id~pred(comma_conjunct1(1), 1)..args~[args~[]..dref~1..name~x..type~variable..arity~0]..name~even..type~relation..arity~1]..name~ & .. type~logical_symbol..arity~2]..name~ ? .. type~quantifier], []]],
	X = [id~and(pred(comma_conjunct2(1), 0), pred(comma_conjunct1(1), 1))..args~[id~pred(comma_conjunct2(1), 0)..args~[args~[]..dref~1..name~x..type~variable..arity~0]..name~prime..type~relation..arity~1, id~pred(comma_conjunct1(1), 1)..args~[args~[]..dref~1..name~x..type~variable..arity~0]..name~even..type~relation..arity~1]..name~ & .. type~logical_symbol..arity~2] .


test('conjunct with no free var  PRS, check') :-
	p('conjunct with no free var, check',PRS),
	create_obligations:check_prs(PRS,[],
	[],
	_Graph,
	[],
	Ob,
	[],
	X,
	check),
	Ob = [
	[	conjunct2(scope(1)), 
		[id~holds(conjunct2(scope(1)), 2, 0)..args~[
			args~[]..name~x..type~variable..arity~0..dref~1, 
			args~[]..name~x..type~variable..arity~0..dref~1]..
		name~ (=)..type~relation..arity~2], 
		[id~holds(conjunct1(scope(1)), 2, 0)..args~[
			args~[]..name~x..type~variable..arity~0..dref~1, 
			args~[]..name~x..type~variable..arity~0..dref~1]..
		name~ (=)..type~relation..arity~2]
	], 
	[	conjunct1(scope(1)), 
		[id~holds(conjunct1(scope(1)), 2, 0)..args~[
			args~[]..name~x..type~variable..arity~0..dref~1, 
			args~[]..name~x..type~variable..arity~0..dref~1]..
		name~ (=)..type~relation..arity~2], 
		[]]
	],
	X = [
	id~ass(cond(1, 0), 0)..args~[
		[args~[]..name~x..type~variable..arity~0..dref~1], 
		id~holds(conjunct2(scope(1)), 2, 0)..args~[
			args~[]..name~x..type~variable..arity~0..dref~1, 
			args~[]..name~x..type~variable..arity~0..dref~1]..
		name~ (=)..type~relation..arity~2]..
	name~!..type~quantifier..arity~2, 
	id~ass(cond(1, 0), 1)..args~[
		[args~[]..name~x..type~variable..arity~0..dref~1], 
		id~holds(conjunct1(scope(1)), 2, 0)..args~[
			args~[]..name~x..type~variable..arity~0..dref~1, 
			args~[]..name~x..type~variable..arity~0..dref~1]..
		name~ (=)..type~relation..arity~2]..
	name~!..type~quantifier..arity~2].

% ----------------------------------------- check the prs --------------------------------------------
test('$x$ is the even prime number, check') :-
	premises:reset_counter,
	build_prs(PRS,[
	sentence(1,[math(['x']),'is','the','even','prime','number'])
	]),!,
	create_obligations:check_prs(PRS, [],
	[],
	_GraphOut,
	[],
	ObOut,
	[],
	POut,
	check),
	PRS = id~0..drefs~[]..mrefs~[]..rrefs~[]..accbefore~[]..accafter~[math_id(1, args~[]..dref~1..name~x..type~variable..arity~0),2]..conds~[id~1..drefs~[1]..mrefs~[args~[]..dref~1..name~x..type~variable..arity~0]..rrefs~[]..accbefore~[]..accafter~[math_id(1, args~[]..dref~1..name~x..type~variable..arity~0),2]..conds~[math_id(1, args~[]..dref~1..name~x..type~variable..arity~0), the(2, id~the(1)..drefs~[]..mrefs~[]..rrefs~[]..accbefore~[math_id(1, args~[]..dref~1..name~x..type~variable..arity~0)]..accafter~[math_id(1, args~[]..dref~1..name~x..type~variable..arity~0)]..conds~[predicate(2, [even]), predicate(2, [prime]), predicate(2, [number])]), predicate(1, 2, ['='])]],
	ObOut = [[1, [type~relation..id~qe(1)..name~ '$true'], [type~quantifier..id~replace(qu(theu(the(1), 1), imp(the(1))))..name~!..arity~2..args~[[type~variable..name~thex..dref~thex], type~logical_symbol..id~replace(imp(the(1)))..name~ (=>)..arity~2..args~[type~logical_symbol..id~replace(replace(and(pred(the(1), 2), and(pred(the(1), 1), pred(the(1), 0)))))..name~ & .. arity~2..args~[type~relation..id~replace(replace(pred(the(1), 2)))..name~number..arity~1..args~[type~variable..name~thex..dref~thex], type~logical_symbol..id~replace(replace(and(pred(the(1), 1), pred(the(1), 0))))..name~ & .. arity~2..args~[type~relation..id~replace(replace(pred(the(1), 1)))..name~prime..arity~1..args~[type~variable..name~thex..dref~thex], type~relation..id~replace(replace(pred(the(1), 0)))..name~even..arity~1..args~[type~variable..name~thex..dref~thex]]], type~relation..id~replace(the(1))..name~ (=)..arity~2..args~[type~variable..name~thex..dref~thex, type~function..name~skolem2..arity~0..args~[]]]], type~logical_symbol..id~replace(replace(and(pred(the(1), 2), and(pred(the(1), 1), pred(the(1), 0)))))..name~ & .. arity~2..args~[type~relation..id~replace(replace(pred(the(1), 2)))..name~number..arity~1..args~[type~function..name~skolem2..arity~0..args~[]], type~logical_symbol..id~replace(replace(and(pred(the(1), 1), pred(the(1), 0))))..name~ & .. arity~2..args~[type~relation..id~replace(replace(pred(the(1), 1)))..name~prime..arity~1..args~[type~function..name~skolem2..arity~0..args~[]], type~relation..id~replace(replace(pred(the(1), 0)))..name~even..arity~1..args~[type~function..name~skolem2..arity~0..args~[]]]]]], [the(1), [type~quantifier..id~qu(theu(the(1), 1), imp(the(1)))..name~!..arity~2..args~[[type~variable..name~thex..dref~thex], type~logical_symbol..id~imp(the(1))..name~ (=>)..arity~2..args~[type~logical_symbol..id~replace(and(pred(the(1), 2), and(pred(the(1), 1), pred(the(1), 0))))..name~ & .. arity~2..args~[type~relation..id~replace(pred(the(1), 2))..name~number..arity~1..args~[type~variable..name~thex..dref~thex], type~logical_symbol..id~replace(and(pred(the(1), 1), pred(the(1), 0)))..name~ & .. arity~2..args~[type~relation..id~replace(pred(the(1), 1))..name~prime..arity~1..args~[type~variable..name~thex..dref~thex], type~relation..id~replace(pred(the(1), 0))..name~even..arity~1..args~[type~variable..name~thex..dref~thex]]], type~relation..id~the(1)..name~ (=)..arity~2..args~[type~variable..name~thex..dref~thex, type~variable..name~thevar..dref~2..the~yes]]]..the~yes], [type~logical_symbol..id~and(pred(the(1), 2), and(pred(the(1), 1), pred(the(1), 0)))..name~ & .. arity~2..args~[type~relation..id~pred(the(1), 2)..name~number..arity~1..args~[type~variable..dref~2], type~logical_symbol..id~and(pred(the(1), 1), pred(the(1), 0))..name~ & .. arity~2..args~[type~relation..id~pred(the(1), 1)..name~prime..arity~1..args~[type~variable..dref~2], type~relation..id~pred(the(1), 0)..name~even..arity~1..args~[type~variable..dref~2]]]..the~yes]], [the(1), [type~quantifier..id~qe(thee(the(1), 1))..name~ ? .. args~[[type~variable..dref~2], type~logical_symbol..id~and(pred(the(1), 2), and(pred(the(1), 1), pred(the(1), 0)))..name~ & .. arity~2..args~[type~relation..id~pred(the(1), 2)..name~number..arity~1..args~[type~variable..dref~2], type~logical_symbol..id~and(pred(the(1), 1), pred(the(1), 0))..name~ & .. arity~2..args~[type~relation..id~pred(the(1), 1)..name~prime..arity~1..args~[type~variable..dref~2], type~relation..id~pred(the(1), 0)..name~even..arity~1..args~[type~variable..dref~2]]]..the~yes]..the~yes], []]],

POut = [type~relation..id~replace(pred(1, 2))..name~ (=)..arity~2..args~[type~variable..dref~1, type~function..name~skolem2..arity~0..args~[]], type~quantifier..id~replace(qu(theu(the(1), 1), imp(the(1))))..name~!..arity~2..args~[[type~variable..name~thex..dref~thex], type~logical_symbol..id~replace(imp(the(1)))..name~ (=>)..arity~2..args~[type~logical_symbol..id~replace(replace(and(pred(the(1), 2), and(pred(the(1), 1), pred(the(1), 0)))))..name~ & .. arity~2..args~[type~relation..id~replace(replace(pred(the(1), 2)))..name~number..arity~1..args~[type~variable..name~thex..dref~thex], type~logical_symbol..id~replace(replace(and(pred(the(1), 1), pred(the(1), 0))))..name~ & .. arity~2..args~[type~relation..id~replace(replace(pred(the(1), 1)))..name~prime..arity~1..args~[type~variable..name~thex..dref~thex], type~relation..id~replace(replace(pred(the(1), 0)))..name~even..arity~1..args~[type~variable..name~thex..dref~thex]]], type~relation..id~replace(the(1))..name~ (=)..arity~2..args~[type~variable..name~thex..dref~thex, type~function..name~skolem2..arity~0..args~[]]]], type~logical_symbol..id~replace(replace(and(pred(the(1), 2), and(pred(the(1), 1), pred(the(1), 0)))))..name~ & .. arity~2..args~[type~relation..id~replace(replace(pred(the(1), 2)))..name~number..arity~1..args~[type~function..name~skolem2..arity~0..args~[]], type~logical_symbol..id~replace(replace(and(pred(the(1), 1), pred(the(1), 0))))..name~ & .. arity~2..args~[type~relation..id~replace(replace(pred(the(1), 1)))..name~prime..arity~1..args~[type~function..name~skolem2..arity~0..args~[]], type~relation..id~replace(replace(pred(the(1), 0)))..name~even..arity~1..args~[type~function..name~skolem2..arity~0..args~[]]]]].





test('Let $x$ be the even prime number. Then $x=x$.,check') :-
	premises:reset_counter,
	build_prs(PRS,[
	sentence(1,['let',math([x]),'be','the','even','prime','number']),
	sentence(2,['then',math([x,=,x])])
	]),!,
	create_obligations:check_prs(PRS, [],
	[],
	_GraphOut,
	[],
	ObOut,
	[],
	POut,
	check),
	PRS = id~0..drefs~[]..mrefs~[]..rrefs~[]..accbefore~[]..accafter~[]..conds~[id~1..drefs~[1]..mrefs~[args~[]..dref~1..name~x..type~variable..arity~0]..rrefs~[]..accbefore~[]..accafter~[math_id(1, args~[]..dref~1..name~x..type~variable..arity~0), 2]..conds~[math_id(1, args~[]..dref~1..name~x..type~variable..arity~0), the(2, id~the(1)..drefs~[]..mrefs~[]..rrefs~[]..accbefore~[math_id(1, args~[]..dref~1..name~x..type~variable..arity~0)]..accafter~[math_id(1, args~[]..dref~1..name~x..type~variable..arity~0)]..conds~[predicate(2, [even]), predicate(2, [prime]), predicate(2, [number])]), predicate(1, 2, [=])]==>id~conseq(1)..drefs~[]..mrefs~[]..rrefs~[]..accbefore~[math_id(1, args~[]..dref~1..name~x..type~variable..arity~0), 2]..accafter~[math_id(1, args~[]..dref~1..name~x..type~variable..arity~0), 2, math_id(3, args~[args~[]..dref~1..name~x..type~variable..arity~0, args~[]..dref~1..name~x..type~variable..arity~0]..name~ (=)..type~relation..arity~2)]..conds~[id~2..drefs~[3]..mrefs~[args~[args~[]..dref~1..name~x..type~variable..arity~0, args~[]..dref~1..name~x..type~variable..arity~0]..name~ (=)..type~relation..arity~2]..rrefs~[]..accbefore~[math_id(1, args~[]..dref~1..name~x..type~variable..arity~0), 2]..accafter~[math_id(1, args~[]..dref~1..name~x..type~variable..arity~0), 2, math_id(3, args~[args~[]..dref~1..name~x..type~variable..arity~0, args~[]..dref~1..name~x..type~variable..arity~0]..name~ (=)..type~relation..arity~2)]..conds~[math_id(3, args~[args~[]..dref~1..name~x..type~variable..arity~0, args~[]..dref~1..name~x..type~variable..arity~0]..name~ (=)..type~relation..arity~2), holds(3)]]],
ObOut = [[2, [type~relation..id~holds(2, 3, 0)..name~ (=)..dref~G41775..arity~2..args~[type~variable..name~x..dref~1..arity~0..args~[], type~variable..name~x..dref~1..arity~0..args~[]]], [type~relation..id~replace(pred(1, 2))..name~ (=)..dref~G41668..arity~2..args~[type~variable..name~G41703..dref~1..args~[], type~function..name~skolem2..dref~G41740..arity~0..args~[]], type~quantifier..id~replace(qu(theu(the(1), 1), imp(the(1))))..name~!..dref~G41861..arity~2..args~[[type~variable..name~thex..dref~thex..args~[]], type~logical_symbol..id~replace(imp(the(1)))..name~ (=>)..dref~G41931..arity~2..args~[type~logical_symbol..id~replace(replace(and(pred(the(1), 2), and(pred(the(1), 1), pred(the(1), 0)))))..name~ & .. dref~G41966..arity~2..args~[type~relation..id~replace(replace(pred(the(1), 2)))..name~number..dref~G42001..arity~1..args~[type~variable..name~thex..dref~thex..args~[]], type~logical_symbol..id~replace(replace(and(pred(the(1), 1), pred(the(1), 0))))..name~ & .. dref~G42069..arity~2..args~[type~relation..id~replace(replace(pred(the(1), 1)))..name~prime..dref~G42104..arity~1..args~[type~variable..name~thex..dref~thex..args~[]], type~relation..id~replace(replace(pred(the(1), 0)))..name~even..dref~G42172..arity~1..args~[type~variable..name~thex..dref~thex..args~[]]]], type~relation..id~replace(the(1))..name~ (=)..dref~G42240..arity~2..args~[type~variable..name~thex..dref~thex..args~[], type~function..name~skolem2..dref~G41740..arity~0..args~[]]]], type~logical_symbol..id~replace(replace(and(pred(the(1), 2), and(pred(the(1), 1), pred(the(1), 0)))))..name~ & .. dref~G42326..arity~2..args~[type~relation..id~replace(replace(pred(the(1), 2)))..name~number..dref~G42361..arity~1..args~[type~function..name~skolem2..dref~G41740..arity~0..args~[]], type~logical_symbol..id~replace(replace(and(pred(the(1), 1), pred(the(1), 0))))..name~ & .. dref~G42429..arity~2..args~[type~relation..id~replace(replace(pred(the(1), 1)))..name~prime..dref~G42464..arity~1..args~[type~function..name~skolem2..dref~G41740..arity~0..args~[]], type~relation..id~replace(replace(pred(the(1), 0)))..name~even..dref~G42532..arity~1..args~[type~function..name~skolem2..dref~G41740..arity~0..args~[]]]]]], [the(1), [type~quantifier..id~qu(theu(the(1), 1), imp(the(1)))..name~!..arity~2..args~[[type~variable..name~thex..dref~thex..args~[]], type~logical_symbol..id~imp(the(1))..name~ (=>)..arity~2..args~[type~logical_symbol..id~replace(and(pred(the(1), 2), and(pred(the(1), 1), pred(the(1), 0))))..name~ & .. arity~2..args~[type~relation..id~replace(pred(the(1), 2))..name~number..arity~1..args~[type~variable..name~thex..dref~thex..args~[]], type~logical_symbol..id~replace(and(pred(the(1), 1), pred(the(1), 0)))..name~ & .. arity~2..args~[type~relation..id~replace(pred(the(1), 1))..name~prime..arity~1..args~[type~variable..name~thex..dref~thex..args~[]], type~relation..id~replace(pred(the(1), 0))..name~even..arity~1..args~[type~variable..name~thex..dref~thex..args~[]]]], type~relation..id~the(1)..name~ (=)..arity~2..args~[type~variable..name~thex..dref~thex..args~[], type~variable..name~thevar..dref~2..the~yes]]]..the~yes], [type~logical_symbol..id~and(pred(the(1), 2), and(pred(the(1), 1), pred(the(1), 0)))..name~ & .. arity~2..args~[type~relation..id~pred(the(1), 2)..name~number..arity~1..args~[type~variable..dref~2], type~logical_symbol..id~and(pred(the(1), 1), pred(the(1), 0))..name~ & .. arity~2..args~[type~relation..id~pred(the(1), 1)..name~prime..arity~1..args~[type~variable..dref~2], type~relation..id~pred(the(1), 0)..name~even..arity~1..args~[type~variable..dref~2]]]..the~yes]], [the(1), [type~quantifier..id~qe(thee(the(1), 1))..name~ ? .. args~[[type~variable..dref~2], type~logical_symbol..id~and(pred(the(1), 2), and(pred(the(1), 1), pred(the(1), 0)))..name~ & .. arity~2..args~[type~relation..id~pred(the(1), 2)..name~number..arity~1..args~[type~variable..dref~2], type~logical_symbol..id~and(pred(the(1), 1), pred(the(1), 0))..name~ & .. arity~2..args~[type~relation..id~pred(the(1), 1)..name~prime..arity~1..args~[type~variable..dref~2], type~relation..id~pred(the(1), 0)..name~even..arity~1..args~[type~variable..dref~2]]]..the~yes]..the~yes], []]],
POut = [type~quantifier..id~ass(cond(0, 0), 0)..name~!..arity~2..args~[[type~variable..name~x..dref~1..arity~0..args~[]], type~logical_symbol..id~imp(cond(0, 0), 0)..name~ (=>)..arity~2..args~[type~relation..id~replace(pred(1, 2))..name~ (=)..dref~G41668..arity~2..args~[type~variable..name~G41703..dref~1..args~[], type~function..name~skolem2..dref~G41740..arity~0..args~[]], type~relation..id~holds(2, 3, 0)..name~ (=)..dref~G41775..arity~2..args~[type~variable..name~x..dref~1..arity~0..args~[], type~variable..name~x..dref~1..arity~0..args~[]]]], type~quantifier..id~replace(qu(theu(the(1), 1), imp(the(1))))..name~!..dref~G41861..arity~2..args~[[type~variable..name~thex..dref~thex..args~[]], type~logical_symbol..id~replace(imp(the(1)))..name~ (=>)..dref~G41931..arity~2..args~[type~logical_symbol..id~replace(replace(and(pred(the(1), 2), and(pred(the(1), 1), pred(the(1), 0)))))..name~ & .. dref~G41966..arity~2..args~[type~relation..id~replace(replace(pred(the(1), 2)))..name~number..dref~G42001..arity~1..args~[type~variable..name~thex..dref~thex..args~[]], type~logical_symbol..id~replace(replace(and(pred(the(1), 1), pred(the(1), 0))))..name~ & .. dref~G42069..arity~2..args~[type~relation..id~replace(replace(pred(the(1), 1)))..name~prime..dref~G42104..arity~1..args~[type~variable..name~thex..dref~thex..args~[]], type~relation..id~replace(replace(pred(the(1), 0)))..name~even..dref~G42172..arity~1..args~[type~variable..name~thex..dref~thex..args~[]]]], type~relation..id~replace(the(1))..name~ (=)..dref~G42240..arity~2..args~[type~variable..name~thex..dref~thex..args~[], type~function..name~skolem2..dref~G41740..arity~0..args~[]]]], type~logical_symbol..id~replace(replace(and(pred(the(1), 2), and(pred(the(1), 1), pred(the(1), 0)))))..name~ & .. dref~G42326..arity~2..args~[type~relation..id~replace(replace(pred(the(1), 2)))..name~number..dref~G42361..arity~1..args~[type~function..name~skolem2..dref~G41740..arity~0..args~[]], type~logical_symbol..id~replace(replace(and(pred(the(1), 1), pred(the(1), 0))))..name~ & .. dref~G42429..arity~2..args~[type~relation..id~replace(replace(pred(the(1), 1)))..name~prime..dref~G42464..arity~1..args~[type~function..name~skolem2..dref~G41740..arity~0..args~[]], type~relation..id~replace(replace(pred(the(1), 0)))..name~even..dref~G42532..arity~1..args~[type~function..name~skolem2..dref~G41740..arity~0..args~[]]]]].

test('Plural 1') :-
	premises:reset_counter,
	build_prs(PRS,[
	sentence(1,[math([x]),'and',math([y]),'are','prime'])
	]),!,
	create_obligations:check_prs(PRS, [],
	[],
	_GraphOut,
	[],
	ObOut,
	[],
	POut,
	check),
	ObOut = [[1, [id~qe(1)..args~[[args~[]..dref~1..name~x..type~variable..arity~0, args~[]..dref~2..name~y..type~variable..arity~0], id~and(pred(s2(plural(1)), 0), pred(s1(plural(1)), 0))..args~[id~pred(s2(plural(1)), 0)..args~[args~[]..dref~2..name~G32490..type~variable]..dref~G32456..name~prime..type~relation..arity~1, id~pred(s1(plural(1)), 0)..args~[args~[]..dref~1..name~G32541..type~variable]..dref~G32507..name~prime..type~relation..arity~1]..name~ & .. type~logical_symbol..arity~2]..name~ ? .. type~quantifier], []]],
	POut = [id~pred(s2(plural(1)), 0)..args~[args~[]..dref~2..name~G32490..type~variable]..dref~G32456..name~prime..type~relation..arity~1, id~pred(s1(plural(1)), 0)..args~[args~[]..dref~1..name~G32541..type~variable]..dref~G32507..name~prime..type~relation..arity~1]. 


:-end_tests(create_obligations).

