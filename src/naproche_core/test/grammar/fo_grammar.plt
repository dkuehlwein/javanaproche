:- begin_tests(fo_grammar).

% ------------- Terminal Symbols ------------------

test('Left Bracket') :-
	fo_grammar:left_bracket(['('],[]).
test('Right Bracket') :-
	fo_grammar:right_bracket([')'],[]).
test('Comma') :-
	fo_grammar:comma([','],[]).

test('Variable') :-
	fo_grammar:fo_variable(X,[x],[]),
	X = type~variable ..name~x.
test('Constant') :-
	fo_grammar:fo_constant(X,[c1],[]),
	X = type~constant ..name~c1.
test('Function Symbol') :-
	fo_grammar:fo_function_symbol(X,[f],[]),
	X = type~function ..name~f.
test('Logic Symbol') :-
	fo_grammar:fo_logical_symbol(X,['\wedge'],[]),
	X = type~logical_symbol ..name~'&'.
test('Quantifier') :-
	fo_grammar:fo_quantifier(X,['\forall'],[]),
	X = type~quantifier ..name~'!'.

% Relation Symbols
test('Relation Ord') :-
        findall(X,fo_grammar:fo_relation(X,['Ord'],[]),Xs),
        Xs = [type~relation ..name~ord].
test('Relation Trans') :-
        findall(X,fo_grammar:fo_relation(X,['Trans'],[]),Xs),
        Xs = [type~relation ..name~trans].
test('Relation R') :-
        findall(X,fo_grammar:fo_relation(X,['R'],[]),Xs),
        Xs = [type~relation ..name~'R'].
test('Relation =') :-
        findall(X,fo_grammar:fo_relation(X,['='],[]),Xs),
        Xs = [type~relation ..name~'='].
test('Relation !=') :-
        findall(X,fo_grammar:fo_relation(X,['\neq'],[]),Xs),
        Xs = [type~relation ..name~'~='].
test('Relation element') :-
        findall(X,fo_grammar:fo_relation(X,['\in'],[]),Xs),
        Xs = [type~relation ..name~in].
test('Relation leq') :-
        findall(X,fo_grammar:fo_relation(X,['\leq'],[]),Xs),
        Xs = [type~relation ..name~leq].
test('Relation geq') :-
        findall(X,fo_grammar:fo_relation(X,['\geq'],[]),Xs),
        Xs = [type~relation ..name~geq].
test('Relation less than') :-
        findall(X,fo_grammar:fo_relation(X,['<'],[]),Xs),
        Xs = [type~relation ..name~less].
test('Relation greater than') :-
        findall(X,fo_grammar:fo_relation(X,['>'],[]),Xs),
        Xs = [type~relation ..name~greater].

% Negation
test('Negation') :-
        fo_grammar:fo_logical_symbol(X,['\neg'],[]),!,
        X = type~logical_symbol ..name~'~'.


% ----------- Functions --------------------------

test('Function Arity 1') :-
	findall([X,Y],fo_grammar:fo_function(X,Y,[f,'(',x,')'],[]),Xs),
	Xs = [[type~function..name~f..args~[type~variable..arity~0..name~x], [type~variable..arity~0..name~x]]].
	
test('Function Arity 2') :-
	findall([X,Y], fo_grammar:fo_function(X,Y,[g,'(',x,',',y,')'],[]), Xs),
	Xs = [[type~function..arity~2..name~g..args~[type~variable..arity~0..name~x, type~variable..arity~0..name~y], [type~variable..arity~0..name~x, type~variable..arity~0..name~y]]].


test('Function Arity 3') :-
	findall([X,Y], fo_grammar:fo_function(X,Y,[h,'(',x,',',y,',',z,')'],[]), Xs),
	Xs = 	[[type~function..arity~3..name~h..args~[type~variable..arity~0..name~x, type~variable..arity~0..name~y, type~variable..arity~0..name~z], 
		[type~variable..arity~0..name~x, type~variable..arity~0..name~y, type~variable..arity~0..name~z]]].

test('Function x*y') :-
	findall([X,Y], fo_grammar:fo_function(X,Y,[x,'*',y],[]), Xs),
	Xs = [[type~function..arity~2..name~mul..args~[type~variable..arity~0..name~x, type~variable..arity~0..name~y], [type~variable..arity~0..name~x, type~variable..arity~0..name~y]]].

test('Function c*y') :-
	findall([X,Y], fo_grammar:fo_function(X,Y,[c1,'*',y],[]), Xs),	
	Xs = [[type~function..arity~2..name~mul..args~[type~constant..arity~0..name~c1, type~variable..arity~0..name~y], [type~variable..arity~0..name~y]]].

test('Function (x*z)*y') :-
	findall([X,Y], fo_grammar:fo_function(X,Y,['(',x,'*',z,')','*',y],[]), Xs),
	Xs = [[type~function..arity~2..name~mul..args~[type~function..arity~2..name~mul..args~[type~variable..arity~0..name~x, type~variable..arity~0..name~z], type~variable..arity~0..name~y], 
		[type~variable..arity~0..name~x, type~variable..arity~0..name~z, type~variable..arity~0..name~y]]].

test('Function (x*z)*(x*y)') :-
	findall([X,Y], fo_grammar:fo_function(X,Y,['(',x,'*',z,')','*','(',x,'*',y,')'],[]), Xs),
	Xs = 	[[type~function..arity~2..name~mul..args~[
			type~function..arity~2..name~mul..args~
				[type~variable..arity~0..name~x, type~variable..arity~0..name~z], 
			type~function..arity~2..name~mul..args~
				[type~variable..arity~0..name~x, type~variable..arity~0..name~y]], 
		[type~variable..arity~0..name~x, type~variable..arity~0..name~z, type~variable..arity~0..name~y]]].

test('Function x*f(x)') :-
	findall([X,Y], fo_grammar:fo_function(X,Y,[x,'*',f,'(',x,')'],[]), Xs),
	Xs = [[type~function..arity~2..name~mul..args~[type~variable..arity~0..name~x, type~function..name~f..args~[type~variable..arity~0..name~x]], [type~variable..arity~0..name~x]]].


test('Function f(x)*y') :-
	findall([X,Y], fo_grammar:fo_function(X,Y,[f,'(',x,')','*',y],[]), Xs),
	Xs = 	[[type~function..arity~2..name~mul..args~[type~function..name~f..args~[type~variable..arity~0..name~x], type~variable..arity~0..name~y], 
		[type~variable..arity~0..name~x, type~variable..arity~0..name~y]]].

test('Function x succ') :-
	findall([X,Y], fo_grammar:fo_function(X,Y,[x,succ],[]), Xs),
	Xs = [[type~function..arity~1..name~succ..args~[type~variable..arity~0..name~x], [type~variable..arity~0..name~x]]].

test('Function (x succ)succ') :-
	findall([X,Y], fo_grammar:fo_function(X,Y,['(',x,succ,')',succ],[]), Xs),
	Xs =  [[type~function..arity~1..name~succ..args~[type~function..arity~1..name~succ..args~[type~variable..arity~0..name~x]], [type~variable..arity~0..name~x]]].


test('Function x succ*f(y)') :-
	findall([X,Y], fo_grammar:fo_function(X,Y,[x,succ,*,f,'(',y,')'],[]), Xs),
	Xs = 	[[type~function..arity~2..name~mul..args~[
			type~function..arity~1..name~succ..args~
				[type~variable..arity~0..name~x], 
				type~function..name~f..args~[type~variable..arity~0..name~y]], 
		[type~variable..arity~0..name~x, type~variable..arity~0..name~y]]].

test('Function f(x succ)') :-
	findall([X,Y], fo_grammar:fo_function(X,Y,[f,'(',x,succ,')'],[]), Xs),
	Xs = [[type~function..name~f..args~[type~function..arity~1..name~succ..args~[type~variable..arity~0..name~x]], [type~variable..arity~0..name~x]]].

test('Function f(x) succ') :-
	findall([X,Y], fo_grammar:fo_function(X,Y,[f,'(',x,')',succ],[]), Xs),
	Xs = [[type~function..arity~1..name~succ..args~[type~function..name~f..args~[type~variable..arity~0..name~x]], [type~variable..arity~0..name~x]]].
	
test('Function (x*y)succ') :-
	findall([X,Y], fo_grammar:fo_function(X,Y,['(',x,*,y,')',succ],[]), Xs),
	Xs = 	[[type~function..arity~1..name~succ..args~[type~function..arity~2..name~mul..args~[type~variable..arity~0..name~x, type~variable..arity~0..name~y]], 
		[type~variable..arity~0..name~x, type~variable..arity~0..name~y]]].

% --------------- Terms --------------------------

test('Term Variable') :-
	findall([X,Y],fo_grammar:fo_term(X,Y,[x],[]),Xs),
	Xs = [[type~variable..arity~0..name~x, [type~variable..arity~0..name~x]]].
test('Term Constant') :-
	findall([X,Y],fo_grammar:fo_term(X,Y,[c1],[]),Xs),
	Xs = [[type~constant ..name~c1,[]]].
test('Term Function') :-
	findall([X,Y],fo_grammar:fo_term(X,Y,[h,'(',x,',',y,',',z,')'],[]),Xs),
	Xs = 	[[type~function..arity~3..name~h..args~[type~variable..arity~0..name~x, type~variable..arity~0..name~y, type~variable..arity~0..name~z], 
		[type~variable..arity~0..name~x, type~variable..arity~0..name~y, type~variable..arity~0..name~z]]].

% -------------- Formulas ------------------------

test('Formula in Brackets') :-
	findall(X,fo_grammar:fo_formula(X,_,['(',x,'=',x,')'],[]),Xs),
	Xs = [type~relation..name~'='..args~[type~variable..name~x..arity~0, type~variable..name~x..arity~0]..arity~2].
test('False') :- 
	findall(X,fo_grammar:fo_formula(X,_,[contradiction],[]),Xs),
	Xs = [type~relation..name~'$false'..arity~0].
test('Relation(Term) is a formula') :-
	findall(X,fo_grammar:fo_formula(X,_,['Ord','(',x,')'],[]),Xs),
	Xs = [type~relation..name~ord..args~[type~variable..name~x..arity~0]..arity~1].
test('Relation(Term1,Term2) is a formula') :-
	findall([X,Y],fo_grammar:fo_formula(X,Y,['R','(',x,',',y,')'],[]),Xs),
	Xs = [[type~relation..arity~2..name~'R'..args~[type~variable..arity~0..name~x, type~variable..arity~0..name~y], [type~variable..arity~0..name~x, type~variable..arity~0..name~y]]].
test('Term1 Relation Term2 is a formula') :-
	findall([X,Y],fo_grammar:fo_formula(X,Y,[x,'=',x],[]),Xs),
	Xs = [[type~relation..arity~2..name~ (=)..args~[type~variable..arity~0..name~x, type~variable..arity~0..name~x], [type~variable..arity~0..name~x]]].
test('Quantified Formula') :-
	findall([X,Y],fo_grammar:fo_formula(X,Y,['\forall',x,'(',x,'=',x,')'],[]),Xs),
	Xs = [[type~quantifier..name~'!'..
	    args~[[type~variable..name~x..arity~0], type~relation..name~'='..args~[type~variable..name~x..arity~0, type~variable..name~x..arity~0]..arity~2]..arity~2,[]]].
test('Quantified Formula with 2 Vars') :-
	findall([X,Y],fo_grammar:fo_formula(X,Y,['\forall',x,',',y,'(',x,'=',y,')'],[]),Xs),
	Xs = [[type~quantifier..name~'!'..
	    args~[[type~variable..name~x..arity~0,type~variable..name~y], type~relation..name~'='..args~[type~variable..name~x..arity~0, type~variable..name~y..arity~0]..arity~2]..arity~2,[]]].
test('Negated Formula') :-
	findall([X,Y],fo_grammar:fo_formula(X,Y,['\neg','(',x,'=',x,')'],[]),Xs),
	Xs = 	[[type~logical_symbol..name~'~'.. args~[type~relation..name~'='..args~[type~variable..name~x..arity~0, type~variable..name~x..arity~0]..arity~2]..arity~1, 
		[type~variable..name~x..arity~0]]].
test('Formula Logical Symbol Formula is a formula') :-
	findall([X,Y],fo_grammar:fo_formula(X,Y,['(',x,'=',x,')','\wedge','(',y,'=',y,')'],[]),Xs),
	Xs = [[type~logical_symbol..name~ & .. 
            args~[type~relation..name~ (=)..args~[type~variable..name~x..arity~0, type~variable..name~x..arity~0]..arity~2,
                  type~relation..name~ (=)..args~[type~variable..name~y..arity~0, type~variable..name~y..arity~0]..arity~2]
            ..arity~2, [type~variable..name~x..arity~0, type~variable..name~y..arity~0]]].

test('x=2*y is a formula') :-
	findall([X,Y],fo_grammar:fo_formula(X,Y,[x,'=','2','*',y],[]),Xs),
	Xs = [[type~relation..arity~2..name~ (=)..args~[type~variable..arity~0..name~x, type~function..arity~2..name~mul..args~[type~constant..arity~0..name~'2', type~variable..arity~0..name~y]], 		[type~variable..arity~0..name~x, type~variable..arity~0..name~y]]].

test('Ord(x)') :-
	findall([X,Y],fo_grammar:fo_formula(X,Y,['Ord','(',x,')'],[]),Xs),
	Xs = [[type~relation..arity~1..name~ord..args~[type~variable..arity~0..name~x], [type~variable..arity~0..name~x]]].

test('x=x') :-
	findall(X,fo_grammar:fo_formula(X,_,[x,'=',x],[]),Xs),
        Xs = [name~ (=)..type~relation..args~[name~x..type~variable, name~x..type~variable]..arity~2].

test('For all x f(x) = g(y,z)') :-
	findall(X,fo_grammar:fo_formula(X,_,['\forall', x, f,'(',x,')', =, g,'(',y,',',z,')'],[]),Xs),
        Xs = [name~!..type~quantifier..args~[
                [name~x..type~variable..arity~0],
                name~ (=)..type~relation..args~[
                        name~f..type~function..args~[
                                name~x..type~variable..arity~0]..
                        arity~1,
                        name~g..type~function..args~[
                                name~y..type~variable..arity~0,
                                name~z..type~variable..arity~0]..
                        arity~2]..
        arity~2]].

test('(for all x (x=x))') :-
	findall(X,fo_grammar:fo_formula(X,_,['(','\forall', x, '(',x, =,x,')',')'],[]),Xs),
        Xs = [name~!..type~quantifier..args~[
                [name~x..type~variable],
                name~ (=)..type~relation..args~[
                        name~x..type~variable,
                        name~x..type~variable]..
        arity~2]].

test('Variables are in right order') :-
% Formula x=y and z=y --> Freevar has to be [x,y,z]
	findall(Freevar,fo_grammar:fo_formula(_,Freevar,['(',x,'=',y,')','\wedge','(',z,'=',y,')'],[]),Xs),
	Xs = [[type~variable..arity~0..name~x, type~variable..arity~0..name~y, type~variable..arity~0..name~z]].

test('Chained Formula') :-
	findall(FreeVar,fo_grammar:fo_chained_formula(Term1,Tree,FreeVar,[x,'=',x,r,y,r,z],[]),FreeVars),
	Term1 = type~variable..arity~0..name~x,
	Tree = type~logical_symbol..arity~2..name~ & .. args~[
		type~relation..arity~2..name~ (=)..args~[type~variable..arity~0..name~x, type~variable..arity~0..name~x], 
  		type~logical_symbol..arity~2..name~ & .. args~[
  			type~relation..arity~2..name~r..args~[type~variable..arity~0..name~x, type~variable..arity~0..name~y], 
  			type~relation..arity~2..name~r..args~[type~variable..arity~0..name~y, type~variable..arity~0..name~z]
  		]
  	],
	FreeVars = [[type~variable..arity~0..name~x, type~variable..arity~0..name~y, type~variable..arity~0..name~z]].

test('formula with complex term'):-
	findall([Tree,FreeVar],fo_formula(Tree,FreeVar,[f, '(', x, ')', *, x, =, '1'],[]),X),
	X=[[type~relation..arity~2..name~ (=)..args~[type~function..arity~2..name~mul..args~[type~function..name~f..args~[type~variable..arity~0..name~x], type~variable..arity~0..name~x], type~constant..arity~0..name~'1'], [type~variable..arity~0..name~x]]].
	

% -------------------------- Expr with logical symbols --------------------------------------------------
test('x=x & Ord(x)') :-
	fo_grammar:fo_formula(X,_,[x,'=',x,'\wedge','Ord','(',x,')'],[]),!,
        X = name~ & .. type~logical_symbol..args~[
                name~ (=)..type~relation..args~[
                        name~x..type~variable,
                        name~x..type~variable]..
                arity~2,
                name~ord..type~relation..args~  [
                        name~x..type~variable]..
                arity~1]..
        arity~2.

test('(for all x (x=x)) \wedge Ord(y)') :-
	fo_grammar:fo_formula(X,_,['(','\forall',x,'(',x, '=',x,')',')','\wedge','Ord','(',y,')'],[]),!,
        X = name~ & .. type~logical_symbol..args~[
                name~!..type~quantifier..args~[
                        [name~x..type~variable],
                        name~ (=)..type~relation..args~[
                                name~x..type~variable,
                                name~x..type~variable]..
                arity~2],
                name~ord..type~relation..args~  [
                        name~y..type~variable]..
                arity~1]..
        arity~2.

test('x=x | Ord(x)') :-
	fo_grammar:fo_formula(X,_,[x,=,x,'\vee','Ord','(',x,')'],[]),!,
        X = name~ | .. type~logical_symbol..args~[
                name~ (=)..type~relation..args~[
                        name~x..type~variable,
                        name~x..type~variable]..
                arity~2,
                name~ord..type~relation..args~  [
                        name~x..type~variable]..
                arity~1]..
        arity~2.

test('(for all x (x=x)) | Ord(y)') :-
	fo_grammar:fo_formula(X,_,['(','\forall',x,'(',x, '=',x,')',')','\vee','Ord','(',y,')'],[]),!,
        X = name~ | .. type~logical_symbol..args~[
                name~!..type~quantifier..args~[
                        [name~x..type~variable],
                        name~ (=)..type~relation..args~[
                                name~x..type~variable,
                                name~x..type~variable]..
                arity~2],
                name~ord..type~relation..args~  [
                        name~y..type~variable]..
                arity~1]..
        arity~2.

test('x=x => Ord(x)') :-
	fo_grammar:fo_formula(X,_,[x,=,x,'\rightarrow','Ord','(',x,')'],[]),!,
        X = name~ '=>' .. type~logical_symbol..args~[
                name~ (=)..type~relation..args~[
                        name~x..type~variable,
                        name~x..type~variable]..
                arity~2,
                name~ord..type~relation..args~  [
                        name~x..type~variable]..
                arity~1]..
        arity~2.

test('(for all x (x=x)) => Ord(y)') :-
	fo_grammar:fo_formula(X,_,['(','\forall',x,'(',x, '=',x,')',')','\rightarrow','Ord','(',y,')'],[]),!,
        X = name~ '=>' .. type~logical_symbol..args~[
                name~!..type~quantifier..args~[
                        [name~x..type~variable],
                        name~ (=)..type~relation..args~[
                                name~x..type~variable,
                                name~x..type~variable]..
                arity~2],
                name~ord..type~relation..args~  [
                        name~y..type~variable]..
                arity~1]..
        arity~2.

test('x=x <=> Ord(x)') :-
	fo_grammar:fo_formula(X,_,[x,=,x,'\leftrightarrow','Ord','(',x,')'],[]),!,
        X = name~ '<=>' .. type~logical_symbol..args~[
                name~ (=)..type~relation..args~[
                        name~x..type~variable,
                        name~x..type~variable]..
                arity~2,
                name~ord..type~relation..args~  [
                        name~x..type~variable]..
                arity~1]..
        arity~2.

test('(for all x (x=x)) <=> Ord(y)') :-
	fo_grammar:fo_formula(X,_,['(','\forall',x,'(',x, '=',x,')',')','\leftrightarrow','Ord','(',y,')'],[]),!,
        X = name~ '<=>' .. type~logical_symbol..args~[
                name~!..type~quantifier..args~[
                        [name~x..type~variable],
                        name~ (=)..type~relation..args~[
                                name~x..type~variable,
                                name~x..type~variable]..
                arity~2],
                name~ord..type~relation..args~  [
                        name~y..type~variable]..
                arity~1]..
        arity~2.



% ------------------- Brackets ---------------------------

test('(x=x) & (y=y)') :-
	fo_grammar:fo_formula(X,_,['(',x, '=',x,')','\wedge','(',y, '=',y,')'],[]),!,
        X = name~ & .. type~logical_symbol..args~[
                name~ (=)..type~relation..args~[
                        name~x..type~variable..arity~0,
                        name~x..type~variable..arity~0]..
                arity~2,
                name~ (=)..type~relation..args~[
                        name~y..type~variable..arity~0,
                        name~y..type~variable..arity~0]..
                arity~2]..
        arity~2.


test('((x=x) & (y=y))') :-
	fo_grammar:fo_formula(X,_,['(','(',x, '=',x,')','\wedge','(',y, '=',y,')',')'],[]),!,
        X = name~ & .. type~logical_symbol..args~[
                name~ (=)..type~relation..args~[
                        name~x..type~variable..arity~0,
                        name~x..type~variable..arity~0]..
                arity~2,
                name~ (=)..type~relation..args~[
                        name~y..type~variable..arity~0,
                        name~y..type~variable..arity~0]..
                arity~2]..
        arity~2.


test('x=x & y=y') :-
	fo_grammar:fo_formula(X,_,[x, '=',x,'\wedge',y, '=',y],[]),!,
        X = name~ & .. type~logical_symbol..args~[
                name~ (=)..type~relation..args~[
                        name~x..type~variable..arity~0,
                        name~x..type~variable..arity~0]..
                arity~2,
                name~ (=)..type~relation..args~[
                        name~y..type~variable..arity~0,
                        name~y..type~variable..arity~0]..
                arity~2]..
        arity~2.

test('((x=x) & (y=y)) & (z=z)') :-
	fo_grammar:fo_formula(X,_,['(','(',x, '=',x,')','\wedge','(',y, '=',y,')',')','\wedge','(',z, '=',z,')'],[]),!,
        X = name~ & .. type~logical_symbol..args~[
                name~ & .. type~logical_symbol..args~[
                        name~ (=)..type~relation..args~[
                                name~x..type~variable..arity~0,
                                name~x..type~variable..arity~0]..
                        arity~2,
                        name~ (=)..type~relation..args~[
                                name~y..type~variable..arity~0,
                                name~y..type~variable..arity~0]..
                        arity~2]..
                arity~2,
                name~ (=)..type~relation..args~[
                        name~z..type~variable..arity~0,
                        name~z..type~variable..arity~0]..
                arity~2]..
        arity~2.


test('(x=x) & ((y=y) & (z=z))') :-
	fo_grammar:fo_formula(X,_,['(',x, '=',x,')','\wedge','(','(',y, '=',y,')','\wedge','(',z, '=',z,')',')'],[]),!,
        X = name~ & .. type~logical_symbol..args~[
                name~ (=)..type~relation..args~[
                        name~x..type~variable..arity~0,
                        name~x..type~variable..arity~0]..
                arity~2,
                name~ & .. type~logical_symbol..args~[
                        name~ (=)..type~relation..args~[
                                name~y..type~variable..arity~0,
                                name~y..type~variable..arity~0]..
                        arity~2,
                        name~ (=)..type~relation..args~[
                                name~z..type~variable..arity~0,
                                name~z..type~variable..arity~0]..
                        arity~2]..
                arity~2]..
        arity~2.


% Free Var test
test('Free var x=x') :-
        fo_grammar:fo_formula(_,[type~variable..arity~0..name~'x'],[x,'=',x],[]),!.

test('Free var x=x & y=y') :-
        fo_grammar:fo_formula(_,[arity~0..type~variable..name~x, arity~0..type~variable..name~y],[x,'=',x,'\wedge',y,'=',y],[]),!.

test('Free var for all x (x=x)') :-
        fo_grammar:fo_formula(_,[],['\forall',x,'(',x,'=',x,')'],[]),!.

test('For all x y x=y') :-
        fo_grammar:fo_formula(X,[],['\forall',x,',',y,'(',x,'=',y,')'],[]),!,
        X = name~! .. type~quantifier..args~[
                [name~x..type~variable..arity~0,
                name~y..type~variable..arity~0],
                name~ (=)..type~relation..args~[
                        name~x..type~variable..arity~0,
                        name~y..type~variable..arity~0]..
                arity~2]..
        arity~2.


% ---------------- Free Functions ----------------------
test('Free Function Arity 0') :-
	fo_grammar:fo_free_function(c1,X,[],[c1],[]),
	X = type~constant..name~c1..arity~0.
test('Free Function Arity 1') :- 
	fo_grammar:fo_free_function(f,X,Y,[f,'(',x,')'],[]),
	X = type~function..name~f..args~[type~variable..name~x..arity~0]..arity~1,
	Y = [type~variable..name~x..arity~0].
test('Free Function Arity 2') :- 
	fo_grammar:fo_free_function(g,X,Y,[g,'(',x,',',y,')'],[]),
	X = type~function..name~g..
	    args~Y..arity~2,
	Y = [type~variable..name~x..arity~0, type~variable..name~y..arity~0].
test('Free Function Arity 3') :- 
	fo_grammar:fo_free_function(h,X,Y,[h,'(',x,',',y,',',z,')'],[]),
	X = type~function..name~h..
	    args~Y..arity~3,
	Y = [type~variable..name~x..arity~0, type~variable..name~y..arity~0, type~variable..name~z..arity~0].


% ----------- Terms without symbol --------------------

test('Term without Symbol: Variable') :-
	fo_grammar:fo_term_without(f,Tree,[Tree],[x],[]).
test('Term without Symbol: Constant') :-
	fo_grammar:fo_term_without(f,_,[],[c1],[]).
test('Term without Symbol: Function') :-
	fo_grammar:fo_term_without(f,_,_,['f2','(',x,')'],[]).
test('Term without Symbol: fail') :-
	\+ fo_grammar:fo_term_without(f,_,_,[f,'(',x,')'],[]).

% ----------- Function without Symbol -----------------
test('Function without Symbol: Arity1') :-
	fo_grammar:fo_term_without(f,_,_,['f2','(',x,')'],[]).
test('Function without Symbol: Arity2') :-
	fo_grammar:fo_term_without(f,_,_,[g,'(',x,',',y,')'],[]).
test('Function without Symbol: Arity1') :-
	fo_grammar:fo_term_without(f,_,_,[h,'(',x,',',y,',',z,')'],[]).
test('Function without Symbol and function symbol in the middle') :-
	fo_grammar:fo_term_without(f,_,_,[x,'*',x],[]).
test('Function without Symbol and function symbol in the middle using difference lists') :-
	fo_grammar:fo_term_without(f,_,_,[x,'*',x, f],[f]).

% ----------- Successor Function ------------------------
test('Succ Function') :-
	fo_grammar:fo_succ(X,Y,[succ,'(',x,')'],[]),
	X = type~function..name~succ..args~[type~variable..name~x..arity~0]..arity~1,
	Y = [type~variable..name~x..arity~0].

% ------------- Free Predicate Symbol -------------------

test('0-ary Predicate Symbol') :- 
	fo_grammar:fo_free_predicate_symbol(X,Y,[contradiction],[]),
	X = type~relation..name~'$false'..arity~0,
	Y = [].
test('1-ary Predicate Symbol') :-
	fo_grammar:fo_free_predicate_symbol(X,Y,['Ord','(',x,')'],[]),
	X = type~relation..name~ord..args~[type~variable..name~x..arity~0]..arity~1,
	Y = [type~variable..name~x..arity~0].
test('2-ary Predicate Symbol') :-
	fo_grammar:fo_free_predicate_symbol(X,Y,['R','(',x,',',y,')'],[]),
	X = type~relation..name~'R'..args~[type~variable..name~x..arity~0, type~variable..name~y..arity~0]..arity~2,
	Y = [type~variable..name~x..arity~0, type~variable..name~y..arity~0].
test('2-ary Predicate Symbol in the middle') :-
	fo_grammar:fo_free_predicate_symbol(X,Y,[x,'=',x],[]),
	X = type~relation..name~'='..args~[type~variable..name~x..arity~0, type~variable..name~x..arity~0]..arity~2,
	Y = [type~variable..name~x..arity~0] .


% ------------- Induction predicates -------------------

test('Induction Start: f(1) = 2') :-
	fo_grammar:fo_induction_start(f,1,Tree,LHSVar,RHSVar,[f,'(','1',')','=','2'],[]),
	Tree = type~relation..name~ (=)..args~[
	 type~function..name~f..args~[type~constant..name~'1'..arity~0]..arity~1, 
	 type~constant..name~'2'..arity~0]..arity~2,
	LHSVar = [],
	RHSVar = [].

test('Induction Start: g(1,x) = f(x)') :-
	fo_grammar:fo_induction_start(g,1,Tree,LHSVar,RHSVar,[g,'(','1',',',x,')','=',f,'(',x,')'],[]),
	Tree = type~relation..name~ (=)..args~[
	 type~function..name~g..args~[
	  type~constant..name~'1'..arity~0, type~variable..name~x..arity~0
	  ]..arity~2, 
	 type~function..name~f..args~[
	  type~variable..name~x..arity~0
	  ]..arity~1
	 ]..arity~2,
	LHSVar = [type~variable..name~x..arity~0],
	RHSVar = [type~variable..name~x..arity~0].

test('Induction Start: 1*x = f(x)') :-
	fo_grammar:fo_induction_start(mul,1,Tree,LHSVar,RHSVar,['1','*',x,'=',f,'(',x,')'],[]),
	Tree = type~relation..name~ (=)..args~[
	 type~function..name~mul..args~[
	  type~constant..name~'1'..arity~0, type~variable..name~x..arity~0
	  ]..arity~2, 
	 type~function..name~f..args~
	  [type~variable..name~x..arity~0]..arity~1
	]..arity~2,
	LHSVar = [type~variable..name~x..arity~0],
	RHSVar = [type~variable..name~x..arity~0].

test('Induction Start: x*1 = f(x)') :-
	fo_grammar:fo_induction_start(mul,2,Tree,LHSVar,RHSVar,[x,'*','1','=',f,'(',x,')'],[]),
	Tree = type~relation..name~ (=)..args~[
	 type~function..name~mul..args~[
	  type~variable..name~x..arity~0, type~constant..name~'1'..arity~0
	  ]..arity~2, 
	 type~function..name~f..args~[
	  type~variable..name~x..arity~0]..arity~1]..arity~2,
	LHSVar = [type~variable..name~x..arity~0],
	RHSVar = [type~variable..name~x..arity~0].

test('Induction Start: h(x,y,1) = g(x,y)') :-
	fo_grammar:fo_induction_start(h,3,Tree,LHSVar,RHSVar,[h,'(',x,',',y,',','1',')','=',g,'(',x,',',y,')'],[]),
	Tree = type~relation..name~ (=)..args~[
	 type~function..name~h..args~[
	  type~variable..name~x..arity~0, type~variable..name~y..arity~0, type~constant..name~'1'..arity~0]
	 ..arity~3, 
	 type~function..name~g..args~[
	  type~variable..name~x..arity~0, type~variable..name~y..arity~0]..arity~2
	]..arity~2,
	LHSVar = [type~variable..name~x..arity~0, type~variable..name~y..arity~0],
	RHSVar = [type~variable..name~x..arity~0, type~variable..name~y..arity~0].

test('Induction Start Fail: f(1,1) = 1',fail):-
	fo_grammar:fo_induction_start(f,_,_,_,_,[f,'(','1',',','1',')','=','1'],[]).

test('Induction Start Fail: f(1) = f(1)',fail):-
	fo_grammar:fo_induction_start(f,_,_,_,_,[f,'(','1',')','=',f,'(','1',')'],[]).

test('Induction start LHS: f(1)') :-
	fo_grammar:fo_induction_start_LHS(f, ['1'], 1, X,Y, [f, '(', '1', ')'],[]),
	X = type~function..name~f..args~[type~constant..name~'1'..arity~0]..arity~1,
	Y = [].

test('Induction start LHS:h (x,y,1)') :-
	fo_grammar:fo_induction_start_LHS(h,['1'],3,X,Y,[h,'(',x,',',y,',','1',')'],[]),
	X = type~function..name~h..args~[
	 type~variable..name~x..arity~0, type~variable..name~y..arity~0, type~constant..name~'1'..arity~0
	 ]..arity~3,
	Y = [type~variable..name~x..arity~0, type~variable..name~y..arity~0].

test('Induction step: f(succ(x))=f(x)') :-
	fo_grammar:fo_induction_step(f,1,Tree,LHSVar,RHSVar,[f,'(','succ','(',x,')',')','=',f,'(',x,')'],[]),
	Tree = type~relation..name~ (=)..args~[
	 type~function..name~f..args~[
	  type~function..name~succ..args~[type~variable..name~x..arity~0]..arity~1]..arity~1, 
	 type~function..name~f..args~[
	  type~variable..name~x..arity~0]..arity~1]..arity~2,
	LHSVar = [type~variable..name~x..arity~0],
	RHSVar = [type~variable..name~x..arity~0].

test('Induction step: g(y,succ(x)) = h(y,z,x)') :-
	fo_grammar:fo_induction_step(g,2,Tree,LHSVar,RHSVar,[g,'(',y,',','succ','(',x,')',')','=',h,'(',y,',',z,',',x,')'],[]),
	Tree = type~relation..name~ (=)..args~[
	 type~function..name~g..args~[
	  type~variable..name~y..arity~0, 
	  type~function..name~succ..args~[type~variable..name~x..arity~0]..arity~1]..arity~2, 
	 type~function..name~h..args~[
	  type~variable..name~y..arity~0, 
	  type~variable..name~z..arity~0, 
	  type~variable..name~x..arity~0]..arity~3]..arity~2,
	LHSVar = [type~variable..name~y..arity~0, type~variable..name~x..arity~0],
	RHSVar = [type~variable..name~y..arity~0, type~variable..name~z..arity~0, type~variable..name~x..arity~0].

test('Induction step g(y,succ(x)) = g(y,x)') :-
	fo_grammar:fo_induction_step(g,2,Tree,LHSVar,RHSVar,[g,'(',y,',','succ','(',x,')',')','=',g,'(',y,',',x,')'],[]),
	Tree = type~relation..name~ (=)..args~[
	 type~function..name~g..args~[
	  type~variable..name~y..arity~0, 
	  type~function..name~succ..args~[
	   type~variable..name~x..arity~0]..arity~1]..arity~2, 
	  type~function..name~g..args~[
	   type~variable..name~y..arity~0, 
	   type~variable..name~x..arity~0]..arity~2]..arity~2,
	LHSVar = [type~variable..name~y..arity~0, type~variable..name~x..arity~0],
	RHSVar = [type~variable..name~y..arity~0, type~variable..name~x..arity~0].

test('Induction step fail: f(succ(x),1) = 1',fail):-
	fo_grammar:fo_induction_step(f,1,_,_,_,[f,'(',succ,'(',x,')',',','1',')','=','1'],[]).

test('Induction step fail: f(succ(x),x) = 1',fail):-
	fo_grammar:fo_induction_step(f,1,_,_,_,[f,'(',succ,'(',x,')',',','x',')','=','1'],[]).

test('Induction step fail: succ(x)*x = 1',fail):-
	fo_grammar:fo_induction_step(mul,1,_,_,_,[succ,'(',x,')','*','x','=','1'],[]).

test('Induction step fail: f(succ(x)) = f(1)',fail):-
	fo_grammar:fo_induction_step(f,1,_,_,_,[f,'(',succ,'(',x,')',')','=',f,'(','1',')'],[]).

/*
% ------------- Error messages  -------------------

test('No term, five atoms'):- 
    clear_messages, 
    findall(X,fo_grammar:fo_term(X,[],[m,y,b,o,nnie],[]),[]),
    get_messages(Messages),
    Messages = [message(error, syntax, fo_term, mybonnie, 'Term expected')].

test('No term, empty list'):- 
    clear_messages, 
    findall(X,fo_grammar:fo_term(X,[],[],[]),[]),
    get_messages(Messages),
    Messages = [message(error, syntax, fo_term, '', 'Term expected')].


test('No formula, four atoms'):- 
    clear_messages, 
    findall(X,fo_grammar:fo_formula(X,[],['is',over,the,ocean],[]),[]),
    get_messages(Messages),
    Messages = [message(error, syntax, fo_formula, isovertheocean, 'Formula expected')].

test('No term, empty list'):- 
    clear_messages, 
    findall(X,fo_grammar:fo_formula(X,[],[],[]),[]),
    get_messages(Messages),
    Messages = [message(error, syntax, fo_formula, '', 'Formula expected')].
*/
%------------- Union in right order -----------------
test('Union in right order'):-
 	union_in_right_order([a,b,x,c],[x,y],X),
	X = [a, b, x, c, y].


:- end_tests(fo_grammar).

