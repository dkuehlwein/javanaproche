:- begin_tests(expr_grammar).

try(Input,Output) :-
        delete(Input,32,Tmp),
        expr_grammar:expr(Output,10,_,Tmp,[]).

% Variables
test('Variable') :-
        expr_grammar:variable(X,"x",[]),!,
        X = type~variable ..name~x.
% Constant
test('Constant') :-
        expr_grammar:const(X,[],"c",[]),!,
        X = type~constant ..name~c.
% funcsym
test('Function') :-
        expr_grammar:funcsym(X,"f",[]),!,
        X = type~function ..name~f.
% relation
test('Relation Ord') :-
        expr_grammar:relationsymb(X,"Ord",[]),!,
        X = type~relation ..name~ord.
test('Relation Trans') :-
        expr_grammar:relationsymb(X,"Trans",[]),!,
        X = type~relation ..name~trans.
test('Relation R') :-
        expr_grammar:relationsymb(X,"R",[]),!,
        X = type~relation ..name~r.
test('Relation =') :-
        expr_grammar:relationsymb(X,"=",[]),!,
        X = type~relation ..name~'='.
test('Relation !=') :-
        expr_grammar:relationsymb(X,"\u2260",[]),!,
        X = type~relation ..name~'~='.
test('Relation element') :-
        expr_grammar:relationsymb(X,"\u2208",[]),!,
        X = type~relation ..name~in.
test('Relation leq') :-
        expr_grammar:relationsymb(X,"\u2264",[]),!,
        X = type~relation ..name~leq.
test('Relation geq') :-
        expr_grammar:relationsymb(X,"\u2265",[]),!,
        X = type~relation ..name~geq.
test('Relation less than') :-
        expr_grammar:relationsymb(X,"\u003C",[]),!,
        X = type~relation ..name~less.
test('Relation greater than') :-
        expr_grammar:relationsymb(X,"\u003E",[]),!,
        X = type~relation ..name~greater.

% Negation
test('Negation') :-
        expr_grammar:logsym(X,"\u00ac",[]),!,
        X = type~logical_symbol ..name~'~'.

% ------------------- Expr Tests ------------------------
test('Ord(x)') :-
        try("Ord(x)",X),!,
        X = name~ord..type~relation..args~[name~x..type~variable]..arity~1.

test('x=x') :-
        try("x=x",X),!,
        X = name~ (=)..type~relation..args~[name~x..type~variable, name~x..type~variable]..arity~2.

test('For all x f(x) = g(y,z)') :-
        try("\u2200 x f(x) = g(y,z)",X),!,
        X = name~!..type~quantifier..args~[
                [name~x..type~variable..arity~0], 
                name~ (=)..type~relation..args~[
                        name~f..type~function..args~[
                                name~x..type~variable..arity~0]..
                        arity~1, 
                        name~g..type~function..args~[
                                name~y..type~variable..arity~0, 
                                name~z..type~variable..arity~0]..
                        arity~2]..
        arity~2].

test('(for all x (x=x))') :-
        try("(\u2200 x (x=x))",X),!,
        X = name~!..type~quantifier..args~[
                [name~x..type~variable], 
                name~ (=)..type~relation..args~[
                        name~x..type~variable, 
                        name~x..type~variable]..
        arity~2].


% -------------------------- Expr with logical symbols --------------------------------------------------
test('x=x & Ord(x)') :-
        try("x=x \u2227 Ord(x)",X),!,
        X = name~ & .. type~logical_symbol..args~[
                name~ (=)..type~relation..args~[
                        name~x..type~variable,
                        name~x..type~variable]..
                arity~2, 
                name~ord..type~relation..args~  [
                        name~x..type~variable]..
                arity~1]..
        arity~2.

test('(for all x (x=x)) \u2227 Ord(y)') :-
        try("(\u2200 x (x=x)) \u2227 Ord(y)",X),!,
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
        try("x=x \u2228 Ord(x)",X),!,
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
        try("(\u2200 x (x=x)) \u2228 Ord(y)",X),!,
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
        try("x=x \u2192 Ord(x)",X),!,
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
        try("(\u2200 x (x=x)) \u2192 Ord(y)",X),!,
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
        try("x=x \u2194 Ord(x)",X),!,
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
        try("(\u2200 x (x=x)) \u2194 Ord(y)",X),!,
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
        try("(x=x) \u2227 (y=y)",X),!,
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
        try("((x=x) \u2227 (y=y))",X),!,
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
        try("x=x \u2227 y=y",X),!,
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
        try("((x=x) \u2227 (y=y)) \u2227 (z=z)",X),!,
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
        try("(x=x) \u2227 ((y=y) \u2227 (z=z))",X),!,
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
        expr_grammar:expr(_,10,[type~variable..arity~0..name~'x'],"x=x",[]),!.

test('Free var x=x & y=y') :-
        expr_grammar:expr(_,10,[arity~0..type~variable..name~y, arity~0..type~variable..name~x],"x=x\u2227y=y",[]),!.

test('Free var for all x (x=x)') :-
        expr_grammar:expr(_,10,[],"\u2200x(x=x)",[]),!. 



:- end_tests(expr_grammar).
