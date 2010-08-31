:- module(math_lexicon,[math_lexicon/2]).

:- use_module(naproche(dcg_lexicon)).


/**     <module>  This module contains all mathematical items which math_grammar can parse
*
*
*/


%%      math_lexicon(Atom,Gulp_list)
%
%       math_lexicon lists all items and their properties which math_grammar can parse.
%
%       ==
%       math_lexicon(x,type~variable..arity~0..name~'x').
%       math_lexicon(f,type~function..arity~1..name~'f').
%       ==
%
%       @param Item     The Name of the item as a String.
%       @param DOBSOD   The DOBSOD representation of the Item.
%


% Variables
math_lexicon(['a'],type~variable..arity~0..args~[]..name~'a'..alph~latin..cap~small).
math_lexicon(['b'],type~variable..arity~0..args~[]..name~'b'..alph~latin..cap~small).
math_lexicon(['c'],type~variable..arity~0..args~[]..name~'c'..alph~latin..cap~small).
math_lexicon(['d'],type~variable..arity~0..args~[]..name~'d'..alph~latin..cap~small).
math_lexicon(['e'],type~variable..arity~0..args~[]..name~'e'..alph~latin..cap~small).
math_lexicon(['J'],type~variable..arity~0..args~[]..name~'J'..alph~latin..cap~cap).
math_lexicon(['K'],type~variable..arity~0..args~[]..name~'K'..alph~latin..cap~cap).
math_lexicon(['L'],type~variable..arity~0..args~[]..name~'L'..alph~latin..cap~cap).
math_lexicon(['M'],type~variable..arity~0..args~[]..name~'M'..alph~latin..cap~cap).
math_lexicon(['N'],type~variable..arity~0..args~[]..name~'N'..alph~latin..cap~cap).
math_lexicon(['m'],type~variable..arity~0..args~[]..name~'m'..alph~latin..cap~small).
math_lexicon(['n'],type~variable..arity~0..args~[]..name~'n'..alph~latin..cap~small).
% The order of the features of 'o' is different due to a strange bug with GULP:
math_lexicon(['o'],type~variable..arity~0..args~[]..alph~latin..cap~small..name~'o').
math_lexicon(['p'],type~variable..arity~0..args~[]..name~'p'..alph~latin..cap~small).
math_lexicon(['q'],type~variable..arity~0..args~[]..name~'q'..alph~latin..cap~small).
math_lexicon(['r'],type~variable..arity~0..args~[]..name~'r'..alph~latin..cap~small).
math_lexicon(['s'],type~variable..arity~0..args~[]..name~'s'..alph~latin..cap~small).
math_lexicon(['t'],type~variable..arity~0..args~[]..name~'t'..alph~latin..cap~small).
math_lexicon(['u'],type~variable..arity~0..args~[]..name~'u'..alph~latin..cap~small).
math_lexicon(['v'],type~variable..arity~0..args~[]..name~'v'..alph~latin..cap~small).
math_lexicon(['w'],type~variable..arity~0..args~[]..name~'w'..alph~latin..cap~small).
math_lexicon(['x'],type~variable..arity~0..args~[]..name~'x'..alph~latin..cap~small).
math_lexicon(['y'],type~variable..arity~0..args~[]..name~'y'..alph~latin..cap~small).
math_lexicon(['z'],type~variable..arity~0..args~[]..name~'z'..alph~latin..cap~small).
math_lexicon(['\alpha'],type~variable..arity~0..args~[]..name~'alpha'..alph~greek..cap~small).
math_lexicon(['\beta'],type~variable..arity~0..args~[]..name~'beta'..alph~greek..cap~small).
math_lexicon(['\gamma'],type~variable..arity~0..args~[]..name~'gamma'..alph~greek..cap~small).
math_lexicon(['\delta'],type~variable..arity~0..args~[]..name~'delta'..alph~greek..cap~small).

% Constants
math_lexicon(['\emptyset'],type~constant..arity~0..args~[]..name~'emptyset').
math_lexicon(['c1'],type~constant..arity~0..args~[]..name~'c1').
math_lexicon([-,'1'],type~constant..arity~0..args~[]..name~'minus1').
math_lexicon(['\perp'],type~constant..arity~0..args~[]..name~'perp').
math_lexicon(['0'],type~constant..arity~0..args~[]..name~'0').
math_lexicon(['1'],type~constant..arity~0..args~[]..name~'1').
math_lexicon(['2'],type~constant..arity~0..args~[]..name~'2').
math_lexicon(['3'],type~constant..arity~0..args~[]..name~'3').
math_lexicon(['4'],type~constant..arity~0..args~[]..name~'4').
math_lexicon(['5'],type~constant..arity~0..args~[]..name~'5').

% Functions
math_lexicon(['f'],type~function..name~'f').
math_lexicon(['f2'],type~function..name~'f2').
math_lexicon(['g'],type~function..name~'g').
math_lexicon(['h'],type~function..name~'h').
math_lexicon(['gcd'],type~function..name~'gcd').
math_lexicon(['\sqrt'],type~function..name~'sqrt').
math_lexicon(['*'],type~function..name~'mul').
math_lexicon(['+'],type~function..name~'plus').
math_lexicon(['^'],type~function..name~'exp').
math_lexicon(['succ'],type~function..name~'succ').
math_lexicon(['skolem'],type~function..name~'skolem').
math_lexicon(['\angle'],type~function..name~'angle').
math_lexicon(['\frac'],type~function..name~'frac').
math_lexicon(['\cup'],type~function..name~'union').
math_lexicon(['\cap'],type~function..name~'intersection').
math_lexicon(['\overline'],type~function..name~'overline').
math_lexicon(['\triangle'],type~function..name~'triangle').
math_lexicon(['\angle'],type~function..name~'angle').
% The following line should be included again once there are operator priority rules ("_" should bind stronger than all other operators).
% math_lexicon(['_'],type~function..name~'subscript').


% Relation
math_lexicon(['contradiction'],type~relation..name~'$false').
math_lexicon(['Ord'],type~relation..name~'ord').
math_lexicon(['ord'],type~relation..name~'ord').
math_lexicon(['Trans'],type~relation..name~'trans').
math_lexicon(['trans'],type~relation..name~'trans').
math_lexicon(['R'],type~relation..name~'R').
math_lexicon(['S'],type~relation..name~'S').
math_lexicon(['W'],type~relation..name~'W').
math_lexicon(['r'],type~relation..name~'r').
math_lexicon(['M'],type~relation..name~'M').
math_lexicon(['='],type~relation..name~'='..arity~2).
math_lexicon(['\neq'],type~relation..name~'~=').
math_lexicon(['\in'],type~relation..name~'in'..arity~2).
math_lexicon(['\leq'],type~relation..name~'leq').
math_lexicon(['\geq'],type~relation..name~'geq').
math_lexicon(['<'],type~relation..name~'less').
math_lexicon(['>'],type~relation..name~'greater').
math_lexicon(['\subset'],type~relation..name~'subset').
math_lexicon(['\supset'],type~relation..name~'supset').
math_lexicon(['\sim'],type~relation..name~'sim').
math_lexicon(['\preceq'],type~relation..name~'preceq').
math_lexicon(['\succeq'],type~relation..name~'succeq').

% All nouns, adjectives, verbs and prepositions from dcg_lexicon are relations

math_lexicon(Words,type~relation..arity~1..name~Word):-
	dcg_lexicon(_,noun,_,Words),
	atomic_list_concat(Words,Word).
% "distinct" is read as "~="
math_lexicon([distinct],type~relation..arity~2..name~'~='..grouped~1):-
	dcg_lexicon([distinct],adjective,_).
math_lexicon([Word],type~relation..arity~2..name~Word..grouped~1):-
	dcg_lexicon([Word],adjective,adj_trans~T),
	\+ T = no.
math_lexicon([Word],type~relation..arity~1..name~Word):-
	dcg_lexicon([Word],adjective,transitive~no).
math_lexicon([Word],type~relation..arity~1..name~Word):-
	dcg_lexicon(_,verb,transitive~minus,Word).
math_lexicon([Word],type~relation..arity~2..name~Word):-
	dcg_lexicon(_,verb,transitive~plus,Word).
math_lexicon(Words,type~relation..arity~2..grouped~no..name~Word):-
	dcg_lexicon(Words,preposition,grouped~no),
	concat_atom(Words,Word).
math_lexicon(Words,type~relation..arity~3..grouped~1..name~Word):-
	dcg_lexicon(Words,preposition,grouped~1),
	concat_atom(Words,Word).
math_lexicon(Words,type~relation..arity~3..grouped~2..name~Word):-
	dcg_lexicon(Words,preposition,grouped~2),
	concat_atom(Words,Word).

% logical symbols
math_lexicon(['\neg'],type~logical_symbol..arity~1..name~'~').
math_lexicon(['\wedge'],type~logical_symbol..arity~2..name~'&').
math_lexicon(['\vee'],type~logical_symbol..arity~2..name~'|').
math_lexicon(['\rightarrow'],type~logical_symbol..arity~2..name~'=>').
math_lexicon(['\leftrightarrow'],type~logical_symbol..arity~2..name~'<=>').


% quantifiers
math_lexicon(['\forall'],type~quantifier..arity~2..name~'!').
math_lexicon(['\exists'],type~quantifier..arity~2..name~'?').
