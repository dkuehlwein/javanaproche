:- module(texmacs,
          [naproche_sentences/2] ).

%% naproche_sentences(+File, -Sentences) is det.
%
% Reads a Naproche XML File and extracts the significant Sentences.
naproche_sentences(File, Sentences) :-
  xml_doc(File, Doc),
  get_elements_by_name(Doc, quotation, Quotes),
  get_elements_by_name(Quotes, concat, Lines),
  maplist(line_contents, Lines, NumberedLinesWithContents),
  maplist(line_sentences, NumberedLinesWithContents, NestedNumberedSentences),
  flatten(NestedNumberedSentences, Sentences).

line_contents(element(concat, Attributes, Content), line(Id, L)) :-
  memberchk(id=Id, Attributes),
  findall(Child,
          ( member(Child, Content),
            subsumes_chk(element(_, _, _), Child)
          ),
          Children),
  maplist(content, Children, ChildContent),
  tokenize_line(ChildContent, T),
  flatten(T, L).

content(element(math, _Attributes, [Content]), math(ContentAsList)) :-
  !,
  name(Content, ContentAsList).

content(element(ElementName, _Attributes, [Content]), Content) :-
  ElementName \= math.

line_sentences(line(LineId, _LineContents), NumberedSentences) :-
  %split(LineContents, '.', Sentences),
  number_sentences(LineId, _Sentences, NumberedSentences).

number_sentences(LineId, Sentences, NumberedSentences) :-
  number_sentences(Sentences, LineId, 0, NumberedSentences).

number_sentences([Sentence|Sentences], LineId, Index, [sentence(id(SentenceId), Sentence)|NSentences]) :-
  atom_number(IndexName, Index),
  concat_atom([LineId, IndexName], '-', SentenceId),
  succ(Index, NewIndex),
  number_sentences(Sentences, LineId, NewIndex, NSentences).
number_sentences([], _, _, []).

% Utility predicates
tokenize_line([math(X)|RestLine], [math(X)|RestTokens]) :-
  !,
  tokenize_line(RestLine, RestTokens).

tokenize_line([LineSegment|RestLine], [LowercaseTokens|RestTokens]) :-
  atom(LineSegment),
  tokenize_atom(LineSegment, Tokens),
  maplist(downcase_atom, Tokens, LowercaseTokens),
  tokenize_line(RestLine, RestTokens).

tokenize_line([], []).
