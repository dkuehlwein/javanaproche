:- module(xml,
         [xml_doc/2,
          xml_doc/3,
          get_elements_by_name/3]).

/** <module> High-level XML access

This module builds on the XML predicates provided by the built-in SGML library.
It encapsulates loading and searching XML files and error reporting.

@author Nickolay Kolev
*/

%% xml_doc(+File, -Doc, +Doc) is det.
%
% Parses File to Doc, validating it agains DTD.
%
% True if File is an XML file which conforms to the Document Type Definition
% specified in DTD and which can successfully be parsed to an internal Prolog
% representation.
%
% _|Note that Doc is the root element of the XML file and *not* a list of elements
% as returned by the load_* predicates of the SGML library.|_
%
% Usage:
%
%==
% ?- xml_doc('examples/ord.xml', Doc, 'examples/allowed.dtd').
%==
%
% @param File is the file to be parsed.
% @param Doc is the internal representation of File.
% @param DTD is a DTD file against which to validate File during parsing.
xml_doc(File, Doc, DTD) :-
  catch(read_xml_file(File, [Doc], DTD),
        error(ErrorType, Msgs),
        (report_error(ErrorType, Msgs), fail)).

%% xml_doc(+File, -Doc)
%
% Parses File to Doc, performing some validation.
% 
% True if File can be parsed to Doc without a DTD. Defined as
%
%==
% xml_doc(File, Doc) :-
%   xml_doc(File, Doc, _).
%==
xml_doc(File, Doc) :-
  xml_doc(File, Doc, _).


read_xml_file(File, Document, DTDFile) :-
  retractall(sgml_parser_error(File, _, _)),
  (
      atom(DTDFile)
  ->  (new_dtd(xml, DTD),
      load_dtd(DTD, DTDFile),
      load_structure(File, Document, [dialect(xml), max_errors(0), space(remove), dtd(DTD)])
      )
  ;   load_structure(File, Document, [dialect(xml), max_errors(0), space(remove)])
  ),
  (   findall(File:Line:Msg, retract(sgml_parser_error(File, Line, Msg)), Errors),
      Errors \== []
  ->  throw(error(xml_error, Errors))
  ;   true
  ).

% A message hook to catch SGML parser messages
user:message_hook(sgml(sgml_parser(_), File, Line, Msg), _WarningOrError, _Lines) :-
  assert(sgml_parser_error(File, Line, Msg)).

% Error reporting
report_error(xml_error, [File:Line:Msg|_]) :-
  write_report(File, Line, Msg).

report_error(limit_exceeded(max_errors, 0), _) :-
  retract(sgml_parser_error(File, Line, Msg)),
  write_report(File, Line, Msg).

report_error(existence_error(source_sink, File), _) :-
  format('~nThe input file ~w could not be found.', [File]).

write_report(File, Line, Msg) :-
  format('~nAn error occured while processing the  XML input.~nHere is what the parser said:~n~n'),
  format('File: ~t~10| ~w~n', [File]),
  format('Line: ~t~10| ~w~n', [Line]),
  format('Message: ~t~10| ~w~n', [Msg]).


%% get_elements_by_name(+Element, +ElementName, -Elements) is det.
%
% Enables the search by tag name for elements within an XML file.
%
% True if Elements is a list of elements nested within and including Element
% whose tag name is ElementName.
%
% Element may be a single element or a list of elements. The second case enables
% the chaining of get_elements_by_name/3 calls. For example (from texmaxs.pl):
%
%==
%   ...
%   xml_doc('examples/ord.xml', Doc),
%   get_elements_by_name(Doc, quote, Quotes),
%   get_elements_by_name(Quotes, concat, Concats),
%   ...
%==
%
% unifies Concats with all =concat= elements nested within =quote= elements in
% the file =|examples/ord.xml|=.
%
% @param Element is an element or a list of elements within which to search.
% @param ElementName is the name of the elements we are searching for.
% @param Elements is unified with the resulting list of all found elements.
get_elements_by_name(Elements, NodeName, Nodes) :-
  get_elements_by_name_x(Elements, NodeName, [], ReverseNodes),
  reverse(ReverseNodes, Nodes).

get_elements_by_name(element(ElementName, Attributes, Content), NodeName, Nodes) :-
  get_elements_by_name([element(ElementName, Attributes, Content)], NodeName, Nodes).

get_elements_by_name(Element, _, Acc, Acc) :-
  atom(Element).

get_elements_by_name(element(NodeName, Attributes, Content), NodeName, Acc, Nodes) :- !,
  get_elements_by_name_x(Content, NodeName, [element(NodeName, Attributes, Content)|Acc], Nodes).

get_elements_by_name(element(ElName, _, Content), NodeName, Acc, Nodes) :-
  ElName \== NodeName,
  get_elements_by_name_x(Content, NodeName, Acc, Nodes).

get_elements_by_name_x([], _, Acc, Acc).
get_elements_by_name_x([Element|Elements], NodeName, Acc, Nodes) :-
  get_elements_by_name(Element, NodeName, Acc, TmpNodes),
  get_elements_by_name_x(Elements, NodeName, TmpNodes, Nodes).
