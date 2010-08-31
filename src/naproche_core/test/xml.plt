:- begin_tests(xml, [setup((telling(CurrentOut), open('/dev/null', write, Out), tell(Out))),
                     cleanup(tell(CurrentOut))
                    ]).

% test loading files
test(load_missing, [fail]) :-
  xml_doc('test/data/not-there.xml', _).

test(load_not_well_formed, [fail]) :-
  xml_doc('test/data/not-well-formed.xml', _).

test(load_well_formed_without_dtd) :-
  xml_doc('test/data/well-formed.xml', _).

test(load_well_formed_not_valid, [fail]) :-
  xml_doc('test/data/well-formed.xml', _, 'test/data/allowed.dtd').

test(load_well_formed_valid) :-
  xml_doc('test/data/well-formed-valid.xml', _, 'test/data/allowed.dtd').

% test getting elements by name
test('Getting elements which do not exist should give back an empty list',
    [true(Blops == [])]) :-
  xml_doc('test/data/well-formed-valid.xml', Doc),
  get_elements_by_name(Doc, blop, Blops).

test('Getting a single <quote> element',
    [true(Quotes = [element(quote, _, _)])]) :-
  xml_doc('test/data/well-formed-valid.xml', Doc),
  get_elements_by_name(Doc, quote, Quotes).

test('Getting all four <par> elements',
    [true(length(Pars, 4))]) :-
  xml_doc('test/data/well-formed-valid.xml', Doc),
  get_elements_by_name(Doc, par, Pars).

test('Getting nested elements should presrve their order in the source file',
    [true(Pars =@= [element(par, [], ['The first paragraph.']),
                    element(par, [], ['The second paragraph.']),
                    element(par, [], ['The third paragraph.']),
                    element(par, [], ['The fourth paragraph.'])
                   ])]) :-
  xml_doc('test/data/well-formed-valid.xml', Doc),
  get_elements_by_name(Doc, par, Pars).

test('Getting nested elements in chained calls should presrve their order',
    [true(Pars =@= [ element(par, [], ['The second paragraph.']),
                    element(par, [], ['The third paragraph.'])
                   ])]) :-
  xml_doc('test/data/well-formed-valid.xml', Doc),
  get_elements_by_name(Doc, quote, Quotes),
  get_elements_by_name(Quotes, par, Pars).

:- end_tests(xml).
