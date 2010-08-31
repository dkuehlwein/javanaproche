% samples for testing
sample_prs(empty,
           'test/data/empty-prs.xml',
           id~container..
           drefs~[]..
           mrefs~[]..
           conds~[]..
           rrefs~[]).

% x in emptyset
sample_prs(statement,
           'test/data/statement-prs.xml', PRS) :-
           PRS = id~1..
             drefs~[2,3]..
             mrefs~[type~variable..name~x..arity~0..args~[], type~relation..name~in..arity~2..args~[type~variable..name~x..arity~0,type~constant..name~emptyset..arity~0..args~[]]]..
             conds~[
	     		math_id(2, type~variable..name~x..arity~0..args~[]), 
			math_id(3, type~relation..name~in..arity~2..args~[type~variable..name~x..arity~0..args~[],type~constant..name~emptyset..arity~0..args~[]]), 
			holds(3)
		]..
             rrefs~[].

% like statement PRS, but the 'main' PRS is in another 'container' PRS
% (i.e. we have another PRS as condition)
sample_prs(statement2,
           'test/data/statement-prs.xml',
           PRS) :-
	   PRS = id~container..
           drefs~[]..
           mrefs~[]..
           conds~[
             id~1..
             drefs~[2,3]..
             mrefs~[type~variable..name~x..arity~0, type~relation..name~in..arity~2..args~[type~variable..name~x..arity~0,type~constant..name~emptyset..arity~0]]..
             conds~[
	     		math_id(2, type~variable..name~x..arity~0), 
			math_id(3, type~relation..name~in..arity~2..args~[type~variable..name~x..arity~0,type~constant..name~emptyset..arity~0]), 
			holds(3)
		]..
             rrefs~[]
	   ]..
           rrefs~[].

sample_prs(implication,
           'test/data/implication-prs.xml',
           id~container..
           drefs~[]..
           mrefs~[]..
           conds~[
             id~1..
             drefs~[2,3]..
             mrefs~[type~variable..name~x..arity~0..args~[], type~relation..name~in..arity~2..args~[type~variable..name~x..arity~0..args~[],type~constant..name~emptyset..arity~0..args~[]]]..
             conds~[
	     		math_id(2, type~variable..name~x..arity~0..args~[]), 
			math_id(3, type~relation..name~in..arity~2..args~[type~variable..name~x..arity~0..args~[],type~constant..name~emptyset..arity~0..args~[]]), 
			holds(3)
		]..
             rrefs~[]

             =>

             id~4..
             drefs~[]..
             mrefs~[]..
             conds~[]..
             rrefs~[]

           ]..
           rrefs~[]).

%TAG
% Definition: R(x) := x<0
sample_prs(definition,
           'test/data/definition-prs.xml',
           id~container..
           drefs~[]..
           mrefs~[]..
           conds~[
             id~1..
             drefs~[2,3]..
             mrefs~[type~variable..name~x..arity~0..args~[], type~relation..name~R..arity~1..args~[type~variable..name~x..arity~0..args~[]]]..
             conds~[math_id(2, type~variable..name~x..arity~0..args~[]), math_id(3, type~relation..name~R..arity~1..args~[type~variable..name~x..arity~0..args~[]]), holds(3)]..
             rrefs~[]

             :=

             id~4..
             drefs~[5]..
             mrefs~[type~relation..name~less..arity~2..args~[type~variable..name~x..arity~0..args~[], type~constant..name~'0'..arity~0..args~[]]]..
             conds~[math_id(5, type~relation..name~less..arity~2..args~[type~variable..name~x..arity~0..args~[], type~constant..name~'0'..arity~0..args~[]]), holds(5)]..
             rrefs~[]

           ]..
           rrefs~[]).

create_tmp_file(File, Stream) :-
  tmp_file(plunit, File),
  open(File, write, Stream).

:- begin_tests(prs_export).

% --------------- simple tests ----------------------------
test('prs_export: one variable') :-
	GULP = type~variable..arity~0..name~x..args~[],
	XML = element(mref,[],[element(name,[],[x]), element(type, [], [variable]), element(arity, [], ['0']), element(args, [], [])]),
	prs_export:dobsod_to_xml(mref, GULP, XMLFree),
	prs_export:dobsod_to_xml(mref, GULPFree, XML),
	GULP = GULPFree,
	XML = XMLFree.	



test('prs_export: function f(x)') :-
	GULP = type~function..name~f..arity~1..args~[type~variable..arity~0..name~x..args~[]],
	XML = element(mref, [], [
		element(name, [], [f]), 
		element(type, [], [function]), 
		element(arity, [], ['1']), 
		element(args, [], [
			element(arg, [], [element(name, [], [x]), 
			element(type, [], [variable]), 
			element(arity, [], ['0']),
			element(args, [], [])
			])
		])
	     ]),
	prs_export:dobsod_to_xml(mref, GULP, XMLFree),
	prs_export:dobsod_to_xml(mref, GULPFree, XML),
	GULP = GULPFree,
	XML = XMLFree.


% --------------- PRS tests ------------------------------
% in memory (de)serialisation
test('Deserialising and serialising empty PRS in memory',
    [true(PRS =@= NewPRS)]) :-
  sample_prs(empty, _, PRS),
  prs_export:prs_to_xml(PRS, XML),
  prs_export:prs_to_xml(NewPRS, XML).

test('Deserialising and serialising statement PRS in memory',
    [true(PRS =@= NewPRS)]) :-
  sample_prs(statement, _, PRS),
  prs_export:prs_to_xml(PRS, XML),
  prs_export:prs_to_xml(NewPRS, XML),
  PRS = NewPRS.

test('Deserialising and serialising statement PRS in memory II',
    [true(PRS =@= NewPRS)]) :-
  sample_prs(statement2, _, PRS),
  prs_export:prs_to_xml(PRS, XML),
  prs_export:prs_to_xml(NewPRS, XML),
  PRS = NewPRS.


test('Deserialising and serialising assumption PRS in memory',
    [true(PRS =@= NewPRS)]) :-
  sample_prs(implication, _, PRS),
  prs_export:prs_to_xml(PRS, XML),
  prs_export:prs_to_xml(NewPRS, XML).


%TAG
test('Deserialising and serialising definition PRS in memory',
    [true(PRS =@= NewPRS)]) :-
  sample_prs(definition, _, PRS),
  prs_export:prs_to_xml(PRS, XML),
  prs_export:prs_to_xml(NewPRS, XML).


% on disk (de)serialisation
test('Deserialising and serialising empty PRS on disk',
    [setup(create_tmp_file(File, Stream)),
     cleanup(delete_file(File)),
     true(PRS =@= NewPRS)]) :-
  sample_prs(empty, _, PRS),
  prs_export:prs_to_xml(PRS, XML),
  xml_write(Stream, XML, []), close(Stream),
  xml_doc(File, NewXML),
  prs_export:prs_to_xml(NewPRS, NewXML).

test('Deserialising and serialising statement PRS on disk',
    [setup(create_tmp_file(File, Stream)),
     cleanup(delete_file(File)),
     true(PRS =@= NewPRS)]) :-
  sample_prs(statement, _, PRS),
  prs_export:prs_to_xml(PRS, XML),
  xml_write(Stream, XML, []), close(Stream),
  xml_doc(File, NewXML),
  prs_export:prs_to_xml(NewPRS, NewXML).

test('Deserialising and serialising implication PRS on disk',
    [setup(create_tmp_file(File, Stream)),
     cleanup(delete_file(File)),
     true(PRS =@= NewPRS)]) :-
  sample_prs(implication, _, PRS),
  prs_export:prs_to_xml(PRS, XML),
  xml_write(Stream, XML, []), close(Stream),
  xml_doc(File, NewXML),
  prs_export:prs_to_xml(NewPRS, NewXML).

test('Deserialising and serialising definition PRS on disk',
    [setup(create_tmp_file(File, Stream)),
     cleanup(delete_file(File)),
     true(PRS =@= NewPRS)]) :-
  sample_prs(definition, _, PRS),
  prs_export:prs_to_xml(PRS, XML),
  xml_write(Stream, XML, []), close(Stream),
  xml_doc(File, NewXML),
  prs_export:prs_to_xml(NewPRS, NewXML).

/*
% equivalence of sample files
test('Empty PRS should be equivalent to its disk representation',
    [true(SamplePRS =@= PRSFromFile)]) :-
  sample_prs(empty, File, SamplePRS),
  xml_doc(File, XML),
  prs_export:prs_to_xml(PRSFromFile, XML).

test('Statement PRS should be equivalent to its disk representation',
    [true(SamplePRS =@= PRSFromFile)]) :-
  sample_prs(statement, File, SamplePRS),
  xml_doc(File, XML),
  prs_export:prs_to_xml(PRSFromFile, XML).

test('Implication PRS should be equivalent to its disk representation',
    [true(SamplePRS =@= PRSFromFile)]) :-
  sample_prs(implication, File, SamplePRS),
  xml_doc(File, XML),
  prs_export:prs_to_xml(PRSFromFile, XML).

test('Definition PRS should be equivalent to its disk representation',
    [true(SamplePRS =@= PRSFromFile)]) :-
  sample_prs(definition, File, SamplePRS),
  xml_doc(File, XML),
  prs_export:prs_to_xml(PRSFromFile, XML).
*/

:- end_tests(prs_export).


