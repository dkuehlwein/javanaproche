#!/usr/local/bin/pl -L128m -G128m -T128m -g main -s

% Library modules
:- use_module(library(cgi)).
:- use_module(library(http/html_write)).
:- use_module(library(ugraphs)).

% set up the search path and namespace for the naproche modules
user:file_search_path(naproche, '../../lib').
user:file_search_path(naproche, '../../data').
user:file_search_path(naproche, '../../data/grammar').
user:file_search_path(naproche, '../../data/lexicon').
user:file_search_path(naproche, '../../src').
user:file_search_path(naproche, '../../src/prs').
user:file_search_path(naproche, '../../src/logic').
user:file_search_path(naproche, '../../src/input').

% third party external modules
:- ensure_loaded(naproche(gulp4swi)).

% Here are the gulp features used in the program:
g_features([
% Non-semantic linguistic features:
number,noun_type,mode,transitive,adj_trans,specifier_type,subordinated,comma,alph,cap,
% Properties of text, naproche_text and cases:
initial_text,empty,all_ass_closed,type,subtype,
% Features influencing the PRS creation:
add_dref,math_id,dref_list,noun,grouped,
% PRS slots:
id,drefs,mrefs,conds,rrefs,accbefore,accafter,dref_cond_links,imagined_accbefore,
% fo_grammar ("type" already mentioned before):
name,dref,arity,args,
% logic module
distance,used,result,premises,contradiction,the]).

% Naproche modules
:- use_module(naproche(prs)).
:- use_module(naproche(error_logger)).
:- use_module(naproche(prs_export)).
:- use_module(naproche(latex_lexicon)).
:- use_module(naproche(create_obligations)).
:- use_module(naproche(discharge_obligations)).
:- use_module(naproche(utils)).
:- use_module(naproche(input_utils)).
:- use_module(naproche(graph)).

:- ensure_loaded(user).

% Dynamic Predicates
:- dynamic sentence_index/1.
:- dynamic check_time/1.
:- dynamic check_prover/1.
:- dynamic check_size/1.
:- dynamic check_place/1.
:- dynamic session_id/1.
:- dynamic ostream/1.
:- dynamic submit/1.

% Prolog Flags
%:- set_prolog_flag(character_escapes, false).

main([FileName]) :-
	
	% Settings
	setval(refids,[]),

	open(FileName,read,IS),
	read_arguments(IS,Input),
	close(IS),

	asserta(ostream('HTML')),	
	submit(Submit),
%	set_arguments(Id,Prover,Time,OutputLvl,Location,'HTML'),

	% Tidy up
	clear_messages,
%	clean,

	% Try to compute, else write errors 
	process_input(Input,Submit);
	(
		get_messages(Messages),
		print_list_html(Messages)
	).

read_arguments(IS,Input) :-
	read(IS,Term),
	read_input(IS,Term,Input),
	read(IS,Term2),
	read_set_settings(IS,Term2).


read_input(IS,sentence(Id,Content),[sentence(Id,Content)|Rest] ) :-
	read(IS,Term),
	!,
	read_input(IS,Term,Rest).

read_input(_IS,X,[]) :-
	asserta(X).

read_set_settings(_IS,end_of_file).

read_set_settings(IS,Term) :-
	asserta(Term),
	read(IS,Term2),
	!,
	read_set_settings(IS,Term2).

/*
read_stream(IS,AtomIn,AtomOut) :-
	get_char(IS,Char),
	write(Char),
	( Char = '#' ->
		AtomOut=AtomIn;
		(
		atom_concat(AtomIn,Char,AtomTmp),
		read_stream(IS,AtomTmp,AtomOut)
		)
	).
*/

%%	set_arguments(+Prover:Atom,+Time:Atom,+Output:Atom,+Location:Atom,+OStream:Atom)
%
%	Sets all the Arguments that will be used in the checking process, 
%	namely as arguments for SystemsOnTPTP, which in turn calls the ATP.

set_arguments(Id,Prover, Time, OutputLvl, Location, OStream) :-
	assertz(session_id(Id)),
	assertz(check_time(Time)),
	assertz(check_prover(Prover)),
	assertz(check_size(OutputLvl)),
	% Check_place is local or extern
	assertz(check_place(Location)),
	% OStream
	assertz(ostream(OStream)).



%%      print_list_html(+List)
%
%       Print a list as html.

print_list_html(List) :-
        format('[ <br/>',[]),
        print_list_html_main(List).

print_list_html_main([Head|[]]) :-
        print_html([Head]),
        format('<br/> ]',[]).

print_list_html_main([Head|Tail]) :-
        print_html([Head]),
        format(', <br/>',[]),
        print_list_html_main(Tail).


