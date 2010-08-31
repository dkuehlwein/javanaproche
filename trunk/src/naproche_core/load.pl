% SWI standard library modules
:- use_module(library(pldoc)).
:- use_module(library(plunit)).
:- use_module(library(sgml)).
:- use_module(library(porter_stem)).
:- use_module(library(apply_macros)).
:- use_module(library(http/html_write)).
:- use_module(library(ugraphs)).

% set up the search path and namespace for the naproche modules
user:file_search_path(naproche, 'lib').
user:file_search_path(naproche, 'data').
user:file_search_path(naproche, 'data/grammar').
user:file_search_path(naproche, 'data/lexicon').
user:file_search_path(naproche, 'src').
user:file_search_path(naproche, 'src/texmacs').
user:file_search_path(naproche, 'src/prs').
user:file_search_path(naproche, 'src/logic').
user:file_search_path(naproche, 'src/input').
user:file_search_path(naproche, 'www').

% third party external modules
:- ensure_loaded(naproche(gulp4swi)).
:- use_module(naproche(error_logger)).

% Here are the gulp features used in the program:
g_features([
% Non-semantic linguistic features:
number,noun_type,mode,transitive,adj_trans,specifier_type,subordinated,comma,
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

% a number of small utility predicates
:- ensure_loaded(naproche(utils)).
:- ensure_loaded(naproche(development_utils)).

% Naproche modules
:- use_module(naproche(input_utils)).
:- use_module(naproche(input_parser)).
:- use_module(naproche(xml)).
:- use_module(naproche(texmacs)).
:- use_module(naproche(prs)).
:- use_module(naproche(prs_export)).
:- use_module(naproche(create_obligations)).
:- use_module(naproche(discharge_obligations)).
:- use_module(naproche(graph)).
:- use_module(naproche(stats)).
:- use_module(naproche(output)).

% grammars
:- use_module(naproche(fo_grammar)).
:- ensure_loaded(naproche(dcg)).
:- ensure_loaded(naproche(dcg_simple)).

% lexicon
:- use_module(naproche(latex_lexicon)).
:- use_module(naproche(math_lexicon)).
:- use_module(naproche(dcg_lexicon)).

% local settings
:- ensure_loaded(user).

% Prolog Flags
% :- set_prolog_flag(character_escapes, false).

% generic settings
% Change to logger(log) to enable logging.
logger(nolog).

%% load_naproche_tests is det
% 
% Goes through all Naproche source files and loads the corresponding test files.
%
% "Corresponding" is defined on the file paths. For a test file to be
% automatically loaded its path within the =|test/|= directory must correspond with
% the path of the to-be-tested file in the =|lib/|= directory. E.g.:
%
%==
% lib/foo.pl
% test/foo.plt
%
% ...
%
% lib/foo/bar.pl
% test/foo/bar.plt
%==
%
% Executed automatically at startup.
user:load_naproche_tests :-
  working_directory(CWD, CWD),
  (
    source_file(File),
    atom_concat(CWD, InternalProjectPath, File),

    (
    atom_concat('lib', InternalPath, InternalProjectPath);
    atom_concat('data', InternalPath, InternalProjectPath);
    atom_concat('src', InternalPath, InternalProjectPath);
    atom_concat('www', InternalPath, InternalProjectPath)
    ),
    file_name_extension(Base, pl, InternalPath),

    atom_concat('test', Base, InternalTestPath),
    file_name_extension(InternalTestPath, plt, TestFile),

    exists_file(TestFile),
    load_files(TestFile, [ if(changed), imports([]) ]),

    fail ; true
  ).


% Load test files by default, so that we can issue run_tests/0 the moment we
% enter the environment.
:- load_naproche_tests.

% automatically start the docuementation server
:- doc_server(8000,[allow(ip(131,220,_,_)),allow(ip(127,0,0,1))]).

