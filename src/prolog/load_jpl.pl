% set up the search path and namespace for the naproche modules
user:file_search_path(naproche, 'prolog/input').
user:file_search_path(naproche, 'prolog/lib').

% Naproche modules
:- use_module(naproche(input_parser)).

% third party external modules
:- use_module(naproche(error_logger)).

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
    atom_concat('src', InternalPath, InternalProjectPath)
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
