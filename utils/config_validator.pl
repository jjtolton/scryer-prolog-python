:- module(config_validator, [
    validate_config/1,
    validate_config_strict/0
]).

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(files)).

%% validate_config(+ConfigFile)
%
% Read configuration file and validate that path predicates use strings not atoms.
% Checks:
%   - python_library_path_user/1 arg must be string (char list)
%   - python_home/1 arg must be string
%   - python_executable/1 arg must be string
%
% @param ConfigFile Path to python.pl or python.pl.example
%
validate_config(ConfigFile) :-
    (   file_exists(ConfigFile)
    ->  open(ConfigFile, read, Stream),
        validate_config_terms(Stream, Errors),
        close(Stream),
        report_config_errors(ConfigFile, Errors)
    ;   write('Config file not found: '), write(ConfigFile), nl
    ).

%% validate_config_terms(+Stream, -Errors)
%
% Read terms from config file and collect validation errors.
%
validate_config_terms(Stream, Errors) :-
    catch(read_term(Stream, Term, []), _, fail),
    !,
    (   Term = end_of_file
    ->  Errors = []
    ;   check_config_term(Term, Error),
        validate_config_terms(Stream, RestErrors),
        (   Error = []
        ->  Errors = RestErrors
        ;   Errors = [Error|RestErrors]
        )
    ).
validate_config_terms(_, []).

%% check_config_term(+Term, -Error)
%
% Check if a configuration term uses correct types.
%
check_config_term(python_library_path_user(Path), Error) :- !,
    (   atom(Path)
    ->  Error = error(python_library_path_user/1, 'Path must be string "..." not atom \'...\'', Path)
    ;   Error = []
    ).
check_config_term(python_home(Path), Error) :- !,
    (   atom(Path)
    ->  Error = error(python_home/1, 'Path must be string "..." not atom \'...\'', Path)
    ;   Error = []
    ).
check_config_term(python_executable(Path), Error) :- !,
    (   atom(Path)
    ->  Error = error(python_executable/1, 'Path must be string "..." not atom \'...\'', Path)
    ;   Error = []
    ).
check_config_term(_, []).

%% report_config_errors(+ConfigFile, +Errors)
%
% Print validation errors or success message.
%
report_config_errors(ConfigFile, []) :- !,
    write('✓ Config file valid: '), write(ConfigFile), nl.
report_config_errors(ConfigFile, Errors) :-
    write('✗ Config validation errors in '), write(ConfigFile), write(':'), nl,
    maplist(print_config_error, Errors).

print_config_error(error(Predicate, Message, Value)) :-
    write('  - '), write(Predicate), write(': '), write(Message), nl,
    write('    Got: '), write(Value), nl.

%% validate_config_strict/0
%
% Called during py_initialize/0 to validate user config.
% Throws error if config uses atoms instead of strings.
%
% NOTE: This would be integrated into src/lib/python.pl
%
validate_config_strict :-
    % Check if user has a python.pl config file
    (   file_exists("python.pl")
    ->  % Validate it
        open("python.pl", read, Stream),
        validate_config_terms(Stream, Errors),
        close(Stream),
        (   Errors = []
        ->  true  % Config is valid
        ;   % Config has errors - throw helpful message
            throw(error(syntax_error(config_file),
                context(validate_config_strict/0,
                    'python.pl configuration error: Paths must use double quotes (strings) not single quotes (atoms). See python.pl.example for correct syntax.')))
        )
    ;   true  % No config file, that's OK
    ).
