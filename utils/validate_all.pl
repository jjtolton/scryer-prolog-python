:- use_module('module_inspector').
:- use_module('doc_parser').
:- use_module('test_coverage').
:- use_module('config_validator').
:- use_module(library(lists)).
:- initialization(main).

main :-
    write('=== Documentation Validation System ==='), nl, nl,

    % 1. Get public predicates
    write('Step 1: Extracting public predicates...'), nl,
    get_public_predicates("src/lib/python.pl", PublicPreds),
    length(PublicPreds, NumPreds),
    write('  Found '), write(NumPreds), write(' public predicates'), nl, nl,

    % 2. Check test coverage for string argument enforcement
    write('Step 2: Checking test coverage for string argument enforcement...'), nl,
    check_string_arg_coverage(CoverageReport),
    (   CoverageReport = complete
    ->  write('  ✓ All predicates with string arguments have type enforcement tests'), nl
    ;   CoverageReport = missing(Missing),
        write('  ✗ Missing tests for: '), write(Missing), nl
    ),
    nl,

    % 3. Validate config example file
    write('Step 3: Validating python.pl.example...'), nl,
    validate_config("python.pl.example"),
    nl,

    % 4. Summary
    write('=== Validation Summary ==='), nl,
    (   CoverageReport = complete
    ->  write('✓ All validation checks passed!'), nl,
        halt(0)
    ;   write('✗ Some validation checks failed'), nl,
        halt(1)
    ).
