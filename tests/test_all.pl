:- initialization(run_all_tests).

run_all_tests :-
    write('=== Running All Phase 1 Tests ==='), nl, nl,

    % Unit tests
    write('[1/6] Python None tests...'), nl,
    consult('unit/test_py_none_simple.pl'),
    nl,

    write('[2/6] Python List tests...'), nl,
    consult('unit/test_py_list_simple.pl'),
    nl,

    write('[3/6] Python Tuple tests...'), nl,
    consult('unit/test_py_tuple_simple.pl'),
    nl,

    write('[4/6] String vs Atom argument tests...'), nl,
    consult('unit/test_string_vs_atom_args.pl'),
    nl,

    write('[5/6] Config validation tests...'), nl,
    consult('unit/test_config_validation.pl'),
    nl,

    write('[6/6] Phase 1 integration tests...'), nl,
    consult('integration/test_phase1_types.pl'),
    nl,

    write('=== All Phase 1 Tests Complete ==='), nl,
    halt.
