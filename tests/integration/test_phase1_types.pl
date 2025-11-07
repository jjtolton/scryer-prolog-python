:- use_module('../../src/lib/python').
:- initialization(test_phase1_types).

test_phase1_types :-
    write('=== Phase 1 Integration Test: None + Lists + Tuples ==='), nl, nl,

    % Test 1: Mixed type list with None
    write('Test 1: List with None values...'), nl,
    py_initialize,
    py_list_from_prolog([1, none, 3], List1),
    py_list_to_prolog(List1, Result1),
    write('  Result: '), write(Result1), nl,
    (Result1 = [1, none, 3] -> write('  PASS'), nl ; write('  FAIL'), nl),
    py_xdecref(List1),
    py_finalize,
    nl,

    % Test 2: Mixed type tuple with None
    write('Test 2: Tuple with None values...'), nl,
    py_initialize,
    py_tuple_from_prolog([none, 2, none], Tuple2),
    py_tuple_to_prolog(Tuple2, Result2),
    write('  Result: '), write(Result2), nl,
    (Result2 = [none, 2, none] -> write('  PASS'), nl ; write('  FAIL'), nl),
    py_xdecref(Tuple2),
    py_finalize,
    nl,

    % Test 3: Nested list structures
    write('Test 3: Nested lists...'), nl,
    py_initialize,
    Original3 = [[1, 2], [3, 4]],
    py_list_from_prolog(Original3, List3),
    py_list_to_prolog(List3, Result3),
    write('  Original: '), write(Original3), nl,
    write('  Result:   '), write(Result3), nl,
    (Result3 = Original3 -> write('  PASS'), nl ; write('  FAIL'), nl),
    py_xdecref(List3),
    py_finalize,
    nl,

    % Test 4: Complex nested structure
    write('Test 4: Complex nested structure...'), nl,
    py_initialize,
    Original4 = [1, [2, none, 3], none, [4, 5]],
    py_list_from_prolog(Original4, List4),
    py_list_to_prolog(List4, Result4),
    write('  Original: '), write(Original4), nl,
    write('  Result:   '), write(Result4), nl,
    (Result4 = Original4 -> write('  PASS'), nl ; write('  FAIL'), nl),
    py_xdecref(List4),
    py_finalize,
    nl,

    % Test 5: All types together in tuple
    write('Test 5: Tuple with all Phase 1 types...'), nl,
    py_initialize,
    Original5 = [none, 42, [1, 2, 3]],
    py_tuple_from_prolog(Original5, Tuple5),
    py_tuple_to_prolog(Tuple5, Result5),
    write('  Original: '), write(Original5), nl,
    write('  Result:   '), write(Result5), nl,
    (Result5 = Original5 -> write('  PASS'), nl ; write('  FAIL'), nl),
    py_xdecref(Tuple5),
    py_finalize,
    nl,

    write('=== All integration tests complete! ==='), nl,
    halt.
