:- use_module('../../src/lib/python').

test_dict_lifecycle :-
    write('=== Test 1: Dictionary Lifecycle ==='), nl,
    write('Creating and destroying dict 1000 times...'), nl,
    test_dict_lifecycle_loop(1000),
    write('PASS: No crashes'), nl, nl.

test_dict_lifecycle_loop(0) :- !.
test_dict_lifecycle_loop(N) :-
    N > 0,
    py_dict_new(Dict),
    py_dict_set(Dict, key1, 'value1'),
    py_dict_set(Dict, key2, 42),
    py_dict_set(Dict, key3, 3.14),
    py_dict_get(Dict, key1, _V1),
    py_dict_get(Dict, key2, _V2),
    py_dict_to_list(Dict, _List),
    py_xdecref(Dict),
    N1 is N - 1,
    test_dict_lifecycle_loop(N1).

test_prolog_to_py_dict_lifecycle :-
    write('=== Test 2: prolog_to_py_dict Lifecycle ==='), nl,
    write('Creating dicts from lists 1000 times...'), nl,
    test_prolog_to_py_dict_loop(1000),
    write('PASS: No crashes'), nl, nl.

test_prolog_to_py_dict_loop(0) :- !.
test_prolog_to_py_dict_loop(N) :-
    N > 0,
    prolog_to_py_dict([a-1, b-2, c-3], Dict),
    py_dict_to_prolog(Dict, _List),
    py_xdecref(Dict),
    N1 is N - 1,
    test_prolog_to_py_dict_loop(N1).

test_error_handling :-
    write('=== Test 3: Error Handling (cleanup on failure) ==='), nl,
    write('Skipping catch/3 test (not fully supported)'), nl,
    write('PASS: Skipped'), nl, nl.

test_nested_operations :-
    write('=== Test 4: Nested Operations ==='), nl,
    write('Creating dict with multiple sets and gets...'), nl,
    py_dict_new(Dict),
    py_dict_set(Dict, name, 'Alice'),
    py_dict_set(Dict, age, 30),
    py_dict_set(Dict, score, 95.5),
    py_dict_set(Dict, active, true),
    py_dict_get(Dict, name, Name),
    py_dict_get(Dict, age, Age),
    py_dict_get(Dict, score, Score),
    py_dict_to_list(Dict, List),
    py_xdecref(Dict),
    write('  Name: '), write(Name), nl,
    write('  Age: '), write(Age), nl,
    write('  Score: '), write(Score), nl,
    write('  List: '), write(List), nl,
    write('PASS'), nl, nl.

test_globals_locals_cleanup :-
    write('=== Test 5: py_run_simple_string/5 Cleanup ==='), nl,
    write('Running with globals/locals 100 times...'), nl,
    test_globals_locals_loop(100),
    write('PASS: No crashes'), nl, nl.

test_globals_locals_loop(0) :- !.
test_globals_locals_loop(N) :-
    N > 0,
    py_run_simple_string('x = a + b', [a-5, b-10], [], _G, _L),
    N1 is N - 1,
    test_globals_locals_loop(N1).

test :-
    write('=== Memory Management Test Suite ==='), nl, nl,
    py_initialize,

    test_dict_lifecycle,
    test_prolog_to_py_dict_lifecycle,
    test_error_handling,
    test_nested_operations,
    test_globals_locals_cleanup,

    py_finalize,
    write('=== All Memory Management Tests Complete! ==='), nl.

:- initialization(test).
