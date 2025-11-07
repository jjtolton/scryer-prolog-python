:- use_module('../../src/lib/python').
:- initialization(test_py_tuple_simple).

test_py_tuple_simple :-
    write('=== Simple Python Tuple Test ==='), nl, nl,

    % Test 1: Create tuple from Prolog list
    write('Test 1: Create tuple from Prolog list...'), nl,
    py_initialize,
    py_tuple_from_prolog([1, 2, 3], PyTuple1),
    py_tuple_size(PyTuple1, Size1),
    write('  Size: '), write(Size1), nl,
    (Size1 = 3 -> write('  PASS'), nl ; write('  FAIL'), nl),
    py_xdecref(PyTuple1),
    py_finalize,
    nl,

    % Test 2: Create empty tuple
    write('Test 2: Create empty tuple...'), nl,
    py_initialize,
    py_tuple_new(0, Tuple2),
    py_tuple_size(Tuple2, Size2),
    write('  Size: '), write(Size2), nl,
    (Size2 = 0 -> write('  PASS'), nl ; write('  FAIL'), nl),
    py_xdecref(Tuple2),
    py_finalize,
    nl,

    % Test 3: Get item by index
    write('Test 3: Get item by index...'), nl,
    py_initialize,
    py_tuple_from_prolog([10, 20, 30], Tuple3),
    py_tuple_get(Tuple3, 0, First),
    py_tuple_get(Tuple3, 1, Second),
    py_tuple_get(Tuple3, 2, Third),
    write('  Items: '), write(First), write(', '), write(Second), write(', '), write(Third), nl,
    (First = 10, Second = 20, Third = 30 -> write('  PASS'), nl ; write('  FAIL'), nl),
    py_xdecref(Tuple3),
    py_finalize,
    nl,

    % Test 4: Convert tuple to Prolog
    write('Test 4: Convert tuple to Prolog...'), nl,
    py_initialize,
    py_tuple_from_prolog([1, 2, 3], Tuple4),
    py_tuple_to_prolog(Tuple4, Result4),
    write('  Result: '), write(Result4), nl,
    (Result4 = [1, 2, 3] -> write('  PASS'), nl ; write('  FAIL'), nl),
    py_xdecref(Tuple4),
    py_finalize,
    nl,

    % Test 5: Round-trip conversion
    write('Test 5: Round-trip conversion...'), nl,
    py_initialize,
    Original = [1, 2, 3, 4, 5],
    py_tuple_from_prolog(Original, Tuple5),
    py_tuple_to_prolog(Tuple5, RoundTrip),
    write('  Original: '), write(Original), nl,
    write('  Result:   '), write(RoundTrip), nl,
    (RoundTrip = Original -> write('  PASS'), nl ; write('  FAIL'), nl),
    py_xdecref(Tuple5),
    py_finalize,
    nl,

    % Test 6: Tuple vs list distinction
    write('Test 6: Tuple vs list are different types...'), nl,
    py_initialize,
    py_list_from_prolog([1, 2, 3], List6),
    py_tuple_from_prolog([1, 2, 3], Tuple6),
    write('  List ptr:  '), write(List6), nl,
    write('  Tuple ptr: '), write(Tuple6), nl,
    (List6 \= Tuple6 -> write('  PASS: Different objects'), nl ; write('  FAIL: Same object'), nl),
    py_xdecref(List6),
    py_xdecref(Tuple6),
    py_finalize,
    nl,

    write('=== All tests complete! ==='), nl,
    halt.
