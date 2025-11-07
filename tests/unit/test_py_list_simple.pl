:- use_module('../../src/lib/python').
:- initialization(test_py_list_simple).

test_py_list_simple :-
    write('=== Simple Python List Test ==='), nl, nl,

    % Test 1: Create empty list
    write('Test 1: Create empty list...'), nl,
    py_initialize,
    py_list_new(List1),
    py_list_size(List1, Size1),
    write('  Size: '), write(Size1), nl,
    (Size1 = 0 -> write('  PASS'), nl ; write('  FAIL'), nl),
    py_xdecref(List1),
    py_finalize,
    nl,

    % Test 2: Create list from Prolog list
    write('Test 2: Create list from Prolog list...'), nl,
    py_initialize,
    py_list_from_prolog([1, 2, 3], PyList),
    py_list_size(PyList, Size2),
    write('  Size: '), write(Size2), nl,
    (Size2 = 3 -> write('  PASS'), nl ; write('  FAIL'), nl),
    py_xdecref(PyList),
    py_finalize,
    nl,

    % Test 3: Get item by index
    write('Test 3: Get item by index...'), nl,
    py_initialize,
    py_list_from_prolog([10, 20, 30], List3),
    py_list_get(List3, 0, First),
    py_list_get(List3, 1, Second),
    py_list_get(List3, 2, Third),
    write('  Items: '), write(First), write(', '), write(Second), write(', '), write(Third), nl,
    (First = 10, Second = 20, Third = 30 -> write('  PASS'), nl ; write('  FAIL'), nl),
    py_xdecref(List3),
    py_finalize,
    nl,

    % Test 4: Set item by index
    write('Test 4: Set item by index...'), nl,
    py_initialize,
    py_list_from_prolog([1, 2, 3], List4),
    py_list_set(List4, 1, 99),
    py_list_get(List4, 1, Value),
    write('  New value at index 1: '), write(Value), nl,
    (Value = 99 -> write('  PASS'), nl ; write('  FAIL'), nl),
    py_xdecref(List4),
    py_finalize,
    nl,

    % Test 5: Append to list
    write('Test 5: Append to list...'), nl,
    py_initialize,
    py_list_new(List5),
    py_list_append(List5, 10),
    py_list_append(List5, 20),
    py_list_append(List5, 30),
    py_list_size(List5, Size5),
    py_list_get(List5, 2, Last),
    write('  Size after appends: '), write(Size5), nl,
    write('  Last item: '), write(Last), nl,
    (Size5 = 3, Last = 30 -> write('  PASS'), nl ; write('  FAIL'), nl),
    py_xdecref(List5),
    py_finalize,
    nl,

    % Test 6: Convert list to Prolog
    write('Test 6: Convert list to Prolog...'), nl,
    py_initialize,
    py_list_from_prolog([1, 2, 3], List6),
    py_list_to_prolog(List6, Result),
    write('  Result: '), write(Result), nl,
    (Result = [1, 2, 3] -> write('  PASS'), nl ; write('  FAIL'), nl),
    py_xdecref(List6),
    py_finalize,
    nl,

    % Test 7: Round-trip conversion
    write('Test 7: Round-trip conversion...'), nl,
    py_initialize,
    Original = [1, 2, 3, 4, 5],
    py_list_from_prolog(Original, List7),
    py_list_to_prolog(List7, RoundTrip),
    write('  Original: '), write(Original), nl,
    write('  Result:   '), write(RoundTrip), nl,
    (RoundTrip = Original -> write('  PASS'), nl ; write('  FAIL'), nl),
    py_xdecref(List7),
    py_finalize,
    nl,

    write('=== All tests complete! ==='), nl,
    halt.
