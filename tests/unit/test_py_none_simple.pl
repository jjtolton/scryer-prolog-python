:- use_module('../../src/lib/python').
:- initialization(test_py_none_simple).

test_py_none_simple :-
    write('=== Simple Python None Test ==='), nl, nl,

    % Test 1: Get None singleton
    write('Test 1: Get None singleton...'), nl,
    py_initialize,
    py_none(N1),
    write('  Got None: '), write(N1), nl,
    py_xdecref(N1),
    py_finalize,
    write('  PASS'), nl, nl,

    % Test 2: None check
    write('Test 2: None check...'), nl,
    py_initialize,
    py_none(N2),
    (py_none_check(N2) -> write('  PASS: None check succeeded'), nl ; write('  FAIL: None check failed'), nl),
    py_xdecref(N2),
    py_finalize,
    nl,

    % Test 3: Prolog none → Python None
    write('Test 3: Prolog none → Python None...'), nl,
    py_initialize,
    prolog_value_to_pyobject(none, PyNone),
    (py_none_check(PyNone) -> write('  PASS: Conversion succeeded'), nl ; write('  FAIL: Conversion failed'), nl),
    py_xdecref(PyNone),
    py_finalize,
    nl,

    % Test 4: Python None → Prolog none
    write('Test 4: Python None → Prolog none...'), nl,
    py_initialize,
    py_none(N4),
    pyobject_to_prolog_value(N4, Value),
    (Value = none -> write('  PASS: Got none'), nl ; write('  FAIL: Got '), write(Value), nl),
    py_xdecref(N4),
    py_finalize,
    nl,

    % Test 5: None in dict
    write('Test 5: None in dict...'), nl,
    py_initialize,
    py_dict_new(Dict),
    py_dict_set(Dict, mykey, none),
    py_dict_get(Dict, mykey, DictValue),
    (DictValue = none -> write('  PASS: Got none from dict'), nl ; write('  FAIL: Got '), write(DictValue), nl),
    py_xdecref(Dict),
    py_finalize,
    nl,

    write('=== All tests complete! ==='), nl,
    halt.
