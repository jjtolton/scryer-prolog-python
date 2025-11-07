:- use_module('../../src/lib/python').
:- use_module('../test_framework').
:- initialization(test_py_none).

test_py_none :-
    write('=== Testing Python None Support ==='), nl, nl,
    reset_test_state,

    % Phase 1.1.1: Check Python None singleton
    run_test('py_none/1 - Get None singleton', test_none_singleton),

    % Phase 1.1.2: Check if object is None
    run_test('py_none_check/1 - Verify object is None', test_none_check),

    % Phase 1.1.3: Prolog none → Python None
    run_test('prolog_value_to_pyobject/2 - Convert none to PyNone', test_prolog_to_none),

    % Phase 1.1.4: Python None → Prolog none
    run_test('pyobject_to_prolog_value/2 - Convert PyNone to none', test_none_to_prolog),

    % Phase 1.1.5: None in dict operations
    run_test('py_dict_set/get with none - Dict operations', test_none_in_dict),

    report_results,
    halt.

%% Test 1.1.1: Get None singleton
test_none_singleton :-
    py_initialize,
    py_none(PyNone1),
    py_none(PyNone2),
    % Both should be the same object (singleton)
    PyNone1 = PyNone2,
    py_xdecref(PyNone1),
    py_xdecref(PyNone2),
    py_finalize.

%% Test 1.1.2: Check if object is None
test_none_check :-
    py_initialize,
    py_none(PyNone),
    py_none_check(PyNone),
    py_xdecref(PyNone),
    py_finalize.

%% Test 1.1.3: Prolog none → Python None
test_prolog_to_none :-
    py_initialize,
    prolog_value_to_pyobject(none, PyNone),
    py_none_check(PyNone),
    py_xdecref(PyNone),
    py_finalize.

%% Test 1.1.4: Python None → Prolog none
test_none_to_prolog :-
    py_initialize,
    py_none(PyNone),
    pyobject_to_prolog_value(PyNone, Value),
    Value = none,
    py_xdecref(PyNone),
    py_finalize.

%% Test 1.1.5: None in dict operations
test_none_in_dict :-
    py_initialize,
    py_dict_new(Dict),
    py_dict_set(Dict, mykey, none),
    py_dict_get(Dict, mykey, DictValue),
    DictValue = none,
    py_xdecref(Dict),
    py_finalize.
