:- use_module(library(format)).
:- use_module('../../../src/lib/python').
:- initialization(main).

test_count(0).

increment_test :-
    retract(test_count(N)),
    N1 is N + 1,
    assertz(test_count(N1)).

pass(TestName) :-
    increment_test,
    format("✓ PASS: ~a~n", [TestName]).

fail_test(TestName, Error) :-
    increment_test,
    format("✗ FAIL: ~a - ~w~n", [TestName, Error]).

main :-
    format("=== Testing README.md Examples ===~n~n", []),
    assertz(test_count(0)),

    test_quick_start,
    test_simple_execution,
    test_globals_locals,
    test_dictionary_operations,
    test_dict_to_list,
    test_prolog_to_py_dict,

    test_count(Total),
    format("~n=== Total Tests Run: ~d ===~n", [Total]),
    halt.

test_quick_start :-
    format("Test: Quick Start Example~n", []),
    catch((
        py_initialize,
        py_run_simple_string('print("Hello from Python!")'),
        py_run_simple_string('x = 42'),
        py_run_simple_string('print(f"The answer is {x}")'),
        py_finalize,
        pass('Quick Start')
    ), Error, (
        (is_python_initialized -> py_finalize ; true),
        fail_test('Quick Start', Error)
    )).

test_simple_execution :-
    format("Test: Simple Code Execution~n", []),
    catch((
        py_initialize,
        py_run_simple_string('x = 10'),
        py_run_simple_string('print(x * 2)'),
        py_finalize,
        pass('Simple Execution')
    ), Error, (
        (is_python_initialized -> py_finalize ; true),
        fail_test('Simple Execution', Error)
    )).

test_globals_locals :-
    format("Test: Globals/Locals~n", []),
    catch((
        py_initialize,
        py_run_simple_string('result = x + y', [x-10, y-20], [], Globals, _),
        (member(result-30, Globals) ->
            pass('Globals/Locals')
        ;   fail_test('Globals/Locals', 'result-30 not in Globals')),
        py_finalize
    ), Error, (
        (is_python_initialized -> py_finalize ; true),
        fail_test('Globals/Locals', Error)
    )).

test_dictionary_operations :-
    format("Test: Dictionary Operations~n", []),
    catch((
        py_initialize,
        py_dict_new(Dict),
        py_dict_set(Dict, name, 'Alice'),
        py_dict_set(Dict, age, 30),
        py_dict_get(Dict, name, Name),
        (Name = 'Alice' ->
            pass('Dictionary Operations')
        ;   fail_test('Dictionary Operations', 'Name not Alice')),
        py_finalize
    ), Error, (
        (is_python_initialized -> py_finalize ; true),
        fail_test('Dictionary Operations', Error)
    )).

test_dict_to_list :-
    format("Test: Dict to List Conversion~n", []),
    catch((
        py_initialize,
        py_dict_new(Dict),
        py_dict_set(Dict, name, 'Alice'),
        py_dict_set(Dict, age, 30),
        py_dict_to_list(Dict, List),
        (member(name-'Alice', List), member(age-30, List) ->
            pass('Dict to List')
        ;   fail_test('Dict to List', 'Expected keys not in list')),
        py_finalize
    ), Error, (
        (is_python_initialized -> py_finalize ; true),
        fail_test('Dict to List', Error)
    )).

test_prolog_to_py_dict :-
    format("Test: Prolog to Python Dict~n", []),
    catch((
        py_initialize,
        prolog_to_py_dict([x-10, y-20], Dict),
        py_dict_get(Dict, x, X),
        (X = 10 ->
            pass('Prolog to Python Dict')
        ;   fail_test('Prolog to Python Dict', 'X not 10')),
        py_finalize
    ), Error, (
        (is_python_initialized -> py_finalize ; true),
        fail_test('Prolog to Python Dict', Error)
    )).
