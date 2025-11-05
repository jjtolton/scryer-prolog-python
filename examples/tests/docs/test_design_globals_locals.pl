:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module('../../../src/lib/python').
:- initialization(main).

:- dynamic(test_count/1).
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
    format("=== Testing DESIGN_GLOBALS_LOCALS.md Examples ===~n~n", []),
    assertz(test_count(0)),

    test_custom_globals,
    test_separate_locals,
    test_extract_value,

    test_count(Total),
    format("~n=== Total Tests Run: ~d ===~n", [Total]),
    halt.

test_custom_globals :-
    format("Test: Custom Globals~n", []),
    catch((
        py_initialize,
        py_run_simple_string('x = a + b', [a-5, b-10], [], NewGlobals, _),
        (member(x-15, NewGlobals) ->
            pass('Custom Globals')
        ;   fail_test('Custom Globals', 'x-15 not in NewGlobals')),
        py_finalize
    ), Error, (
        catch(py_finalize, _, true),
        fail_test('Custom Globals', Error)
    )).

test_separate_locals :-
    format("Test: Separate Locals~n", []),
    catch((
        py_initialize,
        py_run_simple_string('y = x * 2', [x-10], [x-5], _Globals, NewLocals),
        (member(y-10, NewLocals) ->
            pass('Separate Locals')
        ;   fail_test('Separate Locals', 'y-10 not in NewLocals')),
        py_finalize
    ), Error, (
        catch(py_finalize, _, true),
        fail_test('Separate Locals', Error)
    )).

test_extract_value :-
    format("Test: Extract Specific Value~n", []),
    catch((
        py_initialize,
        py_run_simple_string('result = 2 + 2', [], [], Globals, _),
        (member(result-4, Globals) ->
            pass('Extract Value')
        ;   fail_test('Extract Value', 'result-4 not found')),
        py_finalize
    ), Error, (
        catch(py_finalize, _, true),
        fail_test('Extract Value', Error)
    )).
