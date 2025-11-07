:- use_module('../../src/lib/python').
:- use_module(library(lists)).
:- initialization(test_string_vs_atom_args).

test_string_vs_atom_args :-
    write('=== Testing String vs Atom Arguments ==='), nl, nl,

    % Test 1: py_run_simple_string with atom should fail
    write('Test 1: py_run_simple_string with atom (should fail)...'), nl,
    py_initialize,
    catch(
        (py_run_simple_string('print("test")'),
         write('  FAIL: Accepted atom when string required'), nl),
        Error,
        (write('  PASS: Correctly rejected atom - '), write(Error), nl)
    ),
    py_finalize,
    nl,

    % Test 2: py_run_simple_string with string should succeed
    write('Test 2: py_run_simple_string with string (should succeed)...'), nl,
    py_initialize,
    catch(
        (py_run_simple_string("print('test')"),
         write('  PASS: Accepted string'), nl),
        Error,
        (write('  FAIL: Rejected valid string - '), write(Error), nl)
    ),
    py_finalize,
    nl,

    % Test 3: py_run_simple_string/5 with atom should fail
    write('Test 3: py_run_simple_string/5 with atom (should fail)...'), nl,
    py_initialize,
    catch(
        (py_run_simple_string('x = 42', [], [], _, _),
         write('  FAIL: Accepted atom when string required'), nl),
        _Error3,
        (write('  PASS: Correctly rejected atom'), nl)
    ),
    py_finalize,
    nl,

    % Test 4: py_run_simple_string/5 with string should succeed
    write('Test 4: py_run_simple_string/5 with string (should succeed)...'), nl,
    py_initialize,
    catch(
        (py_run_simple_string("x = 42", [], [], Globals, _),
         member(x-42, Globals),
         write('  PASS: Accepted string'), nl),
        Error,
        (write('  FAIL: Rejected valid string - '), write(Error), nl)
    ),
    py_finalize,
    nl,

    write('=== All argument type tests complete! ==='), nl,
    halt.
