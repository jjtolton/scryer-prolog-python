:- use_module('../../src/lib/python').
:- use_module(library(lists)).

test :-
    write('=== Testing py_run_simple_string/5 ==='), nl, nl,

    py_initialize,

    % Test 1: Run with custom globals
    write('Test 1: Run with custom globals...'), nl,
    write('  Running: x = a + b with a=5, b=10'), nl,
    py_run_simple_string("x = a + b",
                         [a-5, b-10],
                         [],
                         Globals1,
                         _Locals1),
    write('  Globals: '), write(Globals1), nl,
    (member(x-15, Globals1) -> write('  PASS: x = 15') ; write('  FAIL')), nl, nl,

    % Test 2: Multiple operations
    write('Test 2: Multiple operations...'), nl,
    write('  Running: y = x * 2 with x=10'), nl,
    py_run_simple_string("y = x * 2",
                         [x-10],
                         [],
                         Globals2,
                         _Locals2),
    write('  Globals: '), write(Globals2), nl,
    (member(y-20, Globals2) -> write('  PASS: y = 20') ; write('  FAIL')), nl, nl,

    % Test 3: Computation
    write('Test 3: Computation...'), nl,
    write('  Running: result = 2 ** 10'), nl,
    py_run_simple_string("result = 2 ** 10",
                         [],
                         [],
                         Globals3,
                         _Locals3),
    write('  Globals: '), write(Globals3), nl,
    (member(result-1024, Globals3) -> write('  PASS: result = 1024') ; write('  FAIL')), nl, nl,

    py_finalize,
    write('=== All tests complete! ==='), nl.

:- initialization(test).
