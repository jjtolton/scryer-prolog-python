:- use_module('../../src/lib/python').

test :-
    write('Testing old API...'), nl,
    py_initialize,
    write('Initialized!'), nl,
    py_run_simple_string("print(\"Hello!\")",
    py_finalize,
    write('Done!'), nl.

:- initialization(test).
