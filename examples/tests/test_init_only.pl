:- use_module('../../src/lib/python').

test :-
    write('Before init'), nl,
    py_initialize,
    write('After init'), nl,
    py_finalize,
    write('After finalize'), nl.

:- initialization(test).
