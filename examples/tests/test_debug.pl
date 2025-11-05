:- use_module('../../src/lib/python').

test :-
    write('Step 1: About to check if initialized'), nl,
    (is_python_initialized -> write('Already initialized!') ; write('Not initialized')), nl,
    write('Step 2: About to initialize'), nl,
    py_initialize,
    write('Step 3: Initialized successfully'), nl,
    py_finalize.

:- initialization(test).
