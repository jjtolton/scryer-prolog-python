:- use_module('../../src/lib/python').

test_init_only :-
    write('Testing initialization...'), nl,
    catch(
        py_initialize,
        Error,
        (write('Error: '), write(Error), nl, fail)
    ),
    write('Python initialized successfully!'), nl,
    py_finalize,
    write('Done!'), nl.

:- initialization(test_init_only).
