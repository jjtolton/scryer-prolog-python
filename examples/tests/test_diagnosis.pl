:- use_module('../../src/lib/python').

test :-
    write('Step 1: Trying to initialize...'), nl,
    catch(
        py_initialize,
        Error,
        (write('ERROR in py_initialize: '), write(Error), nl, fail)
    ),
    write('Step 2: Initialized successfully!'), nl,
    catch(
        py_run_simple_string("print(\"Hello\")",
        Error2,
        (write('ERROR in py_run_simple_string: '), write(Error2), nl, fail)
    ),
    write('Step 3: Executed code!'), nl,
    py_finalize,
    write('Step 4: Done!'), nl.

:- initialization(test).
