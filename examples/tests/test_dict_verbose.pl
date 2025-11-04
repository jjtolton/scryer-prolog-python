:- use_module('src/lib/python').

test :-
    write('Step 1: Initializing Python...'), nl,
    catch(py_initialize, E1, (write('ERROR: '), write(E1), nl, fail)),
    write('SUCCESS: Initialized'), nl,

    write('Step 2: Creating dict...'), nl,
    catch(py_dict_new(Dict), E2, (write('ERROR: '), write(E2), nl, fail)),
    write('SUCCESS: Dict = '), write(Dict), nl,

    write('Step 3: Setting name...'), nl,
    catch(py_dict_set(Dict, name, 'Alice'), E3, (write('ERROR: '), write(E3), nl, fail)),
    write('SUCCESS: Set name'), nl,

    write('Step 4: Getting name...'), nl,
    catch(py_dict_get(Dict, name, Name), E4, (write('ERROR: '), write(E4), nl, fail)),
    write('SUCCESS: Name = '), write(Name), nl,

    py_finalize,
    write('All tests passed!'), nl.

:- initialization(test).
