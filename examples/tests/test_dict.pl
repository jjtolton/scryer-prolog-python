:- use_module('src/lib/python').

test_basic_dict :-
    write('Testing basic dictionary operations...'), nl,
    py_initialize,

    write('1. Creating empty dict...'), nl,
    py_dict_new(Dict),
    write('   Dict created: '), write(Dict), nl,

    write('2. Setting values...'), nl,
    py_dict_set(Dict, name, 'Alice'),
    py_dict_set(Dict, age, 30),
    py_dict_set(Dict, score, 95.5),
    write('   Values set!'), nl,

    write('3. Getting values...'), nl,
    py_dict_get(Dict, name, Name),
    write('   name = '), write(Name), nl,
    py_dict_get(Dict, age, Age),
    write('   age = '), write(Age), nl,
    py_dict_get(Dict, score, Score),
    write('   score = '), write(Score), nl,

    py_finalize,
    write('Test complete!'), nl.

test_prolog_to_dict :-
    write('Testing Prolog list to Python dict conversion...'), nl,
    py_initialize,

    write('Converting [a-5, b-10, c-15]...'), nl,
    prolog_to_py_dict([a-5, b-10, c-15], Dict),
    write('Dict created: '), write(Dict), nl,

    write('Verifying values...'), nl,
    py_dict_get(Dict, a, A),
    py_dict_get(Dict, b, B),
    py_dict_get(Dict, c, C),
    write('a='), write(A), write(', b='), write(B), write(', c='), write(C), nl,

    py_finalize,
    write('Test complete!'), nl.

:- initialization(test_basic_dict).
