:- use_module('../../src/lib/python').

test :-
    write('=== Testing All Type Conversions ==='), nl, nl,

    py_initialize,

    % Test 1: String
    write('Test 1: String...'), nl,
    py_dict_new(Dict1),
    py_dict_set(Dict1, name, 'Alice'),
    py_dict_get(Dict1, name, Name),
    write('  name = '), write(Name), nl,
    (Name = 'Alice' -> write('  PASS') ; write('  FAIL')), nl, nl,

    % Test 2: Integer
    write('Test 2: Integer...'), nl,
    py_dict_new(Dict2),
    py_dict_set(Dict2, age, 42),
    py_dict_get(Dict2, age, Age),
    write('  age = '), write(Age), nl,
    (Age = 42 -> write('  PASS') ; write('  FAIL')), nl, nl,

    % Test 3: Float
    write('Test 3: Float...'), nl,
    py_dict_new(Dict3),
    py_dict_set(Dict3, score, 95.5),
    py_dict_get(Dict3, score, Score),
    write('  score = '), write(Score), nl,
    (Score = 95.5 -> write('  PASS') ; write('  FAIL')), nl, nl,

    % Test 4: Negative integer
    write('Test 4: Negative integer...'), nl,
    py_dict_new(Dict4),
    py_dict_set(Dict4, temp, -10),
    py_dict_get(Dict4, temp, Temp),
    write('  temp = '), write(Temp), nl,
    (Temp = -10 -> write('  PASS') ; write('  FAIL')), nl, nl,

    % Test 5: Zero
    write('Test 5: Zero...'), nl,
    py_dict_new(Dict5),
    py_dict_set(Dict5, zero, 0),
    py_dict_get(Dict5, zero, Zero),
    write('  zero = '), write(Zero), nl,
    (Zero = 0 -> write('  PASS') ; write('  FAIL')), nl, nl,

    py_finalize,
    write('=== All tests complete! ==='), nl.

:- initialization(test).
