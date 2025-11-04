:- use_module('src/lib/python').
:- use_module(library(lists)).

test :-
    write('=== Testing py_dict_to_list ==='), nl, nl,

    py_initialize,

    % Create a dict with multiple values
    write('Creating dict with name=Alice, age=30, score=95.5...'), nl,
    py_dict_new(Dict),
    py_dict_set(Dict, name, 'Alice'),
    py_dict_set(Dict, age, 30),
    py_dict_set(Dict, score, 95.5),

    % Convert to list
    write('Converting dict to list...'), nl,
    py_dict_to_list(Dict, List),
    write('Result: '), write(List), nl, nl,

    % Verify contents
    write('Verifying contents...'), nl,
    (member(name-'Alice', List) -> write('  name: PASS') ; write('  name: FAIL')), nl,
    (member(age-30, List) -> write('  age: PASS') ; write('  age: FAIL')), nl,
    (member(score-95.5, List) -> write('  score: PASS') ; write('  score: FAIL')), nl,

    py_finalize,
    write(nl), write('=== Test complete! ==='), nl.

:- initialization(test).
