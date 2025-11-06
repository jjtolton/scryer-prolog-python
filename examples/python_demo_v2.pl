:- use_module('../src/lib/python').
:- use_module(library(lists)).

/** Version 0.2.0 Feature Demo

This example demonstrates all the features added in version 0.2.0:
- Dictionary operations
- Type conversion (atoms, integers, floats, booleans)
- Extended py_run_simple_string/5 with globals/locals
- Bidirectional Prolog ↔ Python data exchange
*/

demo :-
    write('╔════════════════════════════════════════════╗'), nl,
    write('║  Python Integration Library v0.2.0 Demo   ║'), nl,
    write('╚════════════════════════════════════════════╝'), nl, nl,

    py_initialize,

    % Feature 1: Dictionary Operations
    write('━━━ Feature 1: Dictionary Operations ━━━'), nl,
    write('Creating Python dict and setting values...'), nl,
    py_dict_new(Dict),
    py_dict_set(Dict, name, 'Bob'),
    py_dict_set(Dict, age, 25),
    py_dict_set(Dict, score, 87.5),
    write('Getting values back...'), nl,
    py_dict_get(Dict, name, Name),
    py_dict_get(Dict, age, Age),
    py_dict_get(Dict, score, Score),
    write('  name: '), write(Name), nl,
    write('  age: '), write(Age), nl,
    write('  score: '), write(Score), nl, nl,

    % Feature 2: Dict to List Conversion
    write('━━━ Feature 2: Dict ↔ List Conversion ━━━'), nl,
    write('Converting Python dict to Prolog list...'), nl,
    py_dict_to_list(Dict, List),
    write('  Result: '), write(List), nl,
    write('Converting Prolog list to Python dict...'), nl,
    prolog_to_py_dict([x-10, y-20, z-30], Dict2),
    py_dict_to_list(Dict2, List2),
    write('  Result: '), write(List2), nl, nl,

    % Feature 3: Type Conversion Examples
    write('━━━ Feature 3: Type Conversions ━━━'), nl,
    write('Testing all supported types...'), nl,
    py_dict_new(TypeDict),
    py_dict_set(TypeDict, text, 'Hello World'),
    py_dict_set(TypeDict, number, 42),
    py_dict_set(TypeDict, decimal, 3.14159),
    py_dict_set(TypeDict, negative, -273),
    py_dict_to_list(TypeDict, TypeList),
    write('  Types: '), write(TypeList), nl, nl,

    % Feature 4: Extended py_run_simple_string/5
    write('━━━ Feature 4: Globals/Locals Support ━━━'), nl,
    write('Example 1: Computing with custom variables'), nl,
    write('  Code: distance = speed * time'), nl,
    write('  Inputs: speed=60, time=2.5'), nl,
    py_run_simple_string("distance = speed * time",
                         [speed-60, time-2.5],
                         [],
                         Globals1,
                         _),
    (member(distance-Dist, Globals1) ->
        (write('  Result: distance = '), write(Dist), nl)
    ;   write('  ERROR: distance not found'), nl
    ),
    nl,

    % Feature 5: Complex Computation
    write('Example 2: Python expressions and functions'), nl,
    write('  Code: result = round(2.5 * 3.7 + 1.2, 2)'), nl,
    py_run_simple_string("result = round(2.5 * 3.7 + 1.2, 2)",
                         [],
                         [],
                         Globals2,
                         _),
    (member(result-Res, Globals2) ->
        (write('  Result: result = '), write(Res), nl)
    ;   write('  ERROR: result not found'), nl
    ),
    nl,

    % Feature 6: Stateful Computation
    write('Example 3: Stateful computation chain'), nl,
    write('  Step 1: x = 10'), nl,
    py_run_simple_string("x = 10", [], [], G1, _),
    write('  Step 2: y = x * 2'), nl,
    py_run_simple_string("y = x * 2", G1, [], G2, _),
    write('  Step 3: z = x + y'), nl,
    py_run_simple_string("z = x + y", G2, [], G3, _),
    (member(x-X, G3), member(y-Y, G3), member(z-Z, G3) ->
        (write('  Results: x='), write(X),
         write(', y='), write(Y),
         write(', z='), write(Z), nl)
    ;   write('  ERROR: values not found'), nl
    ),
    nl,

    % Feature 7: Real-world Example
    write('━━━ Feature 7: Real-world Example ━━━'), nl,
    write('Calculating compound interest'), nl,
    write('  Formula: A = P * (1 + r/n)**(n*t)'), nl,
    write('  P=1000, r=0.05, n=12, t=10'), nl,
    py_run_simple_string("A = P * (1 + r/n)**(n*t)",
                         ['P'-1000, r-0.05, n-12, t-10],
                         [],
                         FinGlobals,
                         _),
    (member('A'-Amount, FinGlobals) ->
        (write('  Result: A = $'), write(Amount), nl)
    ;   write('  ERROR: A not found'), nl
    ),
    nl,

    py_finalize,

    write('╔════════════════════════════════════════════╗'), nl,
    write('║         Demo Complete! ✓                   ║'), nl,
    write('╚════════════════════════════════════════════╝'), nl.

:- initialization(demo).
