:- use_module('../src/lib/python').

demo_basic :-
    write('=== Basic Python Demo ==='), nl,
    py_initialize,

    write('1. Simple arithmetic:'), nl,
    py_run_simple_string("result = 2 + 2"),
    py_run_simple_string("print(f'2 + 2 = {result}')"),

    write('2. String manipulation:'), nl,
    py_run_simple_string("name = 'Scryer Prolog'"),
    py_run_simple_string("print(f'Hello from {name}!')"),

    write('3. Lists and loops:'), nl,
    py_run_simple_string("numbers = [1, 2, 3, 4, 5]"),
    py_run_simple_string("total = sum(numbers)"),
    py_run_simple_string("print(f'Sum of {numbers} = {total}')"),

    write('4. Function definition:'), nl,
    py_run_simple_string("def factorial(n): return 1 if n <= 1 else n * factorial(n-1)"),
    py_run_simple_string("print(f'factorial(5) = {factorial(5)}')"),

    write('5. Dictionary usage:'), nl,
    py_run_simple_string("person = {'name': 'Alice', 'age': 30}"),
    py_run_simple_string("print(f'Person: {person}')"),

    py_finalize,
    write('=== Demo complete! ==='), nl.

demo_numpy :-
    write('=== NumPy Demo (if available) ==='), nl,
    py_initialize,

    write('Attempting to import numpy...'), nl,
    catch(
        (py_run_simple_string("import numpy as np"),
         py_run_simple_string("print('NumPy imported successfully!')"),
         py_run_simple_string("arr = np.array([1, 2, 3, 4, 5])"),
         py_run_simple_string("print(f'Array: {arr}')"),
         py_run_simple_string("print(f'Mean: {np.mean(arr)}')")),
        error(python_error(_), _),
        write('NumPy not available')
    ), nl,

    py_finalize,
    write('=== NumPy demo complete! ==='), nl.

demo_error_handling :-
    write('=== Error Handling Demo ==='), nl,
    py_initialize,

    write('1. Valid code:'), nl,
    py_run_simple_string("x = 10"),
    write('   Success!'), nl,

    write('2. Invalid Python code (syntax error):'), nl,
    catch(
        py_run_simple_string("this is invalid python syntax!!!"),
        error(python_error(Code), _),
        (write('   Caught Python error with code: '), write(Code), nl)
    ),

    py_finalize,
    write('=== Error handling demo complete! ==='), nl.

demo_state_persistence :-
    write('=== State Persistence Demo ==='), nl,
    py_initialize,

    write('Setting variables in multiple calls:'), nl,
    py_run_simple_string("a = 5"),
    py_run_simple_string("b = 10"),
    py_run_simple_string("c = a + b"),
    py_run_simple_string("print(f'a={a}, b={b}, c={c}')"),

    write('Variables persist across calls:'), nl,
    py_run_simple_string("d = c * 2"),
    py_run_simple_string("print(f'd = c * 2 = {d}')"),

    py_finalize,
    write('=== State persistence demo complete! ==='), nl.

run_all_demos :-
    demo_basic, nl,
    demo_state_persistence, nl,
    demo_error_handling, nl,
    demo_numpy.

:- initialization(run_all_demos).
