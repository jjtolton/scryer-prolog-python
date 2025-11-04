:- use_module(library(ffi)).

test :-
    write('Loading FFI...'), nl,
    use_foreign_module('/usr/lib/x86_64-linux-gnu/libpython3.10.so', [
        'Py_Initialize'([], void)
    ]),
    write('Calling Py_Initialize...'), nl,
    ffi:'Py_Initialize',
    write('Success!'), nl.

:- initialization(test).
