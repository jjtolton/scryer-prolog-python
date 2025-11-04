:- use_module(library(os)).
:- use_module(library(lists)).
:- use_module('src/lib/python').
:- initialization(main).

main :-
    getenv("HOME", HomeDirStr),
    atom_chars('/programs/anaconda3/lib/libpython3.9.so', LibSuffix),
    atom_chars('/programs/anaconda3', HomeSuffix),
    append(HomeDirStr, LibSuffix, LibPathStr),
    append(HomeDirStr, HomeSuffix, HomePathStr),
    atom_chars(LibPath, LibPathStr),
    atom_chars(CondaHome, HomePathStr),

    write('Testing Conda Python integration...'), nl,
    write('Library: '), write(LibPath), nl,
    write('Home: '), write(CondaHome), nl, nl,

    py_initialize([
        shared_library_path(LibPath),
        python_home(CondaHome)
    ]),

    py_run_simple_string("import sys"),
    py_run_simple_string("print(f'Python version: {sys.version}')"),
    py_run_simple_string("print(f'Python prefix: {sys.prefix}')"),
    py_run_simple_string("print(f'Conda env: {sys.prefix}')"),
    py_run_simple_string("print()"),
    py_run_simple_string("print('Conda Python test: SUCCESS!')"),

    py_finalize,
    halt.
