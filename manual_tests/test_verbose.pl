:- use_module(library(os)).
:- use_module(library(lists)).
:- use_module('src/lib/python').
:- initialization(main).

main :-
    getenv("HOME", HomeDirStr),
    atom_chars('/.local/share/uv/python/cpython-3.11.14-linux-x86_64-gnu/lib/libpython3.11.so', LibSuffix),
    atom_chars('/.local/share/uv/python/cpython-3.11.14-linux-x86_64-gnu', HomeSuffix),
    append(HomeDirStr, LibSuffix, LibPathStr),
    append(HomeDirStr, HomeSuffix, HomePathStr),
    atom_chars(LibPath, LibPathStr),
    atom_chars(PythonHome, HomePathStr),

    py_initialize([
        shared_library_path(LibPath),
        python_home(PythonHome),
        verbose(true)
    ]),

    py_run_simple_string("import json"),
    py_run_simple_string("print('\\nVerbose mode test: SUCCESS!')"),

    py_finalize,
    halt.
