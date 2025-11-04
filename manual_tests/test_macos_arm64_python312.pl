:- use_module(library(charsio)).
:- use_module(library(process)).
:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(library(iso_ext)).
:- use_module(library(lambda)).
:- use_module(library(format)).
:- use_module(library(reif)).
:- use_module(library(dif)).
:- use_module(library(debug)).
:- use_module(library(os)).

:- use_module('/path/to/src/lib/python.pl').

:- initialization(main).

main :-
    py_initialize([
        shared_library_path('/opt/homebrew/opt/python@3.12/Frameworks/Python.framework/Versions/3.12/lib/libpython3.12.dylib'),
        python_executable('/opt/homebrew/opt/python@3.12/bin/python3.12')
    ]),
    py_run_simple_string("import sys"),
    py_run_simple_string("print(f'Python version: {sys.version}')"),
    py_run_simple_string("print(f'Python prefix: {sys.prefix}')"),
    py_run_simple_string("print(f'Platform: {sys.platform}')"),
    py_run_simple_string("print(f'Architecture: {sys.implementation.name}')"),
    py_run_simple_string("print('macOS arm64 Python 3.12 test: SUCCESS!')"),
    py_finalize,
    halt.
