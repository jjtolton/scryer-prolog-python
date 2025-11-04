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

:- use_module('../src/lib/python').

:- initialization(main).

 
main :-
        getenv("HOME", HomeDirStr),
        atom_chars('/.local/share/uv/python/cpython-3.11.14-linux-x86_64-gnu/lib/libpython3.11.so', LibSuffix),
        atom_chars('/programs/scryer-python/.venv/bin/python3', VenvExeSuffix),
        append(HomeDirStr, LibSuffix, LibPathStr),
        append(HomeDirStr, VenvExeSuffix, VenvExeStr),
        atom_chars(LibPath, LibPathStr),
        atom_chars(VenvExe, VenvExeStr),
        py_initialize([
            shared_library_path(LibPath),
            python_executable(VenvExe)
        ]),
        py_run_simple_string("import sys"),
        py_run_simple_string("import warnings"),
        py_run_simple_string("warnings.filterwarnings(\"ignore\")"),
        py_run_simple_string("print(f\"Python version: {sys.version}\")"),
        py_run_simple_string("print(f\"Python prefix: {sys.prefix}\")"),
        py_run_simple_string("import requests"),
        py_run_simple_string("print(f\"Requests version: {requests.__version__}\")"),
        py_run_simple_string("print(\"UV environment test: SUCCESS!\")"),
        py_finalize,
        halt.
