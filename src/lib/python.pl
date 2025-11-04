:- module(python, [
    py_initialize/0,
    py_finalize/0,
    py_run_simple_string/1,
    py_run_simple_string/5,
    py_dict_new/1,
    py_dict_set/3,
    py_dict_get/3,
    py_dict_to_list/2,
    prolog_to_py_dict/2,
    py_dict_to_prolog/2,
    % Reference counting (memory management)
    py_incref/1,
    py_decref/1,
    py_xdecref/1,
    with_new_pyobject/3
]).

/** Python Integration Library

This module provides integration with Python using the FFI library.
It follows a machine-based API similar to libscryer-clj where you:
1. Initialize the Python interpreter (py_initialize/0)
2. Execute Python code (py_run_simple_string/1)
3. Finalize when done (py_finalize/0)

Unlike libpython-clj which creates one Python instance per process,
this library allows you to manage the Python interpreter lifecycle
explicitly, enabling multiple init/finalize cycles if needed.

## Example Usage

```prolog
?- use_module(library(python)).
?- py_initialize.
true.

?- py_run_simple_string('print("Hello from Python!")').
Hello from Python!
true.

?- py_run_simple_string('x = 42').
true.

?- py_run_simple_string('print(f"x = {x}")').
x = 42
true.

?- py_finalize.
true.
```

## Python C API Functions Used

This library uses the following Python C API functions via FFI:
- Py_Initialize(): Initialize the Python interpreter
- Py_Finalize(): Finalize the Python interpreter
- PyRun_SimpleString(): Execute Python code from a string

## Notes

- The Python interpreter must be initialized before any code execution
- py_run_simple_string/1 executes code in the __main__ module namespace
- State persists between calls to py_run_simple_string/1 until py_finalize/0
- If Python code raises an exception, it will throw a python_error/1

## Inspired By

This implementation is inspired by:
- libpython-clj: Python integration for Clojure
- libscryer-clj: Scryer Prolog integration for Clojure
*/

:- use_module(library(ffi)).
:- use_module(library(error)).
:- use_module(library(iso_ext)).
:- use_module(library(files)).
:- use_module(library(os)).

%% ============================================
%% State Management Abstraction
%% ============================================
%%
%% Uses library(iso_ext)'s blackboard system for global state.
%% Abstraction allows easy switching of backend implementation.

%% Internal state management predicates
python_state_set(Key, Value) :- bb_put(Key, Value).
python_state_get(Key, Value) :- bb_get(Key, Value).
python_state_check(Key) :- bb_get(Key, true).

%% Public state check predicates
is_python_initialized :- python_state_check(python_initialized).
is_library_loaded :- python_state_check(python_library_loaded).

%% State mutation predicates
mark_python_initialized :- python_state_set(python_initialized, true).
mark_python_finalized :- python_state_set(python_initialized, false).
mark_library_loaded :- python_state_set(python_library_loaded, true).

%% ============================================
%% Reference Counting Abstraction
%% ============================================
%%
%% Python uses reference counting for memory management.
%% We must track "new" vs "borrowed" references:
%%
%% NEW REFERENCES (we own, must decref when done):
%%   - PyDict_New(), PyLong_FromLong(), PyFloat_FromDouble()
%%   - PyUnicode_FromString(), PyDict_Keys(), PyObject_Type()
%%   - PyRun_String()
%%
%% BORROWED REFERENCES (we don't own, must NOT decref):
%%   - PyDict_GetItemString(), PyList_GetItem()
%%   - PyModule_GetDict()

%% py_incref(+PyObject)
%
% Increment reference count.
% Rarely needed - use when you need to keep an object alive longer.
%
py_incref(PyObject) :-
    PyObject \= 0,
    ffi:'Py_IncRef'(PyObject).

%% py_decref(+PyObject)
%
% Decrement reference count.
% Call when done with a NEW reference.
%
py_decref(PyObject) :-
    PyObject \= 0,
    ffi:'Py_DecRef'(PyObject).

%% py_xdecref(+PyObject)
%
% Safe decrement reference count (handles NULL/0).
% Preferred over py_decref for safety.
% Implements the Py_XDecRef macro logic in Prolog.
%
py_xdecref(PyObject) :-
    (PyObject = 0 -> true ; ffi:'Py_DecRef'(PyObject)).

%% with_new_pyobject(:CreateGoal, -PyObject, :UseGoal)
%
% Create a Python object, use it, then automatically decref.
% Ensures cleanup even on errors via setup_call_cleanup.
%
% Example:
% ```
% with_new_pyobject(
%     ffi:'PyLong_FromLong'(42),
%     PyInt,
%     py_dict_set(Dict, age, PyInt)
% )
% ```
%
with_new_pyobject(CreateGoal, PyObject, UseGoal) :-
    call(CreateGoal, PyObject),
    setup_call_cleanup(
        true,
        call(UseGoal, PyObject),
        py_xdecref(PyObject)
    ).

%% py_initialize
%
% Initialize the Python interpreter.
% This must be called before any other Python operations.
% Throws an error if Python is already initialized.
%
% @throws permission_error if Python is already initialized
%
py_initialize :-
    (   is_python_initialized
    ->  throw(error(permission_error(create, python_interpreter, already_initialized), py_initialize/0))
    ;   load_python_library_once,
        ffi:'Py_Initialize',
        mark_python_initialized
    ).

%% py_finalize
%
% Finalize the Python interpreter and free all resources.
% After calling this, you must call py_initialize again before
% executing more Python code.
%
% @throws existence_error if Python is not initialized
%
py_finalize :-
    (   is_python_initialized
    ->  ffi:'Py_Finalize',
        mark_python_finalized
    ;   throw(error(existence_error(python_interpreter, not_initialized), py_finalize/0))
    ).

%% py_run_simple_string(+Code)
%
% Execute Python code from a string.
% The code is executed in the __main__ module's namespace.
% State persists between calls, so variables assigned in one call
% can be accessed in subsequent calls.
%
% @param Code An atom containing Python code to execute
% @throws existence_error if Python is not initialized
% @throws python_error if the Python code raises an exception
%
% Example:
% ```prolog
% ?- py_run_simple_string('x = 10').
% true.
%
% ?- py_run_simple_string('y = x * 2').
% true.
%
% ?- py_run_simple_string('print(y)').
% 20
% true.
% ```
%
py_run_simple_string(Code) :-
    must_be(atom, Code),
    (   is_python_initialized
    ->  ffi:'PyRun_SimpleString'(Code, Result),
        (Result = 0 -> true ; throw(error(python_error(Result), py_run_simple_string/1)))
    ;   throw(error(existence_error(python_interpreter, not_initialized), py_run_simple_string/1))
    ).

%% python_library_path(-Path)
%
% Determines the path to the Python shared library.
%
% Search order:
% 1. User configuration file (python_config.pl) if it exists
% 2. LIBPYTHON_PATH environment variable if set
% 3. Auto-detection from common locations
%
% Fails if no compatible Python library is found.
%
python_library_path(Path) :-
    % Try user config file first
    (   exists_file("python_config.pl")
    ->  consult('python_config.pl'),
        python_library_path_config(Path)
    % Then try environment variable
    ;   getenv('LIBPYTHON_PATH', EnvPath),
        atom_chars(EnvPath, EnvPathChars),
        file_exists(EnvPathChars),
        !,
        Path = EnvPath
    % Finally, auto-detect
    ;   candidate_python_library(PathAtom),
        atom_chars(PathAtom, PathChars),
        file_exists(PathChars),
        !,
        Path = PathAtom
    % All methods failed
    ;   throw(error(
            existence_error(python_library, not_found),
            context(_, 'Could not find Python shared library. Tried:
1. python_config.pl configuration file
2. LIBPYTHON_PATH environment variable
3. Auto-detection (Python 3.10, 3.11, 3.12)

See INSTALL.md for setup instructions.')
        ))
    ).

%% python_library_path_config(-Path)
%
% Hook for user configuration file.
% This predicate is defined by the user in python_config.pl if they
% want to override the default library path detection.
%
% This is a multifile predicate that can be defined in python_config.pl.
%
:- multifile python_library_path_config/1.

%% getenv(+VarName, -Value)
%
% Get environment variable. Fails if not set.
%
getenv(VarName, Value) :-
    catch(
        (os:getenv(VarName, Value), Value \= ''),
        _,
        fail
    ).

%% candidate_python_library(-Path)
%
% Generates candidate paths for Python shared libraries.
% Order matters - tries newer versions first.
%
candidate_python_library('/usr/lib/x86_64-linux-gnu/libpython3.12.so').
candidate_python_library('/usr/lib/x86_64-linux-gnu/libpython3.11.so').
candidate_python_library('/usr/lib/x86_64-linux-gnu/libpython3.10.so').
candidate_python_library('/opt/homebrew/lib/libpython3.12.dylib').
candidate_python_library('/opt/homebrew/lib/libpython3.11.dylib').
candidate_python_library('/opt/homebrew/lib/libpython3.10.dylib').
candidate_python_library('/usr/local/lib/libpython3.12.dylib').
candidate_python_library('/usr/local/lib/libpython3.11.dylib').
candidate_python_library('/usr/local/lib/libpython3.10.dylib').

%% load_python_library_once
%
% Internal predicate to load the Python shared library and bind the necessary functions.
% Uses a dynamic flag to ensure the library is only loaded once.
%
% Tries multiple common Python library locations in order:
% 1. Python 3.12 (latest stable)
% 2. Python 3.11
% 3. Python 3.10
%
% Supports Linux (x86_64), macOS (Homebrew on both Intel and Apple Silicon).
%
load_python_library_once :-
    is_library_loaded, !.
load_python_library_once :-
    python_library_path(LibPath),
    use_foreign_module(LibPath, [
        % Core Python functions (BASELINE - WORKING)
        'Py_Initialize'([], void),
        'Py_Finalize'([], void),
        'PyRun_SimpleString'([cstr], int),

        % TEST FIRST HALF
        'PyRun_String'([cstr, int, ptr, ptr], ptr),
        'PyDict_New'([], ptr),
        'PyDict_SetItemString'([ptr, cstr, ptr], int),
        'PyDict_GetItemString'([ptr, cstr], ptr),
        'PyDict_Keys'([ptr], ptr),
        'PyDict_Size'([ptr], i64),
        'PyLong_FromLong'([long], ptr),
        'PyFloat_FromDouble'([double], ptr),
        'PyUnicode_FromString'([cstr], ptr),
        'PyLong_AsLong'([ptr], long),
        'PyFloat_AsDouble'([ptr], double),
        'PyUnicode_AsUTF8'([ptr], cstr),
        'PyObject_Type'([ptr], ptr),
        'PyObject_IsTrue'([ptr], int),

        % Type checking functions
        % Note: PyLong_Check, PyFloat_Check etc are macros, not functions
        % So we use a try-convert approach instead
        'PyErr_Occurred'([], ptr),  % Check if an error occurred
        'PyErr_Clear'([], void),    % Clear the error

        % List operations (for iterating dict keys)
        'PyList_Size'([ptr], i64),
        'PyList_GetItem'([ptr, i64], ptr),

        % Module operations (for py_run_simple_string/5)
        'PyImport_AddModule'([cstr], ptr),
        'PyModule_GetDict'([ptr], ptr),

        % Reference counting (memory management)
        'Py_IncRef'([ptr], void),   % Increment reference count
        'Py_DecRef'([ptr], void)    % Decrement reference count
        % Note: Py_XDecRef is a macro, not a function - we implement it in Prolog

        % NOTE: PyType_GetName hangs - incorrect signature or unavailable
        % 'PyType_GetName'([ptr], cstr)
    ]),
    mark_library_loaded.

%% ============================================
%% Dictionary Operations (Version 0.2.0+)
%% ============================================

%% py_dict_new(-DictPtr)
%
% Create a new empty Python dictionary.
% Returns a pointer to the dictionary.
%
% MEMORY: Returns a NEW reference - caller must py_xdecref(DictPtr) when done!
%
% @param DictPtr Unified with pointer to new Python dict
%
py_dict_new(DictPtr) :-
    is_python_initialized,
    ffi:'PyDict_New'(DictPtr).

%% py_dict_set(+DictPtr, +Key, +Value)
%
% Set a key-value pair in a Python dictionary.
% Currently supports: atoms (as strings), integers, floats
%
% @param DictPtr Pointer to Python dict
% @param Key Prolog atom (converted to Python string)
% @param Value Prolog value (atom/integer/float)
%
py_dict_set(DictPtr, Key, Value) :-
    must_be(atom, Key),
    is_python_initialized,
    prolog_value_to_pyobject(Value, PyValue),
    setup_call_cleanup(
        true,
        (ffi:'PyDict_SetItemString'(DictPtr, Key, PyValue, Result),
         (Result = 0 -> true ; throw(error(python_error(dict_set_failed), py_dict_set/3)))),
        py_xdecref(PyValue)
    ).

%% py_dict_get(+DictPtr, +Key, -Value)
%
% Get a value from a Python dictionary by key.
% Converts Python values back to Prolog.
%
% @param DictPtr Pointer to Python dict
% @param Key Prolog atom (key name)
% @param Value Unified with Prolog value
%
py_dict_get(DictPtr, Key, Value) :-
    must_be(atom, Key),
    is_python_initialized,
    ffi:'PyDict_GetItemString'(DictPtr, Key, PyValue),
    (PyValue = 0 ->
        fail  % Key not found
    ;
        pyobject_to_prolog_value(PyValue, Value)
    ).

%% py_dict_to_list(+DictPtr, -List)
%
% Convert a Python dictionary to a Prolog list of Key-Value pairs.
%
% @param DictPtr Pointer to Python dict
% @param List Unified with list of Key-Value pairs
%
py_dict_to_list(DictPtr, List) :-
    is_python_initialized,
    % Get the keys as a Python list (NEW reference - must decref)
    ffi:'PyDict_Keys'(DictPtr, KeysObj),
    setup_call_cleanup(
        ffi:'PyList_Size'(KeysObj, Size),
        % Iterate through indices and build the list
        dict_keys_to_list(DictPtr, KeysObj, 0, Size, List),
        py_xdecref(KeysObj)
    ).

%% dict_keys_to_list(+DictPtr, +KeysObj, +Index, +Size, -List)
%
% Helper to iterate through dictionary keys and build Key-Value pairs.
%
dict_keys_to_list(_DictPtr, _KeysObj, Index, Size, []) :-
    Index >= Size, !.
dict_keys_to_list(DictPtr, KeysObj, Index, Size, [Key-Value|Rest]) :-
    Index < Size,
    % Get the key at this index
    ffi:'PyList_GetItem'(KeysObj, Index, KeyObj),
    % Convert key to Prolog (should be a string/atom)
    pyobject_to_prolog_value(KeyObj, Key),
    % Get the corresponding value from the dict
    ffi:'PyDict_GetItemString'(DictPtr, Key, ValueObj),
    % Convert value to Prolog
    pyobject_to_prolog_value(ValueObj, Value),
    % Continue with next index
    NextIndex is Index + 1,
    dict_keys_to_list(DictPtr, KeysObj, NextIndex, Size, Rest).

%% prolog_to_py_dict(+PrologList, -DictPtr)
%
% Convert a Prolog list of Key-Value pairs to a Python dictionary.
%
% MEMORY: Returns a NEW reference - caller must py_xdecref(DictPtr) when done!
%
% @param PrologList List of Key-Value pairs (e.g., [name-'Alice', age-30])
% @param DictPtr Unified with pointer to new Python dict
%
prolog_to_py_dict(PrologList, DictPtr) :-
    must_be(list, PrologList),
    is_python_initialized,
    py_dict_new(DictPtr),
    fill_dict_from_list(DictPtr, PrologList).

fill_dict_from_list(_DictPtr, []).
fill_dict_from_list(DictPtr, [Key-Value|Rest]) :-
    py_dict_set(DictPtr, Key, Value),
    fill_dict_from_list(DictPtr, Rest).

%% py_dict_to_prolog(+DictPtr, -PrologList)
%
% Convert a Python dictionary to a Prolog list of Key-Value pairs.
% Alias for py_dict_to_list/2.
%
py_dict_to_prolog(DictPtr, List) :-
    py_dict_to_list(DictPtr, List).

%% prolog_value_to_pyobject(+Value, -PyObject)
%
% Convert a Prolog value to a Python object pointer.
% Supports: integers, floats, atoms (as strings)
%
% MEMORY: Returns a NEW reference - caller must py_xdecref(PyObject) when done!
%
prolog_value_to_pyobject(Value, PyObject) :-
    integer(Value), !,
    ffi:'PyLong_FromLong'(Value, PyObject).
prolog_value_to_pyobject(Value, PyObject) :-
    float(Value), !,
    ffi:'PyFloat_FromDouble'(Value, PyObject).
prolog_value_to_pyobject(Value, PyObject) :-
    atom(Value), !,
    ffi:'PyUnicode_FromString'(Value, PyObject).
prolog_value_to_pyobject(Value, _) :-
    throw(error(type_error(python_convertible, Value), prolog_value_to_pyobject/2)).

%% pyobject_to_prolog_value(+PyObject, -Value)
%
% Convert a Python object pointer to a Prolog value.
% Supports: bool, int, float, str
%
% Follows libpython-clj's approach:
% 1. Get the type of the Python object
% 2. Dispatch based on type
% 3. Convert using appropriate Python C API function
%
pyobject_to_prolog_value(PyObject, Value) :-
    % Try-convert approach: attempt each conversion and check for errors
    % Python C API sets error state when conversion fails

    % Try string first (most specific)
    ffi:'PyUnicode_AsUTF8'(PyObject, StrResult),
    ffi:'PyErr_Occurred'(Err1),
    (   Err1 = 0, StrResult \= 0
    ->  % Convert character list to atom
        atom_chars(Value, StrResult)
    ;   % Not a string, clear error and try integer
        ffi:'PyErr_Clear',
        ffi:'PyLong_AsLong'(PyObject, IntResult),
        ffi:'PyErr_Occurred'(Err2),
        (   Err2 = 0
        ->  Value = IntResult
        ;   % Not an int, clear error and try float
            ffi:'PyErr_Clear',
            ffi:'PyFloat_AsDouble'(PyObject, FloatResult),
            ffi:'PyErr_Occurred'(Err3),
            (   Err3 = 0
            ->  Value = FloatResult
            ;   % Try boolean last
                ffi:'PyErr_Clear',
                ffi:'PyObject_IsTrue'(PyObject, BoolInt),
                (BoolInt = 1 -> Value = true ; Value = false)
            )
        )
    ).

%% ============================================
%% Extended py_run_simple_string with globals/locals
%% ============================================

%% py_run_simple_string(+Code, +GlobalsIn, +LocalsIn, -GlobalsOut, -LocalsOut)
%
% Execute Python code with explicit globals and locals dictionaries.
% Similar to libpython-clj's run-simple-string.
%
% @param Code Python code to execute (atom)
% @param GlobalsIn List of Key-Value pairs for global namespace
% @param LocalsIn List of Key-Value pairs for local namespace
% @param GlobalsOut Resulting global namespace as Key-Value list
% @param LocalsOut Resulting local namespace as Key-Value list
%
% Example:
% ```prolog
% ?- py_run_simple_string('x = a + b',
%                         [a-5, b-10],
%                         [],
%                         Globals,
%                         Locals).
% Globals = [a-5, b-10, x-15],
% Locals = [...].
% ```
%
py_run_simple_string(Code, GlobalsIn, LocalsIn, GlobalsOut, LocalsOut) :-
    must_be(atom, Code),
    must_be(list, GlobalsIn),
    must_be(list, LocalsIn),
    is_python_initialized,

    % Convert Prolog lists to Python dicts
    % Track whether we own the references (must decref) or not
    (GlobalsIn = [] ->
        % Use __main__ globals (BORROWED reference - don't decref)
        ffi:'PyImport_AddModule'('__main__', MainModule),
        ffi:'PyModule_GetDict'(MainModule, GlobalsDict),
        GlobalsOwned = false
    ;
        % Create new dict (NEW reference - must decref)
        prolog_to_py_dict(GlobalsIn, GlobalsDict),
        GlobalsOwned = true
    ),

    (LocalsIn = [] ->
        % Alias to globals (don't decref separately)
        LocalsDict = GlobalsDict,
        LocalsOwned = false
    ;
        % Create new dict (NEW reference - must decref)
        prolog_to_py_dict(LocalsIn, LocalsDict),
        LocalsOwned = true
    ),

    % Execute code and ensure cleanup
    setup_call_cleanup(
        % Execute: PyRun_String(code, Py_file_input=257, globals, locals)
        ffi:'PyRun_String'(Code, 257, GlobalsDict, LocalsDict, ResultPtr),
        % Process results
        (ResultPtr = 0 ->
            throw(error(python_error(execution_failed), py_run_simple_string/5))
        ;
            % Convert dicts back to Prolog lists
            (py_dict_to_prolog(GlobalsDict, GlobalsOut),
             py_dict_to_prolog(LocalsDict, LocalsOut))
        ),
        % Cleanup: decref only NEW references
        (py_xdecref(ResultPtr),
         (GlobalsOwned = true -> py_xdecref(GlobalsDict) ; true),
         (LocalsOwned = true -> py_xdecref(LocalsDict) ; true))
    ).
