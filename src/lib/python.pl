:- module(python, [
    py_initialize/0,
    py_initialize/1,
    py_finalize/0,
    py_run_simple_string/1,
    py_run_simple_string/5,
    py_dict_new/1,
    py_dict_set/3,
    py_dict_get/3,
    py_dict_to_list/2,
    prolog_to_py_dict/2,
    py_dict_to_prolog/2,
    % Python None support
    py_none/1,
    py_none_check/1,
    % Python List support
    py_list_new/1,
    py_list_size/2,
    py_list_get/3,
    py_list_set/3,
    py_list_append/2,
    py_list_from_prolog/2,
    py_list_to_prolog/2,
    % Python Tuple support
    py_tuple_new/2,
    py_tuple_size/2,
    py_tuple_get/3,
    py_tuple_from_prolog/2,
    py_tuple_to_prolog/2,
    % Type conversion
    prolog_value_to_pyobject/2,
    pyobject_to_prolog_value/2,
    % Reference counting (memory management)
    py_incref/1,
    py_decref/1,
    py_xdecref/1,
    with_new_pyobject/3,
    python_library_path/1
]).

/** Python Integration Library (v0.3.0)

This module provides integration with Python using the FFI library.
It follows a machine-based API similar to libscryer-clj where you:
1. Initialize the Python interpreter (py_initialize/0 or py_initialize/1)
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

## Specifying Python Library Path and Virtual Environments

If auto-detection fails or you want to use a specific Python installation:

```prolog
% Just specify the library path
?- py_initialize([shared_library_path('/path/to/libpython3.11.so')]).
true.

% For virtual environments, specify python_executable (recommended)
?- py_initialize([
    shared_library_path('/home/user/.local/share/uv/python/cpython-3.11.14-linux-x86_64-gnu/lib/libpython3.11.so'),
    python_executable('/home/user/project/.venv/bin/python3')
]).
true.

% Alternatively, specify python_home for base Python installations
?- py_initialize([
    shared_library_path('/home/user/.local/share/uv/python/cpython-3.11.14-linux-x86_64-gnu/lib/libpython3.11.so'),
    python_home('/home/user/.local/share/uv/python/cpython-3.11.14-linux-x86_64-gnu')
]).
true.

% Enable verbose mode for debugging import issues
?- py_initialize([
    shared_library_path('/path/to/libpython3.11.so'),
    verbose(true)
]).
true.
```

Note: Use `python_executable` for venvs - Python will automatically configure sys.path correctly.
The `verbose(true)` option enables Python's verbose mode, showing all module imports and cleanup.

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
:- use_module(library(lists)).

%% Multifile hooks for python.pl configuration
%%
%% python_library_path_user(-Path)
%% Path must be a STRING (double quotes "..."), not an atom!
%% Example: python_library_path_user("/path/to/libpython3.11.so").
:- multifile(python_library_path_user/1).
:- multifile(python_home/1).
:- multifile(python_executable/1).
:- multifile(python_verbose/1).

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
        mark_python_initialized,
        cache_python_none_singleton
    ).

%% py_initialize(+Options)
%
% Initialize the Python interpreter with options.
%
% @param Options List of options:
%   - shared_library_path(Path): Path to libpython*.so
%   - python_home(Path): Python home directory (sets PYTHONHOME)
%   - python_executable(Path): Path to Python executable (for venvs)
%   - verbose(true|false): Enable Python verbose mode for debugging
%
% @throws permission_error if Python is already initialized
%
% Examples:
% ```prolog
% ?- py_initialize([shared_library_path('/path/to/libpython3.11.so')]).
% ?- py_initialize([shared_library_path('/path/to/libpython3.11.so'),
%                    python_home('/path/to/python/home')]).
% ?- py_initialize([shared_library_path('/path/to/libpython3.11.so'),
%                    python_executable('/path/to/.venv/bin/python3')]).
% ?- py_initialize([shared_library_path('/path/to/libpython3.11.so'),
%                    verbose(true)]).
% ```
%
py_initialize(Options) :-
    must_be(list, Options),
    (   is_python_initialized
    ->  throw(error(permission_error(create, python_interpreter, already_initialized), py_initialize/1))
    ;   process_init_options(Options),
        load_python_library_once,
        apply_python_home,
        ffi:'Py_Initialize',
        mark_python_initialized,
        cache_python_none_singleton
    ).

%% process_init_options(+Options)
%
% Process initialization options and set state accordingly.
%
process_init_options([]).
process_init_options([shared_library_path(Path)|Rest]) :-
    python_state_set(python_library_path_override, Path),
    process_init_options(Rest).
process_init_options([python_home(Path)|Rest]) :-
    python_state_set(python_home_override, Path),
    process_init_options(Rest).
process_init_options([python_executable(Path)|Rest]) :-
    python_state_set(python_executable_override, Path),
    process_init_options(Rest).
process_init_options([verbose(Flag)|Rest]) :-
    must_be(atom, Flag),
    (   (Flag = true ; Flag = false)
    ->  python_state_set(python_verbose_override, Flag),
        process_init_options(Rest)
    ;   throw(error(domain_error(boolean, Flag), process_init_options/1))
    ).
process_init_options([Unknown|_]) :-
    throw(error(domain_error(py_initialize_option, Unknown), process_init_options/1)).

%% apply_python_home
%
% Apply python_home, python_executable, and verbose settings if provided.
% Must be called after load_python_library_once and before Py_Initialize.
%
apply_python_home :-
    (   catch(python_state_get(python_verbose_override, true), _, fail)
    ->  os:setenv("PYTHONVERBOSE", "1")
    ;   true
    ),
    (   catch(python_state_get(python_executable_override, ExePath), _, fail)
    ->  ffi:'Py_DecodeLocale'(ExePath, 0, WideExePath),
        ffi:'Py_SetProgramName'(WideExePath)
    ;   true
    ),
    (   catch(python_state_get(python_home_override, HomePath), _, fail)
    ->  ffi:'Py_DecodeLocale'(HomePath, 0, WideHomePath),
        ffi:'Py_SetPythonHome'(WideHomePath)
    ;   true
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
    ->  clear_python_none_singleton,
        ffi:'Py_Finalize',
        mark_python_finalized
    ;   throw(error(existence_error(python_interpreter, not_initialized), py_finalize/0))
    ).

%% py_run_simple_string(+Code)
%
% Execute Python code from an atom or string.
% The code is executed in the __main__ module's namespace.
% State persists between calls, so variables assigned in one call
% can be accessed in subsequent calls.
%
% @param Code An atom or string (char list) containing Python code to execute
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
    % Enforce strings (double quotes) only - reject atoms (single quotes)
    (   atom(Code)
    ->  throw(error(type_error(string, Code),
            context(py_run_simple_string/1,
                    'Python code must use double quotes (strings), not single quotes (atoms). Use "code" not ''code''.')))
    ;   true
    ),
    must_be(chars, Code),
    (   is_python_initialized
    ->  ffi:'PyRun_SimpleString'(Code, Result),
        (Result = 0 -> true ; throw(error(python_error(Result), py_run_simple_string/1)))
    ;   throw(error(existence_error(python_interpreter, not_initialized), py_run_simple_string/1))
    ).

%% python_library_path(-Path)
%
% Determines the path to the Python shared library.
%
% @param Path String (char list) containing the library path
%
% IMPORTANT: Path is a STRING (double quotes "..."), not an atom (single quotes '...')!
%
% Search order:
% 1. Override from py_initialize/1 (if provided)
% 2. User-defined path from python.pl (python_library_path_user/1)
% 3. Auto-detection from candidate paths
%
% Fails if no compatible Python library is found.
%
python_library_path(Path) :-
    % Try override from py_initialize/1 first
    (   catch(python_state_get(python_library_path_override, Path), _, fail)
    ->  true
    % Then try user-defined path (from python.pl if consulted)
    ;   catch(python_library_path_user(Path), _, fail)
    ->  true
    % Finally auto-detect (convert atoms to strings)
    ;   candidate_python_library(PathAtom),
        atom_chars(PathAtom, Path),
        file_exists(Path)
    % All methods failed
    ;   throw(error(
            existence_error(python_library, not_found),
            context(_, 'Could not find Python shared library. See INSTALL.md or consult python.pl for configuration.')
        ))
    ).


%% candidate_python_library(-Path)
%
% Generates candidate paths for Python shared libraries.
% Order matters - tries newer versions first.
%
% Linux system Python
candidate_python_library('/usr/lib/x86_64-linux-gnu/libpython3.12.so').
candidate_python_library('/usr/lib/x86_64-linux-gnu/libpython3.11.so').
candidate_python_library('/usr/lib/x86_64-linux-gnu/libpython3.10.so').

% macOS Homebrew (Apple Silicon) - Framework structure
candidate_python_library('/opt/homebrew/opt/python@3.12/Frameworks/Python.framework/Versions/3.12/lib/libpython3.12.dylib').
candidate_python_library('/opt/homebrew/opt/python@3.11/Frameworks/Python.framework/Versions/3.11/lib/libpython3.11.dylib').
candidate_python_library('/opt/homebrew/opt/python@3.10/Frameworks/Python.framework/Versions/3.10/lib/libpython3.10.dylib').

% macOS Homebrew (Apple Silicon) - Direct lib (fallback)
candidate_python_library('/opt/homebrew/lib/libpython3.12.dylib').
candidate_python_library('/opt/homebrew/lib/libpython3.11.dylib').
candidate_python_library('/opt/homebrew/lib/libpython3.10.dylib').

% macOS Homebrew (Intel) - Framework structure
candidate_python_library('/usr/local/opt/python@3.12/Frameworks/Python.framework/Versions/3.12/lib/libpython3.12.dylib').
candidate_python_library('/usr/local/opt/python@3.11/Frameworks/Python.framework/Versions/3.11/lib/libpython3.11.dylib').
candidate_python_library('/usr/local/opt/python@3.10/Frameworks/Python.framework/Versions/3.10/lib/libpython3.10.dylib').

% macOS Homebrew (Intel) - Direct lib (fallback)
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
        'Py_SetPythonHome'([ptr], void),
        'Py_SetProgramName'([ptr], void),
        'Py_DecodeLocale'([cstr, ptr], ptr),
        'PyRun_SimpleString'([cstr], int),

        % TEST FIRST HALF
        'PyRun_String'([cstr, int, ptr, ptr], ptr),
        'PyDict_New'([], ptr),
        'PyDict_SetItemString'([ptr, cstr, ptr], int),
        'PyDict_GetItemString'([ptr, cstr], ptr),
        'PyDict_Keys'([ptr], ptr),
        'PyDict_Size'([ptr], long),
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

        % List operations
        'PyList_New'([long], ptr),          % Create new list
        'PyList_Size'([ptr], long),         % Get list size
        'PyList_GetItem'([ptr, long], ptr), % Get item (BORROWED ref)
        'PyList_SetItem'([ptr, long, ptr], int), % Set item (STEALS ref)
        'PyList_Append'([ptr, ptr], int),   % Append item (does NOT steal)

        % Tuple operations
        'PyTuple_New'([long], ptr),         % Create new tuple
        'PyTuple_Size'([ptr], long),        % Get tuple size
        'PyTuple_GetItem'([ptr, long], ptr), % Get item (BORROWED ref)
        'PyTuple_SetItem'([ptr, long, ptr], int), % Set item (STEALS ref, only for new tuples)

        % Module operations (for py_run_simple_string/5)
        'PyImport_AddModule'([cstr], ptr),
        'PyModule_GetDict'([ptr], ptr),

        % Reference counting (memory management)
        'Py_IncRef'([ptr], void),   % Increment reference count
        'Py_DecRef'([ptr], void)    % Decrement reference count
        % Note: Py_XDecRef is a macro, not a function - we implement it in Prolog

        % NOTE: PyType_GetName hangs - incorrect signature or unavailable
        % 'PyType_GetName'([ptr], cstr)
    ], [scope(global)]),
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
% Supports: none, integers, floats, atoms (as strings)
%
% MEMORY: Returns a NEW reference - caller must py_xdecref(PyObject) when done!
%
prolog_value_to_pyobject(none, PyObject) :- !,
    py_none(PyObject).
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
% Supports: none, bool, int, float, str
%
% Follows libpython-clj's approach:
% 1. Get the type of the Python object
% 2. Dispatch based on type
% 3. Convert using appropriate Python C API function
%
pyobject_to_prolog_value(PyObject, none) :-
    % Check for None first (most efficient check)
    py_none_check(PyObject), !.
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
%% Python None Support (Version 0.4.0 Phase 1)
%% ============================================

%% cache_python_none_singleton
%
% Cache the Python None singleton during initialization.
% Called automatically by py_initialize/0 and py_initialize/1.
% INTERNAL PREDICATE - not exported.
%
cache_python_none_singleton :-
    ffi:'PyImport_AddModule'("__main__", MainModule),
    ffi:'PyModule_GetDict'(MainModule, Globals),
    ffi:'PyRun_String'("None", 258, Globals, Globals, PyNone),
    python_state_set(py_none_singleton, PyNone).

%% clear_python_none_singleton
%
% Clear the cached Python None singleton and decref it.
% Called automatically by py_finalize/0.
% INTERNAL PREDICATE - not exported.
%
clear_python_none_singleton :-
    (   catch(python_state_get(py_none_singleton, PyNone), _, fail)
    ->  py_xdecref(PyNone),
        python_state_set(py_none_singleton, 0)
    ;   true
    ).

%% py_none(-PyNone)
%
% Get the Python None singleton with a new reference.
% Uses cached None singleton from initialization for efficiency.
% MEMORY: Returns a NEW reference - caller must py_xdecref(PyNone) when done!
%
% Example:
% ```prolog
% ?- py_none(N), py_none_check(N), py_xdecref(N).
% true.
% ```
%
py_none(PyNone) :-
    is_python_initialized,
    python_state_get(py_none_singleton, CachedNone),
    py_incref(CachedNone),
    PyNone = CachedNone.

%% py_none_check(+PyObject)
%
% Check if a Python object is None.
% Succeeds if PyObject is the None singleton, fails otherwise.
% Uses cached None singleton for efficient comparison (no allocation).
%
% Example:
% ```prolog
% ?- py_none(N), py_none_check(N), py_xdecref(N).
% true.
% ```
%
py_none_check(PyObject) :-
    is_python_initialized,
    python_state_get(py_none_singleton, CachedNone),
    PyObject = CachedNone.

%% ============================================
%% Python List Support (Version 0.4.0 Phase 1)
%% ============================================

%% py_list_new(-PyList)
%
% Create a new empty Python list.
% MEMORY: Returns a NEW reference - caller must py_xdecref(PyList) when done!
%
% Example:
% ```prolog
% ?- py_list_new(L), py_list_size(L, Size), py_xdecref(L).
% Size = 0.
% ```
%
py_list_new(PyList) :-
    is_python_initialized,
    ffi:'PyList_New'(0, PyList).

%% py_list_size(+PyList, -Size)
%
% Get the size of a Python list.
%
% Example:
% ```prolog
% ?- py_list_from_prolog([1,2,3], L), py_list_size(L, Size), py_xdecref(L).
% Size = 3.
% ```
%
py_list_size(PyList, Size) :-
    is_python_initialized,
    ffi:'PyList_Size'(PyList, Size).

%% py_list_get(+PyList, +Index, -Value)
%
% Get an item from a Python list by index (0-based).
% Returns the Prolog value, not the PyObject pointer.
%
% Note: PyList_GetItem returns a BORROWED reference, so we don't decref.
%
% Example:
% ```prolog
% ?- py_list_from_prolog([10,20,30], L), py_list_get(L, 1, V), py_xdecref(L).
% V = 20.
% ```
%
py_list_get(PyList, Index, Value) :-
    is_python_initialized,
    ffi:'PyList_GetItem'(PyList, Index, PyItem),
    (PyItem = 0 ->
        fail  % Index out of bounds or error
    ;
        pyobject_to_prolog_value(PyItem, Value)
    ).

%% py_list_set(+PyList, +Index, +Value)
%
% Set an item in a Python list by index (0-based).
% The list must already have an item at this index (use py_list_append for new items).
%
% Note: PyList_SetItem STEALS a reference, so we don't decref the converted value.
%
% Example:
% ```prolog
% ?- py_list_from_prolog([1,2,3], L), py_list_set(L, 1, 99), py_list_get(L, 1, V), py_xdecref(L).
% V = 99.
% ```
%
py_list_set(PyList, Index, Value) :-
    is_python_initialized,
    prolog_value_to_pyobject(Value, PyValue),
    ffi:'PyList_SetItem'(PyList, Index, PyValue, Result),
    (Result = 0 ->
        true
    ;
        throw(error(python_error(list_set_failed), py_list_set/3))
    ).

%% py_list_append(+PyList, +Value)
%
% Append a value to the end of a Python list.
%
% Note: PyList_Append does NOT steal a reference, so we must decref.
%
% Example:
% ```prolog
% ?- py_list_new(L), py_list_append(L, 42), py_list_size(L, S), py_xdecref(L).
% S = 1.
% ```
%
py_list_append(PyList, Value) :-
    is_python_initialized,
    prolog_value_to_pyobject(Value, PyValue),
    setup_call_cleanup(
        true,
        (ffi:'PyList_Append'(PyList, PyValue, Result),
         (Result = 0 -> true ; throw(error(python_error(list_append_failed), py_list_append/2)))),
        py_xdecref(PyValue)
    ).

%% py_list_from_prolog(+PrologList, -PyList)
%
% Convert a Prolog list to a Python list.
% Recursively converts nested lists.
%
% MEMORY: Returns a NEW reference - caller must py_xdecref(PyList) when done!
%
% Example:
% ```prolog
% ?- py_list_from_prolog([1,2,3], L), py_list_to_prolog(L, R), py_xdecref(L).
% R = [1,2,3].
% ```
%
py_list_from_prolog(PrologList, PyList) :-
    is_python_initialized,
    must_be(list, PrologList),
    length(PrologList, Len),
    ffi:'PyList_New'(Len, PyList),
    fill_py_list(PyList, 0, PrologList).

%% fill_py_list(+PyList, +Index, +PrologList)
%
% Helper predicate to fill a Python list from a Prolog list.
% Uses PyList_SetItem which STEALS references.
%
fill_py_list(_, _, []).
fill_py_list(PyList, Index, [Head|Tail]) :-
    % Check if Head is a list by attempting to unify with list pattern
    (   Head = [_|_]  % Non-empty list
    ->  py_list_from_prolog(Head, PySubList),
        ffi:'PyList_SetItem'(PyList, Index, PySubList, _)
    ;   Head = []  % Empty list
    ->  py_list_from_prolog(Head, PySubList),
        ffi:'PyList_SetItem'(PyList, Index, PySubList, _)
    ;   % Scalar value
        prolog_value_to_pyobject(Head, PyValue),
        ffi:'PyList_SetItem'(PyList, Index, PyValue, _)
    ),
    NextIndex is Index + 1,
    fill_py_list(PyList, NextIndex, Tail).

%% py_list_to_prolog(+PyList, -PrologList)
%
% Convert a Python list to a Prolog list.
% Recursively converts nested lists.
%
% Example:
% ```prolog
% ?- py_list_from_prolog([1,[2,3],4], L), py_list_to_prolog(L, R), py_xdecref(L).
% R = [1,[2,3],4].
% ```
%
py_list_to_prolog(PyList, PrologList) :-
    is_python_initialized,
    py_list_size(PyList, Size),
    collect_list_items(PyList, 0, Size, PrologList).

%% collect_list_items(+PyList, +Index, +Size, -Items)
%
% Helper predicate to collect items from a Python list.
% PyList_GetItem returns BORROWED references, so no decref needed.
%
collect_list_items(_, Index, Size, []) :-
    Index >= Size, !.
collect_list_items(PyList, Index, Size, [Head|Tail]) :-
    Index < Size,
    ffi:'PyList_GetItem'(PyList, Index, PyItem),
    (   PyItem = 0
    ->  fail  % Error getting item
    ;   % Try to get size to check if it's a list
        ffi:'PyList_Size'(PyItem, _SubSize),
        ffi:'PyErr_Occurred'(Err),
        (   Err = 0
        ->  % It's a list, recursively convert
            py_list_to_prolog(PyItem, Head)
        ;   % Not a list, clear error and convert to Prolog value
            ffi:'PyErr_Clear',
            pyobject_to_prolog_value(PyItem, Head)
        )
    ),
    NextIndex is Index + 1,
    collect_list_items(PyList, NextIndex, Size, Tail).

%% ============================================
%% Python Tuple Support (Version 0.4.0 Phase 1)
%% ============================================

%% py_tuple_new(+Size, -PyTuple)
%
% Create a new Python tuple with the specified size.
% MEMORY: Returns a NEW reference - caller must py_xdecref(PyTuple) when done!
%
% Note: Tuples are immutable, but when first created via PyTuple_New,
% they can be populated using PyTuple_SetItem before being exposed.
%
% Example:
% ```prolog
% ?- py_tuple_new(3, T), py_tuple_size(T, S), py_xdecref(T).
% S = 3.
% ```
%
py_tuple_new(Size, PyTuple) :-
    is_python_initialized,
    ffi:'PyTuple_New'(Size, PyTuple).

%% py_tuple_size(+PyTuple, -Size)
%
% Get the size of a Python tuple.
%
% Example:
% ```prolog
% ?- py_tuple_from_prolog([1,2,3], T), py_tuple_size(T, S), py_xdecref(T).
% S = 3.
% ```
%
py_tuple_size(PyTuple, Size) :-
    is_python_initialized,
    ffi:'PyTuple_Size'(PyTuple, Size).

%% py_tuple_get(+PyTuple, +Index, -Value)
%
% Get an item from a Python tuple by index (0-based).
% Returns the Prolog value, not the PyObject pointer.
%
% Note: PyTuple_GetItem returns a BORROWED reference, so we don't decref.
%
% Example:
% ```prolog
% ?- py_tuple_from_prolog([10,20,30], T), py_tuple_get(T, 1, V), py_xdecref(T).
% V = 20.
% ```
%
py_tuple_get(PyTuple, Index, Value) :-
    is_python_initialized,
    ffi:'PyTuple_GetItem'(PyTuple, Index, PyItem),
    (PyItem = 0 ->
        fail  % Index out of bounds or error
    ;
        pyobject_to_prolog_value(PyItem, Value)
    ).

%% py_tuple_from_prolog(+PrologList, -PyTuple)
%
% Convert a Prolog list to a Python tuple.
% Recursively converts nested lists to nested tuples.
%
% MEMORY: Returns a NEW reference - caller must py_xdecref(PyTuple) when done!
%
% Example:
% ```prolog
% ?- py_tuple_from_prolog([1,2,3], T), py_tuple_to_prolog(T, R), py_xdecref(T).
% R = [1,2,3].
% ```
%
py_tuple_from_prolog(PrologList, PyTuple) :-
    is_python_initialized,
    must_be(list, PrologList),
    length(PrologList, Len),
    ffi:'PyTuple_New'(Len, PyTuple),
    fill_py_tuple(PyTuple, 0, PrologList).

%% fill_py_tuple(+PyTuple, +Index, +PrologList)
%
% Helper predicate to fill a Python tuple from a Prolog list.
% Uses PyTuple_SetItem which STEALS references.
% This is only valid for newly created tuples (before they're exposed).
%
fill_py_tuple(_, _, []).
fill_py_tuple(PyTuple, Index, [Head|Tail]) :-
    % Check if Head is a list by attempting to unify with list pattern
    (   Head = [_|_]  % Non-empty list
    ->  py_tuple_from_prolog(Head, PySubTuple),
        ffi:'PyTuple_SetItem'(PyTuple, Index, PySubTuple, _)
    ;   Head = []  % Empty list
    ->  py_tuple_from_prolog(Head, PySubTuple),
        ffi:'PyTuple_SetItem'(PyTuple, Index, PySubTuple, _)
    ;   % Scalar value
        prolog_value_to_pyobject(Head, PyValue),
        ffi:'PyTuple_SetItem'(PyTuple, Index, PyValue, _)
    ),
    NextIndex is Index + 1,
    fill_py_tuple(PyTuple, NextIndex, Tail).

%% py_tuple_to_prolog(+PyTuple, -PrologList)
%
% Convert a Python tuple to a Prolog list.
% Recursively converts nested tuples to nested lists.
%
% Example:
% ```prolog
% ?- py_tuple_from_prolog([1,[2,3],4], T), py_tuple_to_prolog(T, R), py_xdecref(T).
% R = [1,[2,3],4].
% ```
%
py_tuple_to_prolog(PyTuple, PrologList) :-
    is_python_initialized,
    py_tuple_size(PyTuple, Size),
    collect_tuple_items(PyTuple, 0, Size, PrologList).

%% collect_tuple_items(+PyTuple, +Index, +Size, -Items)
%
% Helper predicate to collect items from a Python tuple.
% PyTuple_GetItem returns BORROWED references, so no decref needed.
%
collect_tuple_items(_, Index, Size, []) :-
    Index >= Size, !.
collect_tuple_items(PyTuple, Index, Size, [Head|Tail]) :-
    Index < Size,
    ffi:'PyTuple_GetItem'(PyTuple, Index, PyItem),
    (   PyItem = 0
    ->  fail  % Error getting item
    ;   % Try to get size to check if it's a tuple or list
        ffi:'PyTuple_Size'(PyItem, _TupleSize),
        ffi:'PyErr_Occurred'(Err1),
        (   Err1 = 0
        ->  % It's a tuple, recursively convert
            py_tuple_to_prolog(PyItem, Head)
        ;   % Not a tuple, clear error and check if it's a list
            ffi:'PyErr_Clear',
            ffi:'PyList_Size'(PyItem, _ListSize),
            ffi:'PyErr_Occurred'(Err2),
            (   Err2 = 0
            ->  % It's a list, recursively convert
                py_list_to_prolog(PyItem, Head)
            ;   % Not a tuple or list, convert to Prolog value
                ffi:'PyErr_Clear',
                pyobject_to_prolog_value(PyItem, Head)
            )
        )
    ),
    NextIndex is Index + 1,
    collect_tuple_items(PyTuple, NextIndex, Size, Tail).

%% ============================================
%% Extended py_run_simple_string with globals/locals
%% ============================================

%% py_run_simple_string(+Code, +GlobalsIn, +LocalsIn, -GlobalsOut, -LocalsOut)
%
% Execute Python code with explicit globals and locals dictionaries.
% Similar to libpython-clj's run-simple-string.
%
% @param Code Python code to execute (string/char list)
% @param GlobalsIn List of Key-Value pairs for global namespace
% @param LocalsIn List of Key-Value pairs for local namespace
% @param GlobalsOut Resulting global namespace as Key-Value list
% @param LocalsOut Resulting local namespace as Key-Value list
%
% Example:
% ```prolog
% ?- py_run_simple_string("x = a + b",
%                         [a-5, b-10],
%                         [],
%                         Globals,
%                         Locals).
% Globals = [a-5, b-10, x-15],
% Locals = [...].
% ```
%
py_run_simple_string(Code, GlobalsIn, LocalsIn, GlobalsOut, LocalsOut) :-
    % Enforce strings (double quotes) only - reject atoms (single quotes)
    (   atom(Code)
    ->  throw(error(type_error(string, Code),
            context(py_run_simple_string/5,
                    'Python code must use double quotes (strings), not single quotes (atoms). Use "code" not ''code''.')))
    ;   true
    ),
    must_be(chars, Code),
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
