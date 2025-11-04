# State Management & Reference Counting Design

## Problem

1. **Current state tracking** uses assert/retract which is not ideal for global state
2. **No reference counting** - we're leaking Python object memory
3. **No abstraction** - state management is scattered throughout the code

## Solution

### Part 1: State Management Abstraction

Use library(iso_ext)'s blackboard system (`bb_put/2`, `bb_get/2`) wrapped in clean predicates:

```prolog
%% Internal state management
python_state_get(Key, Value) :- bb_get(Key, Value).
python_state_set(Key, Value) :- bb_put(Key, Value).
python_state_check(Key) :- catch(bb_get(Key, true), _, fail).

%% Public state checks
is_python_initialized :- python_state_check(python_initialized).
is_library_loaded :- python_state_check(python_library_loaded).

%% State mutations
mark_python_initialized :- python_state_set(python_initialized, true).
mark_python_finalized :- python_state_set(python_initialized, false).
mark_library_loaded :- python_state_set(python_library_loaded, true).
```

### Part 2: Reference Counting

Python C API uses reference counting for garbage collection. We need to track:

#### New vs Borrowed References

**New references** (we own them, must DECREF when done):
- PyDict_New()
- PyLong_FromLong(), PyFloat_FromDouble(), PyUnicode_FromString()
- PyDict_Keys()
- PyObject_Type()
- PyRun_String()

**Borrowed references** (we don't own them, must NOT decref):
- PyDict_GetItemString() - **BORROWED**
- PyList_GetItem() - **BORROWED**
- PyModule_GetDict() - **BORROWED**

#### FFI Bindings Needed

```prolog
'Py_IncRef'([ptr], void),   % Increment refcount
'Py_DecRef'([ptr], void),   % Decrement refcount
'Py_XDecRef'([ptr], void)   % Decref if not NULL
```

#### Reference Counting Abstraction

```prolog
%% py_incref(+PyObject)
% Increment reference count (rarely needed - for keeping objects alive)
py_incref(PyObject) :-
    PyObject \= 0,
    ffi:'Py_IncRef'(PyObject).

%% py_decref(+PyObject)
% Decrement reference count (call when done with new reference)
py_decref(PyObject) :-
    PyObject \= 0,
    ffi:'Py_DecRef'(PyObject).

%% py_xdecref(+PyObject)
% Safe decref (handles NULL)
py_xdecref(PyObject) :-
    ffi:'Py_XDecRef'(PyObject).
```

#### Scoped Resource Management

For automatic cleanup, use setup_call_cleanup:

```prolog
%% with_new_pyobject(+CreateGoal, -PyObject, +UseGoal)
% Create PyObject, use it, then decref automatically
with_new_pyobject(CreateGoal, PyObject, UseGoal) :-
    call(CreateGoal, PyObject),
    setup_call_cleanup(
        true,
        call(UseGoal, PyObject),
        py_xdecref(PyObject)
    ).
```

### Part 3: Refactoring Strategy

1. Replace all `dynamic` + `assert`/`retract` with blackboard abstraction
2. Add Py_IncRef/Py_DecRef/Py_XDecRef FFI bindings
3. Audit all predicates for reference leaks:
   - `prolog_value_to_pyobject` - returns NEW ref, callers must decref
   - `py_dict_new` - returns NEW ref
   - `py_dict_get` - uses borrowed ref from PyDict_GetItemString
   - `py_dict_to_list` - PyDict_Keys returns NEW ref, must decref KeysObj
4. Add setup_call_cleanup wrappers for automatic cleanup
5. Document NEW vs BORROWED in each predicate's docstring

## Example Refactoring

### Before (py_dict_set with leak):
```prolog
py_dict_set(DictPtr, Key, Value) :-
    prolog_value_to_pyobject(Value, PyValue),  % NEW reference
    ffi:'PyDict_SetItemString'(DictPtr, Key, PyValue, Result),
    % LEAK: PyValue never decref'd!
    (Result = 0 -> true ; throw(...)).
```

### After (with refcounting):
```prolog
py_dict_set(DictPtr, Key, Value) :-
    prolog_value_to_pyobject(Value, PyValue),  % NEW reference
    setup_call_cleanup(
        true,
        (ffi:'PyDict_SetItemString'(DictPtr, Key, PyValue, Result),
         (Result = 0 -> true ; throw(...))),
        py_xdecref(PyValue)  % Always decref, even on error
    ).
```

## Testing Strategy

1. Run existing tests - should still pass
2. Add stress test that creates/destroys many objects
3. Use Python's sys.getrefcount() to verify counts
4. Test cleanup on errors (throw in middle of operation)

## Implementation Order

1. ✅ Design abstraction
2. ⬜ Add bb_put/bb_get based state management
3. ⬜ Add Py_IncRef/DecRef FFI bindings
4. ⬜ Audit and fix py_dict_set, prolog_value_to_pyobject
5. ⬜ Audit and fix py_dict_to_list (KeysObj leak)
6. ⬜ Audit and fix py_run_simple_string/5
7. ⬜ Add with_new_pyobject helper
8. ⬜ Test and verify
