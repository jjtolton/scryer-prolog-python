# libpython-clj Review: Key Patterns for ScryPy

**Date**: 2025-11-06
**Purpose**: Extract patterns from libpython-clj to guide Phase 1 implementation (None, Lists, Tuples)

## Overview

libpython-clj is a Clojure-Python integration library that provides deep interop via JNA (Java Native Access). While the platforms differ (JVM vs Scryer Prolog FFI), the Python C API usage patterns are directly applicable.

## Python C API Functions Used

### None
- **`_Py_NoneStruct`** - The None singleton (accessed via `py-none`)
- Pattern: None is a singleton object that needs reference counting like any other PyObject

### Lists
- **`PyList_New(size)`** - Create new list
- **`PyList_SetItem(list, index, item)`** - Set item (steals reference!)
- **`PyList_GetItem(list, index)`** - Get item (borrowed reference!)
- **`PyList_Size(list)`** - Get length

### Tuples
- **`PyTuple_New(size)`** - Create new tuple
- **`PyTuple_SetItem(tuple, index, item)`** - Set item during initialization
- **`PyTuple_GetItem(tuple, index)`** - Get item (borrowed reference!)
- **`PyTuple_Size(tuple)`** - Get length

## Key Memory Management Patterns

### 1. Reference Counting Discipline

**NEW references** (must decref):
- `PyList_New()`, `PyTuple_New()`
- Return values from most creation functions

**BORROWED references** (do NOT decref):
- `PyList_GetItem()`, `PyTuple_GetItem()`
- These return references owned by the container

**STOLEN references**:
- `PyList_SetItem()`, `PyTuple_SetItem()` STEAL the reference
- After calling these, you no longer own the reference - don't decref!

### 2. Cleanup Patterns

libpython-clj uses `with-decref` macro:
```clojure
(with-decref [pyobj (PyList_New 10)]
  ;; Use pyobj
  ;; Automatically decrefd in finally block
)
```

**ScryPy equivalent**: Our `setup_call_cleanup/3` pattern:
```prolog
py_list_new(L) :-
    ffi:'PyList_New'(0, L).

use_list_safely(Result) :-
    py_list_new(L),
    setup_call_cleanup(
        true,
        work_with_list(L, Result),
        py_xdecref(L)
    ).
```

### 3. GIL Management

libpython-clj wraps all Python operations in `with-gil`:
```clojure
(py-ffi/with-gil
  (python-operation))
```

**ScryPy consideration**: Single-threaded for now, but important for future:
- Python 3.12+ per-interpreter GIL
- Future: `setup_call_cleanup` could ensure GIL acquire/release

## Type Conversion Patterns

### Python → Native

libpython-clj approach:
```clojure
;; Python None → Clojure nil
(if (py-none? v) nil ...)

;; Python list → Clojure vector
(vec python-list)

;; Python tuple → Clojure vector
(vec python-tuple)
```

**ScryPy equivalent**:
```prolog
% Python None → Prolog none atom
pyobject_to_prolog_value(PyObject, none) :-
    py_none(PyObject), !.

% Python list → Prolog list
py_list_to_prolog(PyList, PrologList) :-
    py_list_size(PyList, Size),
    % Iterate and convert each element
    ...

% Python tuple → Prolog list
py_tuple_to_prolog(PyTuple, PrologList) :-
    py_tuple_size(PyTuple, Size),
    % Iterate and convert each element
    ...
```

### Native → Python

libpython-clj:
```clojure
(->py-list [1 2 3])   ;; Clojure vector → Python list
(->py-tuple [1 2 3])  ;; Clojure vector → Python tuple
```

**ScryPy equivalent**:
```prolog
% Prolog list → Python list
prolog_to_py_list([1, 2, 3], PyList) :-
    length(List, Len),
    py_list_new(Len, PyList),
    fill_list(PyList, 0, [1, 2, 3]).

% Prolog list → Python tuple
prolog_to_py_tuple([1, 2, 3], PyTuple) :-
    length(List, Len),
    py_tuple_new(Len, PyTuple),
    fill_tuple(PyTuple, 0, [1, 2, 3]).
```

## Critical Gotcha: Borrowed vs Stolen References

### Problem: PyList_GetItem returns BORROWED reference

```python
# C API:
PyObject* item = PyList_GetItem(list, 0);  // BORROWED
Py_INCREF(item);  // Must incref if keeping!
```

**ScryPy implication**:
```prolog
% WRONG - decref a borrowed reference
py_list_get(List, Index, Item) :-
    ffi:'PyList_GetItem'(List, Index, Item),
    % DON'T decref Item later!

% CORRECT - incref if caller will manage it
py_list_get_owned(List, Index, Item) :-
    ffi:'PyList_GetItem'(List, Index, Item),
    py_incref(Item).  % Now caller owns it
```

### Problem: PyList_SetItem STEALS reference

```python
# C API:
PyObject* item = PyLong_FromLong(42);  // NEW reference
PyList_SetItem(list, 0, item);  // STEALS reference - don't decref!
```

**ScryPy implication**:
```prolog
% CORRECT - SetItem steals, so don't decref
py_list_set(List, Index, Value) :-
    prolog_value_to_pyobject(Value, PyValue),  % NEW ref
    ffi:'PyList_SetItem'(List, Index, PyValue, _Result).
    % PyValue is now owned by List - don't decref!
```

## Architecture Recommendations for Phase 1

### 1. None Support

**FFI Bindings**:
```prolog
'_Py_NoneStruct'() -> ptr  % Returns the None singleton
```

**Predicates**:
```prolog
py_none(-PyNone)  % Get None singleton and incref
py_none_check(+PyObject)  % Check if object is None
```

**Type Conversion**:
```prolog
% Prolog none → Python None
prolog_value_to_pyobject(none, PyNone) :- !,
    ffi:'_Py_NoneStruct'(PyNone),
    py_incref(PyNone).

% Python None → Prolog none
pyobject_to_prolog_value(PyObject, none) :-
    ffi:'_Py_NoneStruct'(PyNone),
    PyObject = PyNone, !.
```

### 2. List Support

**FFI Bindings**:
```prolog
'PyList_New'([int], ptr),
'PyList_Size'([ptr], int),
'PyList_GetItem'([ptr, int], ptr),  % BORROWED
'PyList_SetItem'([ptr, int, ptr], int),  % STEALS
'PyList_Append'([ptr, ptr], int)
```

**High-Level Predicates**:
```prolog
py_list_new(-PyList)  % Create empty list
py_list_from_prolog(+PrologList, -PyList)  % Convert
py_list_to_prolog(+PyList, -PrologList)  % Convert back
py_list_get(+PyList, +Index, -Value)  % Get as Prolog value
py_list_set(+PyList, +Index, +Value)  % Set from Prolog value
py_list_append(+PyList, +Value)  % Append Prolog value
py_list_length(+PyList, -Length)
```

### 3. Tuple Support

**FFI Bindings**:
```prolog
'PyTuple_New'([int], ptr),
'PyTuple_Size'([ptr], int),
'PyTuple_GetItem'([ptr, int], ptr),  % BORROWED
'PyTuple_SetItem'([ptr, int, ptr], int)  % STEALS (only during init!)
```

**High-Level Predicates**:
```prolog
py_tuple_new(+Size, -PyTuple)  % Create uninitialized tuple
py_tuple_from_prolog(+PrologList, -PyTuple)  % Convert
py_tuple_to_prolog(+PyTuple, -PrologList)  % Convert back
py_tuple_get(+PyTuple, +Index, -Value)  % Get as Prolog value
py_tuple_length(+PyTuple, -Length)
```

**Note**: Tuples are immutable! SetItem only works during initialization.

## Testing Patterns from libpython-clj

### Unit Test Structure
1. Test creation (empty, with data)
2. Test access (get by index, out of bounds)
3. Test modification (lists only - tuples immutable)
4. Test conversion (roundtrip Prolog ↔ Python)
5. Test memory management (stress test 1000+ creates/destroys)

### Example Test Pattern
```prolog
test_list_roundtrip :-
    Original = [1, hello, 3.14, true, none],
    py_list_from_prolog(Original, PyList),
    py_list_to_prolog(PyList, Result),
    Result = Original,  % Should be identical
    py_xdecref(PyList).
```

## Key Differences: Clojure/JVM vs Prolog

| Aspect | libpython-clj (JVM) | ScryPy (Prolog) |
|--------|---------------------|-----------------|
| **GC Integration** | JVM GC tracks Python objects via finalizers | Manual setup_call_cleanup |
| **Type System** | Duck typing, protocols | Logical unification |
| **Collections** | Vectors, maps, seqs | Lists, terms |
| **None mapping** | `nil` | `none` atom or `[]`? |
| **Mutability** | Immutable by default | Logical variables |
| **Error Handling** | Exceptions | fail/throw |

## Immediate Next Steps

1. **Implement Python None** (simplest):
   - Add `_Py_NoneStruct` to FFI bindings
   - Implement `py_none/1`, `py_none_check/1`
   - Add `none` to type conversion predicates
   - Write 5 unit tests

2. **Implement Python Lists**:
   - Add PyList_* FFI bindings
   - Implement creation, access, conversion predicates
   - Handle borrowed/stolen reference semantics carefully
   - Write 20 unit tests

3. **Implement Python Tuples**:
   - Add PyTuple_* FFI bindings
   - Implement creation, access, conversion predicates
   - Remember: tuples are immutable!
   - Write 13 unit tests

## Concrete Implementation Examples from libpython-clj Source

### Creating Python List from Clojure Vector

From `src/libpython_clj2/python/copy.clj`:
```clojure
(defn ->py-list
  "Copy an object into a new python list."
  [item-seq]
  (py-ffi/check-gil)
  (let [item-seq (vec item-seq)
        retval (py-ffi/PyList_New (count item-seq))]
    (pygc/with-stack-context
      (dotimes [idx (count item-seq)]
        (let [si-retval
              ;;setitem does steal the reference
              (py-ffi/PyList_SetItem
               retval
               idx
               (py-ffi/untracked->python (item-seq idx) py-base/->python))]
          (when-not (== 0 (long si-retval))
            (py-ffi/check-error-throw)))))
    (py-ffi/track-pyobject retval)))
```

**Key points**:
- `PyList_New` creates list with size
- `PyList_SetItem` STEALS the reference (no decref needed)
- Error checking after each SetItem
- Final `track-pyobject` binds to JVM GC

**ScryPy equivalent**:
```prolog
py_list_from_prolog(PrologList, PyList) :-
    length(PrologList, Len),
    ffi:'PyList_New'(Len, PyList),
    setup_call_cleanup(
        true,
        fill_py_list(PyList, 0, PrologList),
        true  % PyList owned by caller
    ).

fill_py_list(_, _, []) :- !.
fill_py_list(PyList, Idx, [H|T]) :-
    prolog_value_to_pyobject(H, PyValue),
    % PyList_SetItem STEALS reference - don't decref PyValue!
    ffi:'PyList_SetItem'(PyList, Idx, PyValue, Result),
    (Result = 0 -> true ; throw(error(python_error, 'PyList_SetItem failed'))),
    Idx1 is Idx + 1,
    fill_py_list(PyList, Idx1, T).
```

### Creating Python Tuple

From `src/libpython_clj2/python/ffi.clj`:
```clojure
(defn untracked-tuple
  "Low-level make tuple fn.  Returns an untracked python tuple."
  [args & [conv-fallback]]
  (check-gil)
  (let [args (vec args)
        argcount (count args)
        tuple (PyTuple_New argcount)]
    (dotimes [idx argcount]
      (PyTuple_SetItem tuple idx (untracked->python (args idx)
                                                    conv-fallback)))
    tuple))
```

**ScryPy equivalent**:
```prolog
py_tuple_from_prolog(PrologList, PyTuple) :-
    length(PrologList, Len),
    ffi:'PyTuple_New'(Len, PyTuple),
    setup_call_cleanup(
        true,
        fill_py_tuple(PyTuple, 0, PrologList),
        true  % PyTuple owned by caller
    ).

fill_py_tuple(_, _, []) :- !.
fill_py_tuple(PyTuple, Idx, [H|T]) :-
    prolog_value_to_pyobject(H, PyValue),
    % PyTuple_SetItem STEALS reference during init
    ffi:'PyTuple_SetItem'(PyTuple, Idx, PyValue, Result),
    (Result = 0 -> true ; throw(error(python_error, 'PyTuple_SetItem failed'))),
    Idx1 is Idx + 1,
    fill_py_tuple(PyTuple, Idx1, T).
```

### Converting Python Tuple to Clojure Vector

From `src/libpython_clj2/python/copy.clj`:
```clojure
(defmethod py-proto/pyobject->jvm :tuple
  [pyobj & [options]]
  (let [n-elems (py-ffi/PyTuple_Size pyobj)]
    (mapv (fn [^long idx]
            (py-base/->jvm (py-ffi/PyTuple_GetItem pyobj idx)))
          (range n-elems))))
```

**Key points**:
- Get size first
- `PyTuple_GetItem` returns BORROWED reference
- Convert each element recursively
- No explicit decref needed (borrowed references)

**ScryPy equivalent**:
```prolog
py_tuple_to_prolog(PyTuple, PrologList) :-
    ffi:'PyTuple_Size'(PyTuple, Size),
    collect_tuple_items(PyTuple, 0, Size, [], PrologList).

collect_tuple_items(_, Size, Size, Acc, Result) :- !,
    reverse(Acc, Result).
collect_tuple_items(PyTuple, Idx, Size, Acc, Result) :-
    ffi:'PyTuple_GetItem'(PyTuple, Idx, PyItem),  % BORROWED
    pyobject_to_prolog_value(PyItem, Value),
    Idx1 is Idx + 1,
    collect_tuple_items(PyTuple, Idx1, Size, [Value|Acc], Result).
```

### Converting Python List to Clojure Vector

From `src/libpython_clj2/python/copy.clj`:
```clojure
(defn python->jvm-copy-persistent-vector
  [pyobj]
  (when-not (= 1 (py-ffi/PySequence_Check pyobj))
    (errors/throwf "Object does not implement sequence protocol: %s"
                   (py-proto/python-type pyobj)))
  (try
    (->> (range (py-ffi/with-error-check (py-ffi/PySequence_Length pyobj)))
         (mapv (fn [idx]
                 (let [pyitem (py-ffi/PySequence_GetItem pyobj idx)]
                   (try
                     (py-base/->jvm pyitem)
                     (finally
                       (py-ffi/Py_DecRef pyitem)))))))
    (catch Throwable e [])))
```

**Key point**: `PySequence_GetItem` returns NEW reference (must decref!)

**ScryPy equivalent** (using PyList_GetItem which is BORROWED):
```prolog
py_list_to_prolog(PyList, PrologList) :-
    ffi:'PyList_Size'(PyList, Size),
    collect_list_items(PyList, 0, Size, [], PrologList).

collect_list_items(_, Size, Size, Acc, Result) :- !,
    reverse(Acc, Result).
collect_list_items(PyList, Idx, Size, Acc, Result) :-
    ffi:'PyList_GetItem'(PyList, Idx, PyItem),  % BORROWED
    pyobject_to_prolog_value(PyItem, Value),
    Idx1 is Idx + 1,
    collect_list_items(PyList, Idx1, Size, [Value|Acc], Result).
```

### Handling None

From `src/libpython_clj2/python/copy.clj`:
```clojure
(defmethod py-proto/pyobject->jvm :default
  [pyobj & [options]]
  (let [python-type-keyword (py-ffi/pyobject-type-kwd pyobj)]
    (cond
      (= :none-type python-type-keyword)
      nil
      ...)))
```

And from `src/libpython_clj2/python/ffi.clj`:
```clojure
(define-static-symbol py-none "_Py_NoneStruct" false)

;; Usage:
(nil? item) (incref (py-none))
```

**ScryPy equivalent**:
```prolog
% Get None singleton
py_none(PyNone) :-
    ffi:'_Py_NoneStruct'(PyNone),
    py_incref(PyNone).  % Must incref if keeping!

% Check if object is None
py_none_check(PyObject) :-
    ffi:'_Py_NoneStruct'(PyNone),
    PyObject = PyNone.

% Convert in type conversion
pyobject_to_prolog_value(PyObject, none) :-
    py_none_check(PyObject), !.

prolog_value_to_pyobject(none, PyNone) :- !,
    py_none(PyNone).
```

## References

- [libpython-clj GitHub](https://github.com/clj-python/libpython-clj)
- Local copy: `../scryer-prolog-python/libpython-clj/`
- [Python C API - None](https://docs.python.org/3/c-api/none.html)
- [Python C API - List](https://docs.python.org/3/c-api/list.html)
- [Python C API - Tuple](https://docs.python.org/3/c-api/tuple.html)
- [Python C API - Reference Counting](https://docs.python.org/3/c-api/refcounting.html)
