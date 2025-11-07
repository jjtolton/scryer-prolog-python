# Phase 1 Implementation Plan: Core Type Expansion

**Status**: Planning Document
**Phase**: v0.4.0
**Created**: 2025-11-07
**Approach**: Test-Driven Development (TDD)

---

## Overview

This document provides a detailed, test-first implementation plan for Phase 1 of the type bridging roadmap. Each type follows the RED-GREEN-REFACTOR cycle.

**Types to Implement**:
1. Python None
2. Python Lists
3. Python Tuples

---

## Implementation Order & Rationale

**Order**: None → Lists → Tuples

**Rationale**:
1. **None** - Simplest, requires design decision but minimal implementation
2. **Lists** - Most commonly used, mutable, good learning ground
3. **Tuples** - Similar to lists but immutable, can reuse patterns

---

## Type 1: Python None

### Design Decision Required

**Question**: How should Python None be represented in Prolog?

**Options**:
- **A**: Atom `none` (recommended - explicit, clear)
- **B**: Empty list `[]` (ambiguous with empty Python list)
- **C**: Boolean `false` (semantically wrong - None ≠ False)
- **D**: Predicate failure (breaks composability)

**Decision**: Use atom `none` (Option A)
- Clear semantics
- No ambiguity
- Follows libpython-clj pattern (`nil` in Clojure)

### Test Plan (RED)

**File**: `tests/unit/test_py_none.pl`

**Test Suite**:
```prolog
:- use_module('../../src/lib/python').
:- use_module('../test_framework').
:- initialization(test_py_none).

test_py_none :-
    reset_test_state,

    % Test 1: Check Python None
    run_test('Python None is recognized', (
        py_initialize,
        py_run_simple_string("x = None"),
        py_run_simple_string("x", [], Globals, []),
        member('x'-Value, Globals),
        Value = none,
        py_finalize
    )),

    % Test 2: Prolog none → Python None
    run_test('Prolog none converts to Python None', (
        py_initialize,
        prolog_value_to_pyobject(none, PyNone),
        py_none(PyNone),  % Verify it's None
        py_xdecref(PyNone),
        py_finalize
    )),

    % Test 3: Python None → Prolog none
    run_test('Python None converts to Prolog none', (
        py_initialize,
        py_run_simple_string("x = None"),
        py_run_simple_string("x", [], Globals, []),
        member('x'-none, Globals),
        py_finalize
    )),

    % Test 4: None in dict operations
    run_test('None works in dictionary operations', (
        py_initialize,
        py_dict_new(Dict),
        py_dict_set(Dict, key, none),
        py_dict_get(Dict, key, Value),
        Value = none,
        py_xdecref(Dict),
        py_finalize
    )),

    % Test 5: None comparison
    run_test('None is distinct from other values', (
        py_initialize,
        py_run_simple_string("x = None; y = False; z = 0; w = ''"),
        py_run_simple_string("x", [], G1, []),
        py_run_simple_string("y", [], G2, []),
        py_run_simple_string("z", [], G3, []),
        py_run_simple_string("w", [], G4, []),
        member('x'-none, G1),
        member('y'-false, G2),
        member('z'-0, G3),
        member('w'-'', G4),
        py_finalize
    )),

    report_results.
```

### Implementation Steps (GREEN)

**1. Add FFI binding for Py_None** (src/lib/python.pl)

```prolog
% Add to FFI bindings section
use_foreign_module(
    LibPath,
    [
        % Existing bindings...

        % None singleton
        '_Py_NoneStruct' as 'Py_GetNone'() -> address
    ],
    [scope(global)]
).
```

**2. Add None check predicate**

```prolog
%% py_none(+PyObject)
% True if PyObject is Python None.
% MEMORY: PyObject must be a valid Python object reference.
py_none(PyObject) :-
    ffi:'Py_GetNone'(PyNone),
    % Compare pointers
    PyObject = PyNone.
```

**3. Update type conversion - Prolog to Python**

```prolog
% Add to prolog_value_to_pyobject/2
prolog_value_to_pyobject(none, PyNone) :- !,
    ffi:'Py_GetNone'(PyNone),
    py_incref(PyNone).  % Increment ref since we're returning it
```

**4. Update type conversion - Python to Prolog**

```prolog
% Add to pyobject_to_prolog_value/2 (first clause)
pyobject_to_prolog_value(PyObject, none) :-
    py_none(PyObject), !.
```

### Verification (REFACTOR)

**Run tests**:
```bash
scryer-prolog tests/unit/test_py_none.pl
```

**Expected output**: All 5 tests pass

**Refactoring checklist**:
- [ ] Add documentation comments
- [ ] Ensure memory safety (ref counting correct)
- [ ] Update main test suite to include None tests
- [ ] Add None examples to demos

---

## Type 2: Python Lists

### Design Decisions

**List Conversion Strategy**: Hybrid
- Provide both deep conversion (`py_list_to_prolog/2`) and opaque handle operations
- Let users choose based on use case

**Mutation Strategy**: Both pure and impure
- Pure: `py_list_append_pure(ListIn, Item, ListOut)` - threads state
- Impure: `py_list_append(List, Item)` - mutates in place
- Start with impure (simpler), add pure later if needed

### Test Plan (RED)

**File**: `tests/unit/test_py_list.pl`

**Test Suite Structure**:

```prolog
:- use_module('../../src/lib/python').
:- use_module('../test_framework').
:- initialization(test_py_list).

test_py_list :-
    reset_test_state,

    % === Creation Tests ===
    run_test('Create empty list', test_list_create_empty),
    run_test('Create list from Prolog list', test_list_from_prolog),
    run_test('Create list with initial size', test_list_new_size),

    % === Access Tests ===
    run_test('Get list length', test_list_length),
    run_test('Get item by index', test_list_get_item),
    run_test('Get item out of bounds fails', test_list_get_bounds),

    % === Mutation Tests ===
    run_test('Set item by index', test_list_set_item),
    run_test('Append item to list', test_list_append),
    run_test('Insert item at index', test_list_insert),

    % === Conversion Tests ===
    run_test('Convert list to Prolog', test_list_to_prolog),
    run_test('Round-trip conversion', test_list_roundtrip),
    run_test('Nested list conversion', test_list_nested),

    % === Iteration Tests ===
    run_test('Iterate with member/2', test_list_member),
    run_test('Iterate with nth0/3', test_list_nth0),
    run_test('Backtracking iteration', test_list_backtrack),

    % === Memory Tests ===
    run_test('List cleanup on failure', test_list_cleanup),
    run_test('List reference counting', test_list_refcount),

    report_results.

% === Test Implementations ===

test_list_create_empty :-
    py_initialize,
    py_list_new(List),
    py_list_length(List, Len),
    Len = 0,
    py_xdecref(List),
    py_finalize.

test_list_from_prolog :-
    py_initialize,
    prolog_to_py_list([1, 2, 3], PyList),
    py_list_length(PyList, Len),
    Len = 3,
    py_xdecref(PyList),
    py_finalize.

test_list_new_size :-
    py_initialize,
    py_list_new(5, List),
    py_list_length(List, Len),
    Len = 5,
    py_xdecref(List),
    py_finalize.

test_list_length :-
    py_initialize,
    prolog_to_py_list([1, 2, 3, 4, 5], List),
    py_list_length(List, Len),
    Len = 5,
    py_xdecref(List),
    py_finalize.

test_list_get_item :-
    py_initialize,
    prolog_to_py_list([10, 20, 30], List),
    py_list_get(List, 0, First),
    py_list_get(List, 1, Second),
    py_list_get(List, 2, Third),
    First = 10,
    Second = 20,
    Third = 30,
    py_xdecref(List),
    py_finalize.

test_list_get_bounds :-
    py_initialize,
    prolog_to_py_list([1, 2, 3], List),
    \+ py_list_get(List, 99, _),  % Should fail
    py_xdecref(List),
    py_finalize.

test_list_set_item :-
    py_initialize,
    prolog_to_py_list([1, 2, 3], List),
    py_list_set(List, 1, 99),
    py_list_get(List, 1, Value),
    Value = 99,
    py_xdecref(List),
    py_finalize.

test_list_append :-
    py_initialize,
    py_list_new(List),
    py_list_append(List, 10),
    py_list_append(List, 20),
    py_list_append(List, 30),
    py_list_length(List, Len),
    Len = 3,
    py_list_get(List, 2, Last),
    Last = 30,
    py_xdecref(List),
    py_finalize.

test_list_insert :-
    py_initialize,
    prolog_to_py_list([1, 3], List),
    py_list_insert(List, 1, 2),
    py_list_to_prolog(List, Result),
    Result = [1, 2, 3],
    py_xdecref(List),
    py_finalize.

test_list_to_prolog :-
    py_initialize,
    prolog_to_py_list([1, 2, 3], PyList),
    py_list_to_prolog(PyList, Result),
    Result = [1, 2, 3],
    py_xdecref(PyList),
    py_finalize.

test_list_roundtrip :-
    py_initialize,
    Original = [1, 2, 3, 4, 5],
    prolog_to_py_list(Original, PyList),
    py_list_to_prolog(PyList, Result),
    Result = Original,
    py_xdecref(PyList),
    py_finalize.

test_list_nested :-
    py_initialize,
    Original = [1, [2, 3], 4],
    prolog_to_py_list(Original, PyList),
    py_list_to_prolog(PyList, Result),
    Result = Original,
    py_xdecref(PyList),
    py_finalize.

test_list_member :-
    py_initialize,
    prolog_to_py_list([1, 2, 3], List),
    py_list_member(2, List),
    py_xdecref(List),
    py_finalize.

test_list_nth0 :-
    py_initialize,
    prolog_to_py_list([a, b, c], List),
    py_list_nth0(1, List, Item),
    Item = b,
    py_xdecref(List),
    py_finalize.

test_list_backtrack :-
    py_initialize,
    prolog_to_py_list([1, 2, 3], List),
    findall(X, py_list_member(X, List), Items),
    Items = [1, 2, 3],
    py_xdecref(List),
    py_finalize.

test_list_cleanup :-
    py_initialize,
    (   py_list_new(List),
        throw(test_error)
    ;   true
    ),
    py_finalize.

test_list_refcount :-
    py_initialize,
    prolog_to_py_list([1, 2, 3], List),
    % Create 1000 references and clean them up
    between(1, 1000, _),
    py_incref(List),
    py_xdecref(List),
    fail.
test_list_refcount :-
    py_finalize.
```

### Implementation Steps (GREEN)

**1. Add FFI bindings** (src/lib/python.pl)

```prolog
% Add to FFI bindings section
use_foreign_module(
    LibPath,
    [
        % Existing bindings...

        % List operations
        'PyList_New'(size: long) -> address,
        'PyList_Size'(list: address) -> long,
        'PyList_GetItem'(list: address, index: long) -> address,  % BORROWED
        'PyList_SetItem'(list: address, index: long, item: address) -> int,
        'PyList_Append'(list: address, item: address) -> int,
        'PyList_Insert'(list: address, index: long, item: address) -> int
    ],
    [scope(global)]
).
```

**2. Implement list creation predicates**

```prolog
%% py_list_new(-PyList)
% Create a new empty Python list.
% MEMORY: Returns a NEW reference - caller must py_xdecref(PyList) when done!
py_list_new(PyList) :-
    ffi:'PyList_New'(0, PyList),
    PyList \= 0.  % Check for NULL

%% py_list_new(+Size, -PyList)
% Create a new Python list with initial size (filled with None).
% MEMORY: Returns a NEW reference - caller must py_xdecref(PyList) when done!
py_list_new(Size, PyList) :-
    integer(Size),
    Size >= 0,
    ffi:'PyList_New'(Size, PyList),
    PyList \= 0.
```

**3. Implement list access predicates**

```prolog
%% py_list_length(+PyList, -Length)
% Get the length of a Python list.
py_list_length(PyList, Length) :-
    ffi:'PyList_Size'(PyList, Length).

%% py_list_get(+PyList, +Index, -Value)
% Get an item from a Python list by index.
% MEMORY: PyList_GetItem returns BORROWED reference - don't decref!
% But we convert to Prolog, so no ref management needed.
py_list_get(PyList, Index, Value) :-
    integer(Index),
    Index >= 0,
    ffi:'PyList_GetItem'(PyList, Index, PyItem),
    PyItem \= 0,  % Check for NULL (index error)
    pyobject_to_prolog_value(PyItem, Value).
```

**4. Implement list mutation predicates**

```prolog
%% py_list_set(+PyList, +Index, +Value)
% Set an item in a Python list by index.
% MEMORY: PyList_SetItem STEALS reference to item!
py_list_set(PyList, Index, Value) :-
    integer(Index),
    Index >= 0,
    prolog_value_to_pyobject(Value, PyValue),
    % PyList_SetItem steals reference, so we DON'T decref
    ffi:'PyList_SetItem'(PyList, Index, PyValue, Result),
    Result = 0.  % 0 = success

%% py_list_append(+PyList, +Value)
% Append an item to the end of a Python list.
% MEMORY: PyList_Append does NOT steal reference, so we must manage it.
py_list_append(PyList, Value) :-
    prolog_value_to_pyobject(Value, PyValue),
    setup_call_cleanup(
        true,
        (   ffi:'PyList_Append'(PyList, PyValue, Result),
            Result = 0
        ),
        py_xdecref(PyValue)
    ).

%% py_list_insert(+PyList, +Index, +Value)
% Insert an item at the specified index.
% MEMORY: PyList_Insert does NOT steal reference.
py_list_insert(PyList, Index, Value) :-
    integer(Index),
    Index >= 0,
    prolog_value_to_pyobject(Value, PyValue),
    setup_call_cleanup(
        true,
        (   ffi:'PyList_Insert'(PyList, Index, PyValue, Result),
            Result = 0
        ),
        py_xdecref(PyValue)
    ).
```

**5. Implement conversion predicates**

```prolog
%% prolog_to_py_list(+PrologList, -PyList)
% Convert a Prolog list to a Python list (deep copy).
% MEMORY: Returns a NEW reference - caller must py_xdecref(PyList) when done!
prolog_to_py_list(PrologList, PyList) :-
    is_list(PrologList),
    length(PrologList, Len),
    py_list_new(PyList),
    prolog_to_py_list_helper(PrologList, PyList, 0).

prolog_to_py_list_helper([], _, _).
prolog_to_py_list_helper([H|T], PyList, Index) :-
    (   is_list(H)
    ->  prolog_to_py_list(H, PyH),
        setup_call_cleanup(
            true,
            ffi:'PyList_SetItem'(PyList, Index, PyH, _),
            true  % SetItem steals ref, no cleanup needed
        )
    ;   prolog_value_to_pyobject(H, PyH),
        ffi:'PyList_SetItem'(PyList, Index, PyH, _)
    ),
    NextIndex is Index + 1,
    prolog_to_py_list_helper(T, PyList, NextIndex).

%% py_list_to_prolog(+PyList, -PrologList)
% Convert a Python list to a Prolog list (deep copy).
py_list_to_prolog(PyList, PrologList) :-
    ffi:'PyList_Size'(PyList, Size),
    py_list_to_prolog_helper(PyList, 0, Size, PrologList).

py_list_to_prolog_helper(_, Size, Size, []) :- !.
py_list_to_prolog_helper(PyList, Index, Size, [H|T]) :-
    Index < Size,
    ffi:'PyList_GetItem'(PyList, Index, PyItem),
    pyobject_to_prolog_value(PyItem, H),
    NextIndex is Index + 1,
    py_list_to_prolog_helper(PyList, NextIndex, Size, T).
```

**6. Implement iteration predicates**

```prolog
%% py_list_member(?Item, +PyList)
% True if Item is a member of PyList.
% On backtracking, iterates through all items.
py_list_member(Item, PyList) :-
    ffi:'PyList_Size'(PyList, Size),
    between(0, Size, Index),
    py_list_get(PyList, Index, Item).

%% py_list_nth0(?Index, +PyList, ?Item)
% True if Item is at Index in PyList.
% Works in multiple modes: (+,+,?), (+,?,+), (?,+,+)
py_list_nth0(Index, PyList, Item) :-
    (   integer(Index)
    ->  py_list_get(PyList, Index, Item)
    ;   ffi:'PyList_Size'(PyList, Size),
        between(0, Size, Index),
        py_list_get(PyList, Index, Item)
    ).
```

### Verification (REFACTOR)

**Run tests**:
```bash
scryer-prolog tests/unit/test_py_list.pl
```

**Expected output**: All 20 tests pass

**Refactoring checklist**:
- [ ] Review memory management (especially SetItem stealing refs)
- [ ] Add error handling for index out of bounds
- [ ] Add documentation comments
- [ ] Optimize nested list conversion
- [ ] Add integration tests
- [ ] Update demos with list examples

---

## Type 3: Python Tuples

### Design Notes

**Similarity to Lists**: Tuples are immutable lists
- Reuse conversion patterns from lists
- No mutation predicates (set, append, insert)
- Otherwise nearly identical API

### Test Plan (RED)

**File**: `tests/unit/test_py_tuple.pl`

**Test Suite**:

```prolog
:- use_module('../../src/lib/python').
:- use_module('../test_framework').
:- initialization(test_py_tuple).

test_py_tuple :-
    reset_test_state,

    % === Creation Tests ===
    run_test('Create tuple from Prolog list', test_tuple_from_prolog),
    run_test('Create empty tuple', test_tuple_empty),
    run_test('Create tuple with size', test_tuple_new_size),

    % === Access Tests ===
    run_test('Get tuple size', test_tuple_size),
    run_test('Get item by index', test_tuple_get_item),
    run_test('Get item out of bounds fails', test_tuple_get_bounds),

    % === Immutability Tests ===
    run_test('Tuple cannot be modified', test_tuple_immutable),

    % === Conversion Tests ===
    run_test('Convert tuple to Prolog', test_tuple_to_prolog),
    run_test('Round-trip conversion', test_tuple_roundtrip),
    run_test('Nested tuple conversion', test_tuple_nested),

    % === Iteration Tests ===
    run_test('Iterate with member/2', test_tuple_member),
    run_test('Iterate with nth0/3', test_tuple_nth0),

    % === Comparison Tests ===
    run_test('Tuple vs list distinction', test_tuple_vs_list),

    report_results.

% Test implementations...

test_tuple_from_prolog :-
    py_initialize,
    prolog_to_py_tuple([1, 2, 3], PyTuple),
    py_tuple_size(PyTuple, Size),
    Size = 3,
    py_xdecref(PyTuple),
    py_finalize.

test_tuple_empty :-
    py_initialize,
    py_tuple_new(0, Tuple),
    py_tuple_size(Tuple, Size),
    Size = 0,
    py_xdecref(Tuple),
    py_finalize.

test_tuple_new_size :-
    py_initialize,
    py_tuple_new(5, Tuple),
    py_tuple_size(Tuple, Size),
    Size = 5,
    py_xdecref(Tuple),
    py_finalize.

test_tuple_size :-
    py_initialize,
    prolog_to_py_tuple([a, b, c], Tuple),
    py_tuple_size(Tuple, Size),
    Size = 3,
    py_xdecref(Tuple),
    py_finalize.

test_tuple_get_item :-
    py_initialize,
    prolog_to_py_tuple([10, 20, 30], Tuple),
    py_tuple_get(Tuple, 0, First),
    py_tuple_get(Tuple, 2, Third),
    First = 10,
    Third = 30,
    py_xdecref(Tuple),
    py_finalize.

test_tuple_get_bounds :-
    py_initialize,
    prolog_to_py_tuple([1, 2], Tuple),
    \+ py_tuple_get(Tuple, 99, _),
    py_xdecref(Tuple),
    py_finalize.

test_tuple_immutable :-
    py_initialize,
    prolog_to_py_tuple([1, 2, 3], Tuple),
    % Verify no set predicate exists
    \+ current_predicate(py_tuple_set/3),
    py_xdecref(Tuple),
    py_finalize.

test_tuple_to_prolog :-
    py_initialize,
    prolog_to_py_tuple([1, 2, 3], PyTuple),
    py_tuple_to_prolog(PyTuple, Result),
    Result = [1, 2, 3],
    py_xdecref(PyTuple),
    py_finalize.

test_tuple_roundtrip :-
    py_initialize,
    Original = [a, b, c, d],
    prolog_to_py_tuple(Original, PyTuple),
    py_tuple_to_prolog(PyTuple, Result),
    Result = Original,
    py_xdecref(PyTuple),
    py_finalize.

test_tuple_nested :-
    py_initialize,
    Original = [1, [2, 3], 4],
    prolog_to_py_tuple(Original, PyTuple),
    py_tuple_to_prolog(PyTuple, Result),
    Result = Original,
    py_xdecref(PyTuple),
    py_finalize.

test_tuple_member :-
    py_initialize,
    prolog_to_py_tuple([a, b, c], Tuple),
    py_tuple_member(b, Tuple),
    py_xdecref(Tuple),
    py_finalize.

test_tuple_nth0 :-
    py_initialize,
    prolog_to_py_tuple([10, 20, 30], Tuple),
    py_tuple_nth0(1, Tuple, Item),
    Item = 20,
    py_xdecref(Tuple),
    py_finalize.

test_tuple_vs_list :-
    py_initialize,
    prolog_to_py_list([1, 2, 3], List),
    prolog_to_py_tuple([1, 2, 3], Tuple),
    % They should be different Python types
    \+ List = Tuple,
    py_xdecref(List),
    py_xdecref(Tuple),
    py_finalize.
```

### Implementation Steps (GREEN)

**1. Add FFI bindings**

```prolog
% Add to FFI bindings section
use_foreign_module(
    LibPath,
    [
        % Existing bindings...

        % Tuple operations
        'PyTuple_New'(size: long) -> address,
        'PyTuple_Size'(tuple: address) -> long,
        'PyTuple_GetItem'(tuple: address, index: long) -> address,  % BORROWED
        'PyTuple_SetItem'(tuple: address, index: long, item: address) -> int
    ],
    [scope(global)]
).
```

**2. Implement tuple creation**

```prolog
%% py_tuple_new(+Size, -PyTuple)
% Create a new Python tuple with the given size.
% MEMORY: Returns a NEW reference - caller must py_xdecref(PyTuple) when done!
py_tuple_new(Size, PyTuple) :-
    integer(Size),
    Size >= 0,
    ffi:'PyTuple_New'(Size, PyTuple),
    PyTuple \= 0.
```

**3. Implement tuple access**

```prolog
%% py_tuple_size(+PyTuple, -Size)
% Get the size of a Python tuple.
py_tuple_size(PyTuple, Size) :-
    ffi:'PyTuple_Size'(PyTuple, Size).

%% py_tuple_get(+PyTuple, +Index, -Value)
% Get an item from a Python tuple by index.
% MEMORY: PyTuple_GetItem returns BORROWED reference.
py_tuple_get(PyTuple, Index, Value) :-
    integer(Index),
    Index >= 0,
    ffi:'PyTuple_GetItem'(PyTuple, Index, PyItem),
    PyItem \= 0,
    pyobject_to_prolog_value(PyItem, Value).
```

**4. Implement conversions**

```prolog
%% prolog_to_py_tuple(+PrologList, -PyTuple)
% Convert a Prolog list to a Python tuple (deep copy).
% MEMORY: Returns a NEW reference - caller must py_xdecref(PyTuple) when done!
prolog_to_py_tuple(PrologList, PyTuple) :-
    is_list(PrologList),
    length(PrologList, Len),
    py_tuple_new(Len, PyTuple),
    prolog_to_py_tuple_helper(PrologList, PyTuple, 0).

prolog_to_py_tuple_helper([], _, _).
prolog_to_py_tuple_helper([H|T], PyTuple, Index) :-
    (   is_list(H)
    ->  prolog_to_py_tuple(H, PyH),
        ffi:'PyTuple_SetItem'(PyTuple, Index, PyH, _)
    ;   prolog_value_to_pyobject(H, PyH),
        ffi:'PyTuple_SetItem'(PyTuple, Index, PyH, _)
    ),
    NextIndex is Index + 1,
    prolog_to_py_tuple_helper(T, PyTuple, NextIndex).

%% py_tuple_to_prolog(+PyTuple, -PrologList)
% Convert a Python tuple to a Prolog list (deep copy).
py_tuple_to_prolog(PyTuple, PrologList) :-
    ffi:'PyTuple_Size'(PyTuple, Size),
    py_tuple_to_prolog_helper(PyTuple, 0, Size, PrologList).

py_tuple_to_prolog_helper(_, Size, Size, []) :- !.
py_tuple_to_prolog_helper(PyTuple, Index, Size, [H|T]) :-
    Index < Size,
    ffi:'PyTuple_GetItem'(PyTuple, Index, PyItem),
    pyobject_to_prolog_value(PyItem, H),
    NextIndex is Index + 1,
    py_tuple_to_prolog_helper(PyTuple, NextIndex, Size, T).
```

**5. Implement iteration**

```prolog
%% py_tuple_member(?Item, +PyTuple)
% True if Item is a member of PyTuple.
py_tuple_member(Item, PyTuple) :-
    ffi:'PyTuple_Size'(PyTuple, Size),
    between(0, Size, Index),
    py_tuple_get(PyTuple, Index, Item).

%% py_tuple_nth0(?Index, +PyTuple, ?Item)
% True if Item is at Index in PyTuple.
py_tuple_nth0(Index, PyTuple, Item) :-
    (   integer(Index)
    ->  py_tuple_get(PyTuple, Index, Item)
    ;   ffi:'PyTuple_Size'(PyTuple, Size),
        between(0, Size, Index),
        py_tuple_get(PyTuple, Index, Item)
    ).
```

### Verification (REFACTOR)

**Run tests**:
```bash
scryer-prolog tests/unit/test_py_tuple.pl
```

**Expected output**: All 13 tests pass

**Refactoring checklist**:
- [ ] Ensure SetItem ref stealing handled correctly
- [ ] Add error handling
- [ ] Add documentation
- [ ] Consider tuple unpacking syntax
- [ ] Add integration tests
- [ ] Update demos

---

## Integration Testing

After all three types are implemented, create integration tests.

**File**: `tests/integration/test_phase1_types.pl`

```prolog
test_mixed_types :-
    py_initialize,

    % Create dict with None, lists, and tuples
    py_dict_new(Dict),
    py_dict_set(Dict, none_val, none),
    prolog_to_py_list([1, 2, 3], List),
    py_dict_set(Dict, list_val, List),
    prolog_to_py_tuple([a, b, c], Tuple),
    py_dict_set(Dict, tuple_val, Tuple),

    % Verify retrieval
    py_dict_get(Dict, none_val, N),
    py_dict_get(Dict, list_val, L),
    py_dict_get(Dict, tuple_val, T),

    N = none,
    py_list_to_prolog(L, [1, 2, 3]),
    py_tuple_to_prolog(T, [a, b, c]),

    % Cleanup
    py_xdecref(List),
    py_xdecref(Tuple),
    py_xdecref(Dict),
    py_finalize.
```

---

## Documentation Updates

After implementation, update:

1. **README.md** - Add Phase 1 features to feature list
2. **ARCHITECTURE.md** - Document list/tuple memory patterns
3. **PACKAGE.md** - Bump to v0.4.0
4. **ROADMAP_TYPE_BRIDGING.md** - Mark Phase 1 complete
5. **examples/python_demo_v3.pl** - Create demo showing new types

---

## Success Criteria

**Phase 1 is complete when**:
- [ ] All 38 unit tests pass (5 None + 20 List + 13 Tuple)
- [ ] Integration tests pass
- [ ] Memory stress tests pass (1000+ iterations)
- [ ] Documentation updated
- [ ] Examples created
- [ ] CI/CD passing

---

## Next Steps After Phase 1

**Phase 2: Object Access (v0.5.0)**
- Attribute access (`py_get_attr`, `py_set_attr`)
- Item access (`py_get_item`, `py_set_item`)
- Directory listing (`py_dir`)

This sets foundation for working with arbitrary Python objects!
