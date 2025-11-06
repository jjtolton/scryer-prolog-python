# Phase 1 Implementation Roadmap (v0.4.0)

**Status**: RED - Tests written, implementation pending
**Created**: 2025-11-06

---

## Overview

Phase 1 adds core Python type support: None, Lists, and Tuples. Following strict TDD (Test-Driven Development):

1. ✅ **RED**: Tests written and failing (CURRENT STATE)
2. ⏳ **GREEN**: Implement features to pass tests
3. ⏳ **REFACTOR**: Clean up while keeping tests green

## Test Suite Status

**Total Tests**: 38 tests across 3 type categories

### Test Files Created

1. **`tests/unit/test_py_none.pl`** - 8 tests
2. **`tests/unit/test_py_list.pl`** - 18 tests
3. **`tests/unit/test_py_tuple.pl`** - 12 tests
4. **`tests/test_framework.pl`** - Test infrastructure
5. **`tests/test_all.pl`** - Test runner

All tests currently **FAILING** (RED phase) - this is expected and correct!

---

## Implementation Plan

### Step 1: Python None Support (8 tests)

**FFI Functions Needed**:
```prolog
% Get reference to Py_None singleton
% Note: Py_None is a macro, need to get it via PyObject or constant
```

**Predicates to Implement**:
```prolog
% 1.1 None Detection
py_none(+PyObject)              % Check if PyObject is None

% 1.2 None Conversion
prolog_value_to_pyobject(none, -PyNone)   % Extend existing
pyobject_to_prolog_value(+PyNone, none)   % Extend existing
```

**Implementation Strategy**:
- Python's `None` is a singleton object (`Py_None`)
- Need to get pointer to `Py_None` constant
- Compare PyObject pointers for equality
- Map Prolog atom `none` ↔ Python `None`

**Tests**:
1. Detect Python None
2. Reject non-None values
3. Prolog atom `none` → PyNone
4. PyNone → Prolog atom `none`
5. Round-trip conversion
6. None as dict value
7. None as dict key
8. Memory management

---

### Step 2: Python List Support (18 tests)

**FFI Functions Needed**:
```prolog
% Add to use_foreign_module/3 in src/lib/python.pl:
'PyList_New'([long], ptr),              % Create list of size
'PyList_Size'([ptr], long),             % Get length
'PyList_GetItem'([ptr, long], ptr),     % Get item (BORROWED ref)
'PyList_SetItem'([ptr, long, ptr], int), % Set item (steals ref)
'PyList_Append'([ptr, ptr], int),       % Append item
'PyList_Insert'([ptr, long, ptr], int)  % Insert at index
```

**Predicates to Implement**:

```prolog
% 2.1 List Creation
py_list_new(-PyList)
    % Create empty Python list
    % MEMORY: Returns NEW reference

py_list_from_prolog(+PrologList, -PyList)
    % Convert Prolog list to Python list (deep copy)
    % Recursively converts all elements
    % MEMORY: Returns NEW reference

% 2.2 List Access
py_list_length(+PyList, -Length)
    % Get list length via PyList_Size

py_list_get(+PyList, +Index, -Value)
    % Get item at index (0-based, supports negative)
    % MEMORY: Returns BORROWED ref, must convert immediately

py_list_set(+PyList, +Index, +Value)
    % Set item at index (mutates list in place)
    % MEMORY: PyList_SetItem STEALS reference

% 2.3 List Conversion
py_list_to_prolog(+PyList, -PrologList)
    % Convert Python list to Prolog list (deep copy)
    % Recursively converts all elements

prolog_to_py_list(+PrologList, -PyList)
    % Alias for py_list_from_prolog/2

% 2.4 List Iteration (Relational/Backtracking)
py_list_member(?Item, +PyList)
    % Check membership or iterate on backtrack
    % Succeeds for each item in list

py_list_nth0(?Index, +PyList, ?Item)
    % Access by index with backtracking
    % Generates Index-Item pairs on backtrack

% 2.5 List Modification
py_list_append(+PyList, +Item)
    % Append item to end (mutates in place)

py_list_extend(+PyList, +OtherList)
    % Extend with another list (mutates in place)

py_list_insert(+PyList, +Index, +Item)
    % Insert item at index (mutates in place)

py_list_remove(+PyList, +Item)
    % Remove first occurrence of item (mutates in place)
```

**Implementation Details**:

1. **Memory Management**:
   - `PyList_New` returns NEW reference
   - `PyList_GetItem` returns BORROWED reference (don't decref!)
   - `PyList_SetItem` STEALS reference (increments internally)
   - Always use `setup_call_cleanup` for temp objects

2. **Recursive Conversion**:
   ```prolog
   py_list_from_prolog([], PyList) :-
       py_list_new(PyList).
   py_list_from_prolog([H|T], PyList) :-
       py_list_new(PyList),
       py_list_from_prolog_items([H|T], PyList).

   py_list_from_prolog_items([], _).
   py_list_from_prolog_items([H|T], PyList) :-
       prolog_value_to_pyobject(H, PyH),
       setup_call_cleanup(
           true,
           py_list_append(PyList, PyH),
           py_xdecref(PyH)
       ),
       py_list_from_prolog_items(T, PyList).
   ```

3. **Backtracking Iteration**:
   ```prolog
   py_list_member(Item, PyList) :-
       py_list_length(PyList, Len),
       between(0, Len-1, I),
       py_list_get(PyList, I, Item).
   ```

**Tests**:
1. Create empty list
2. Create from Prolog list
3. Mixed types in list
4. Get list length
5. Get by index
6. Negative indexing
7. Set by index
8. Convert to Prolog
9. Round-trip conversion
10. Nested lists
11. Membership test
12. Backtracking iteration
13. nth0 with backtracking
14. Append items
15. Extend lists
16. Insert at index
17. Remove item
18. Memory stress test (1000 cycles)

---

### Step 3: Python Tuple Support (12 tests)

**FFI Functions Needed**:
```prolog
% Add to use_foreign_module/3:
'PyTuple_New'([long], ptr),              % Create tuple of size
'PyTuple_Size'([ptr], long),             % Get length
'PyTuple_GetItem'([ptr, long], ptr),     % Get item (BORROWED ref)
'PyTuple_SetItem'([ptr, long, ptr], int) % Set item (steals ref, init only)
```

**Predicates to Implement**:

```prolog
% 3.1 Tuple Creation
py_tuple_new(+Size, -PyTuple)
    % Create empty tuple of given size
    % MEMORY: Returns NEW reference

py_tuple_from_prolog(+PrologList, -PyTuple)
    % Convert Prolog list to Python tuple (immutable)
    % MEMORY: Returns NEW reference

% 3.2 Tuple Access
py_tuple_length(+PyTuple, -Length)
    % Get tuple length

py_tuple_get(+PyTuple, +Index, -Value)
    % Get item at index (0-based, supports negative)
    % MEMORY: Returns BORROWED ref

% 3.3 Tuple Conversion
py_tuple_to_prolog(+PyTuple, -PrologList)
    % Convert Python tuple to Prolog list

prolog_to_py_tuple(+PrologList, -PyTuple)
    % Alias for py_tuple_from_prolog/2
```

**Implementation Details**:

1. **Immutability**:
   - Tuples are immutable after creation
   - `PyTuple_SetItem` only works during initialization
   - No append/extend/remove operations

2. **Creation Pattern**:
   ```prolog
   py_tuple_from_prolog(List, PyTuple) :-
       length(List, Len),
       ffi:'PyTuple_New'(Len, PyTuple),
       py_tuple_fill(List, PyTuple, 0).

   py_tuple_fill([], _, _).
   py_tuple_fill([H|T], PyTuple, Index) :-
       prolog_value_to_pyobject(H, PyH),
       % PyTuple_SetItem steals reference!
       ffi:'PyTuple_SetItem'(PyTuple, Index, PyH, Result),
       (Result = 0 -> true ; throw(error(python_error, 'Tuple set failed'))),
       NextIndex is Index + 1,
       py_tuple_fill(T, PyTuple, NextIndex).
   ```

**Tests**:
1. Create empty tuple
2. Create from Prolog list
3. Mixed types in tuple
4. Get tuple length
5. Get by index
6. Negative indexing
7. Convert to Prolog
8. Round-trip conversion
9. Nested tuples
10. Immutability verification
11. Tuple equality
12. Memory stress test (1000 cycles)

---

## Implementation Order

### Phase 1.1: Python None (Easiest)
1. Get `Py_None` singleton reference
2. Implement `py_none/1` comparison
3. Extend type conversion predicates
4. Run tests → should be **GREEN**

**Estimated Time**: 30 minutes

### Phase 1.2: Python Lists (Most Complex)
1. Add FFI bindings for PyList_* functions
2. Implement creation predicates
3. Implement access predicates
4. Implement conversion predicates
5. Implement iteration predicates (backtracking)
6. Implement modification predicates
7. Memory management testing
8. Run tests → should be **GREEN**

**Estimated Time**: 2-3 hours

### Phase 1.3: Python Tuples (Similar to Lists)
1. Add FFI bindings for PyTuple_* functions
2. Implement creation predicates (immutable pattern)
3. Implement access predicates
4. Implement conversion predicates
5. Memory management testing
6. Run tests → should be **GREEN**

**Estimated Time**: 1-2 hours

---

## Success Criteria

### All Tests Pass
```bash
scryer-prolog tests/test_all.pl
# Expected output:
# ✅ Passed: 38
# ❌ Failed: 0
```

### Memory Safety
- No segfaults
- No memory leaks (verified by stress tests)
- Proper reference counting
- All `setup_call_cleanup` patterns in place

### Documentation Updated
- [ ] README.md updated with Phase 1 features
- [ ] PACKAGE.md version bumped to 0.4.0
- [ ] ARCHITECTURE.md updated with new patterns
- [ ] Example code added

---

## Known Challenges

### 1. Py_None Singleton Access
**Problem**: `Py_None` is a macro, not a function
**Solution**: May need to access via `PyObject_RichCompareBool` or load constant

### 2. BORROWED References
**Problem**: `PyList_GetItem` and `PyTuple_GetItem` return borrowed refs
**Solution**: Convert immediately, don't store. Never decref borrowed refs.

### 3. Nested Structure Conversion
**Problem**: Recursive conversion of lists/tuples containing lists/tuples
**Solution**: Recursively call conversion predicates, careful with reference counting

### 4. Backtracking with Python Objects
**Problem**: Python objects must be cleaned up even on backtrack failure
**Solution**: Use `setup_call_cleanup` or deterministic resource management

---

## Testing Strategy

### Unit Tests
Each predicate tested in isolation with:
- Happy path (normal usage)
- Edge cases (empty, single item, large)
- Error cases (invalid input, out of bounds)
- Memory management (create/destroy cycles)

### Integration Tests
Cross-feature tests:
- Lists containing tuples containing lists
- None in collections
- Mixed type collections
- Conversion pipelines

### Memory Tests
Stress tests:
- 1000+ create/destroy cycles
- Nested structure cleanup
- Backtracking cleanup
- No segfaults, no leaks

---

## Post-Phase 1

After all tests pass:

1. **Refactor** (if needed)
2. **Document** new features
3. **Commit** with message:
   ```
   Implement Phase 1: Python None, Lists, Tuples (v0.4.0)

   Add core type bridging for Python collections:
   - Python None support (py_none/1, conversions)
   - Python List support (create, access, iterate, modify)
   - Python Tuple support (create, access, immutable)

   All 38 tests passing.
   Memory safe with proper reference counting.
   ```
4. **Push** to GitHub (with permission)
5. **Move to Phase 2**: Object Access (attributes, items)

---

## References

- [ROADMAP_TYPE_BRIDGING.md](ROADMAP_TYPE_BRIDGING.md) - Complete roadmap
- [Python C API - Lists](https://docs.python.org/3/c-api/list.html)
- [Python C API - Tuples](https://docs.python.org/3/c-api/tuple.html)
- [Python C API - None](https://docs.python.org/3/c-api/none.html)
- [CLAUDE.md](CLAUDE.md) - TDD workflow
