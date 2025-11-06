# ScryPy Type Bridging Roadmap

**Status**: Planning Document
**Created**: 2025-11-06
**Based on**: libpython-clj analysis

---

## Overview

This document outlines the roadmap for expanding ScryPy's type conversion and object bridging capabilities, drawing inspiration from libpython-clj's approach while adapting for Prolog's unique paradigm.

## Current State (v0.3.0)

### Implemented Type Conversions

**Prolog → Python**:
- Atoms → Python strings (`PyUnicode_FromString`)
- Integers → Python ints (`PyLong_FromLong`)
- Floats → Python floats (`PyFloat_FromDouble`)
- Booleans (true/false) → Python True/False

**Python → Prolog**:
- Python strings → Atoms (`PyUnicode_AsUTF8`)
- Python ints → Integers (`PyLong_AsLong`)
- Python floats → Floats (`PyFloat_AsDouble`)
- Python booleans → true/false (`PyObject_IsTrue`)

**Collections**:
- Prolog lists of key-value pairs ↔ Python dicts
- Basic dict operations: create, set, get, keys iteration

### Limitations

- No support for Python lists/tuples
- No support for Python sets
- No support for Python None
- No support for Python objects (arbitrary classes)
- No support for Python iterators/generators
- No attribute access (`.` operator)
- No item access (`[]` operator)
- No function call support
- No NumPy array integration

---

## Inspiration: libpython-clj's Approach

### Key Insights from libpython-clj

1. **Bridging vs Converting**:
   - **Converting**: Copy data between languages (for primitives)
   - **Bridging**: Wrap Python objects with native interfaces (for collections/objects)
   - Example: Python dicts implement `java.util.Map`, allowing native Java operations

2. **Type System**:
   - Comprehensive type detection using Python C API
   - Multimethods dispatching on Python type
   - Support for: int, float, str, bool, range, list, tuple, dict, set, None
   - Special handling for NumPy arrays

3. **Object Access**:
   - `get-attr` / `py.` - Attribute access (`.` operator)
   - `get-item` - Item access (`[]` operator)
   - `call-attr` - Method calls
   - `dir` - List available attributes

4. **Collections**:
   - Python lists/tuples → Clojure sequences (iterable)
   - Python dicts → java.util.Map
   - Python sets → java.util.Set
   - Lazy iteration via iterator protocol

5. **Performance**:
   - Zero-copy for NumPy arrays via shared memory
   - GC integration (Python refs released when JVM objects collected)
   - Fast-callable wrappers for repeated function calls

6. **Syntactic Sugar**:
   - `py.` - Call method with kwargs support
   - `py..` - Chained attribute/method access
   - `->python` / `->jvm` - Explicit conversion
   - `require-python` - Import Python modules as Clojure namespaces

---

## Prolog-Specific Considerations

### Fundamental Differences

1. **No Mutable State**:
   - Prolog variables are logical, not mutable
   - Can't directly represent Python object mutation
   - Need to think in terms of state threading

2. **Relational vs Imperative**:
   - Prolog is declarative/relational
   - Python is imperative/multi-paradigm
   - Challenge: How to represent Python OOP in Prolog?

3. **Unification**:
   - Prolog's core is unification
   - Could use unification for pattern matching Python data
   - Example: `py_list_match([H|T], PyList)` could destructure Python lists

4. **Backtracking**:
   - Prolog backtracks through choice points
   - Could Python iterations participate in backtracking?
   - Example: `py_list_member(X, PyList)` could iterate on backtrack

5. **No OOP**:
   - Prolog doesn't have objects/classes
   - Need to represent Python objects as opaque handles or structured terms

### Design Questions to Explore

1. **Object Representation**:
   - **Option A**: Opaque pointer handles (current approach)
     - Pro: Simple, safe
     - Con: Requires explicit access predicates
   - **Option B**: Structured terms mimicking Python objects
     - Pro: Feels more Prolog-native
     - Con: Complex, memory intensive
   - **Option C**: Hybrid (pointers + cached attributes as terms)

2. **None Handling**:
   - Map to `[]` (empty list)?
   - Map to special atom `none` or `py_none`?
   - Map to `false`?
   - Fail the predicate?

3. **Mutation**:
   - Thread state through predicates: `py_list_append(ListIn, Item, ListOut)`?
   - Allow side effects: `py_list_append(List, Item)` (mutates in place)?
   - Both?

4. **Collections**:
   - Map Python lists to Prolog lists (deep copy)?
   - Keep as opaque PyObject with iteration predicates?
   - Both (conversion on demand)?

---

## Proposed Roadmap

### Phase 1: Core Type Expansion (v0.4.0)

**Priority**: High
**Goal**: Support basic Python collection types

#### 1.1 Python None

```prolog
% Add to type conversion
py_none/1.                    % Check if PyObject is None
prolog_value_to_pyobject(none, PyNone).  % atom 'none' → Python None
pyobject_to_prolog_value(PyNone, none).  % Python None → atom 'none'
```

**FFI Functions Needed**:
- `Py_None` (macro - need to expose via function or constant)

#### 1.2 Python Lists

```prolog
% Creation
py_list_new(-PyList).         % Create empty list
py_list_from_prolog(+List, -PyList).  % [1,2,3] → Python list

% Access
py_list_length(+PyList, -Len).
py_list_get(+PyList, +Index, -Value).
py_list_set(+PyList, +Index, +Value).

% Conversion
py_list_to_prolog(+PyList, -PrologList).  % Deep copy
prolog_to_py_list(+PrologList, -PyList).  % Deep copy

% Iteration (relational style - backtracks)
py_list_member(?Item, +PyList).
py_list_nth0(?Index, +PyList, ?Item).
```

**FFI Functions Needed**:
- `PyList_New(size)` → ptr
- `PyList_Size(list)` → long
- `PyList_GetItem(list, index)` → ptr (BORROWED ref)
- `PyList_SetItem(list, index, item)` → int
- `PyList_Append(list, item)` → int

#### 1.3 Python Tuples

```prolog
py_tuple_new(+Size, -PyTuple).
py_tuple_from_prolog(+List, -PyTuple).
py_tuple_get(+PyTuple, +Index, -Value).
py_tuple_to_prolog(+PyTuple, -PrologList).
```

**FFI Functions Needed**:
- `PyTuple_New(size)` → ptr
- `PyTuple_Size(tuple)` → long
- `PyTuple_GetItem(tuple, index)` → ptr (BORROWED ref)
- `PyTuple_SetItem(tuple, index, item)` → int

### Phase 2: Object Access (v0.5.0)

**Priority**: High
**Goal**: Access Python object attributes and items

#### 2.1 Attribute Access

```prolog
% Get/set attributes
py_get_attr(+PyObj, +AttrName, -Value).
py_set_attr(+PyObj, +AttrName, +Value).
py_has_attr(+PyObj, +AttrName).
py_del_attr(+PyObj, +AttrName).

% List attributes
py_dir(+PyObj, -AttrList).

% Example usage:
% ?- import_module("sys", Sys),
%    py_get_attr(Sys, version, Version).
```

**FFI Functions Needed**:
- `PyObject_GetAttrString(obj, name)` → ptr
- `PyObject_SetAttrString(obj, name, value)` → int
- `PyObject_HasAttrString(obj, name)` → int
- `PyObject_DelAttrString(obj, name)` → int
- `PyObject_Dir(obj)` → ptr (list of attr names)

#### 2.2 Item Access (Subscript)

```prolog
% Get/set items (for __getitem__/__setitem__)
py_get_item(+PyObj, +Key, -Value).
py_set_item(+PyObj, +Key, +Value).
py_del_item(+PyObj, +Key).

% Example usage:
% ?- py_dict_new(D),
%    py_set_item(D, "key", "value"),
%    py_get_item(D, "key", Val).
```

**FFI Functions Needed**:
- `PyObject_GetItem(obj, key)` → ptr
- `PyObject_SetItem(obj, key, value)` → int
- `PyObject_DelItem(obj, key)` → int

### Phase 3: Function Calls (v0.6.0)

**Priority**: High
**Goal**: Call Python functions and methods

#### 3.1 Basic Function Calls

```prolog
% Call function with positional args
py_call(+Callable, +ArgsList, -Result).

% Call with positional + keyword args
py_call_kw(+Callable, +ArgsList, +KwargsDict, -Result).

% Example:
% ?- import_module("math", Math),
%    py_get_attr(Math, sqrt, Sqrt),
%    py_call(Sqrt, [16.0], Result).
%    % Result = 4.0
```

**FFI Functions Needed**:
- `PyObject_Call(callable, args, kwargs)` → ptr
- `PyObject_CallObject(callable, args)` → ptr
- Already have: `PyTuple_New` for args

#### 3.2 Method Calls (Sugar)

```prolog
% Call method on object
py_call_method(+PyObj, +MethodName, +Args, -Result).
py_call_method_kw(+PyObj, +MethodName, +Args, +Kwargs, -Result).

% Example:
% ?- py_list_new(L),
%    py_call_method(L, append, [42], _),
%    py_list_to_prolog(L, [42]).
```

### Phase 4: Iteration & Generators (v0.7.0)

**Priority**: Medium
**Goal**: Iterate Python sequences via backtracking

#### 4.1 Iterator Protocol

```prolog
% Get iterator from iterable
py_iter(+PyIterable, -PyIterator).

% Iterate (backtracks through items)
py_iter_next(+PyIterator, ?Item).

% Example - iterate on backtrack:
% ?- py_iter(PyList, Iter),
%    py_iter_next(Iter, Item),
%    writeln(Item),
%    fail.
```

**FFI Functions Needed**:
- `PyObject_GetIter(obj)` → ptr
- `PyIter_Next(iter)` → ptr (NULL + StopIteration on end)

#### 4.2 High-Level Iteration

```prolog
% Iterate any iterable (list, tuple, generator, etc.)
py_iterate(?Item, +PyIterable).

% Collect all items into Prolog list
py_collect(+PyIterable, -PrologList).

% Example:
% ?- py_run_simple_string("x = range(5)"),
%    py_call(X, range, [5], RangeObj),
%    py_collect(RangeObj, Items).
%    % Items = [0, 1, 2, 3, 4]
```

### Phase 5: Import & Module Management (v0.8.0)

**Priority**: Medium
**Goal**: Better module import and management

#### 5.1 Module Import

```prolog
% Import module
py_import(+ModuleName, -Module).

% From-import
py_from_import(+ModuleName, +Name, -Object).
py_from_import(+ModuleName, +Names:list, -Objects:list).

% Example:
% ?- py_from_import(math, [sqrt, pi], [Sqrt, Pi]).
```

**FFI Functions Needed**:
- `PyImport_ImportModule(name)` → ptr
- `PyImport_Import(name)` → ptr

#### 5.2 Module Introspection

```prolog
% Get module dict
py_module_dict(+Module, -Dict).

% List module contents
py_module_contents(+Module, -Names).
```

Already have:
- `PyImport_AddModule` ✓
- `PyModule_GetDict` ✓

### Phase 6: Advanced Types (v0.9.0)

**Priority**: Low
**Goal**: Support more Python types

#### 6.1 Sets

```prolog
py_set_new(-PySet).
py_set_from_prolog(+List, -PySet).
py_set_to_prolog(+PySet, -List).
py_set_add(+PySet, +Item).
py_set_contains(+PySet, +Item).
```

**FFI Functions Needed**:
- `PySet_New(iterable)` → ptr
- `PySet_Add(set, key)` → int
- `PySet_Contains(set, key)` → int
- `PySet_Size(set)` → long

#### 6.2 Bytes & Bytearray

```prolog
py_bytes_from_string(+Str, -PyBytes).
py_bytes_to_string(+PyBytes, -Str).
```

**FFI Functions Needed**:
- `PyBytes_FromString(str)` → ptr
- `PyBytes_AsString(bytes)` → cstr
- `PyBytes_Size(bytes)` → long

### Phase 7: NumPy Integration (v1.0.0)

**Priority**: High (for scientific computing)
**Goal**: Zero-copy NumPy array access

#### 7.1 NumPy Array Basics

```prolog
% Create NumPy array from Prolog list
numpy_array(+List, -NpArray).
numpy_array(+List, +Dtype, -NpArray).

% Access array data
numpy_shape(+NpArray, -Shape).
numpy_dtype(+NpArray, -Dtype).
numpy_size(+NpArray, -Size).

% Convert to Prolog
numpy_to_list(+NpArray, -List).
```

**FFI Functions Needed**:
- NumPy C API (complex, separate subsystem)
- `PyArray_SimpleNew`
- `PyArray_DATA`
- `PyArray_SHAPE`
- `PyArray_TYPE`

---

## Implementation Strategy

### Design Principles

1. **Start Simple**: Primitives before collections before objects
2. **Prolog-Native**: Make it feel like Prolog, not like Python-in-Prolog
3. **Safe by Default**: Proper memory management always
4. **Relational Where Possible**: Use backtracking for iteration
5. **Explicit When Necessary**: State threading for mutations

### Compatibility Strategy

- **Backward Compatible**: New predicates don't break existing code
- **Semantic Versioning**: Minor bumps for new features, major for breaking changes
- **Deprecation Policy**: Mark old APIs as deprecated before removing

### Testing Strategy

- Add test for each new type conversion
- Test memory management (no leaks via stress tests)
- Test edge cases (None, empty collections, nested structures)
- Test backtracking behavior for iterators

---

## Questions to Resolve

### 1. Object Lifetime & GC

**Question**: How do we manage Python object lifetimes in Prolog?

**Options**:
- A) Manual: User calls `py_decref` explicitly (current)
- B) Automatic: Setup finalizers via Scryer GC hooks (if available)
- C) Scope-based: `with_python_object(Obj, Goal)` pattern

**Recommendation**: Explore option C for ergonomics while keeping A as fallback.

### 2. None Representation

**Question**: What Prolog term represents Python None?

**Options**:
- A) `none` atom
- B) `[]` empty list
- C) `false`
- D) Fail the predicate

**Recommendation**: Use `none` atom for explicitness, consider `false` for boolean context.

### 3. List Conversion Strategy

**Question**: Deep copy or lazy reference?

**Options**:
- A) Always deep copy: `py_list_to_prolog` copies all items
- B) Lazy: Keep PyObject, provide iteration predicates
- C) Hybrid: User chooses via predicate name

**Recommendation**: C - provide both `py_list_to_prolog/2` (copy) and `py_list_iterate/2` (lazy).

### 4. Mutation Semantics

**Question**: How to handle mutable operations?

**Options**:
- A) Pure: `py_list_append(ListIn, Item, ListOut)` (creates new ref)
- B) Impure: `py_list_append(List, Item)` (mutates in place)
- C) Both: Different predicate names

**Recommendation**: B for simplicity, matches Python semantics. Document clearly.

### 5. Error Handling

**Question**: What to do when Python raises exception?

**Current**: Throw Prolog exception with Python error message
**Future**: Consider capturing exception as term for pattern matching?

---

## Success Metrics

### v0.4.0 (Core Types)
- [ ] Python None support
- [ ] Python list creation, access, iteration
- [ ] Python tuple creation, access
- [ ] Prolog list ↔ Python list conversion
- [ ] 95%+ test coverage

### v0.5.0 (Object Access)
- [ ] Get/set attributes on any Python object
- [ ] Get/set items via `[]` operator
- [ ] `py_dir` listing
- [ ] Can access sys.version, os.environ, etc.

### v0.6.0 (Function Calls)
- [ ] Call any Python function
- [ ] Call with keyword arguments
- [ ] Method call sugar
- [ ] Can call math.sqrt, str.upper, etc.

### v0.7.0 (Iteration)
- [ ] Backtracking iteration over Python sequences
- [ ] Generator support
- [ ] Range support
- [ ] Lazy evaluation where appropriate

### v1.0.0 (Stable API)
- [ ] All core types supported
- [ ] NumPy basic integration
- [ ] Comprehensive documentation
- [ ] Production-ready

---

## Next Steps

1. **Discuss & Decide**: Review this roadmap, make decisions on design questions
2. **Prototype**: Implement Phase 1 (None + Lists) as proof of concept
3. **Test**: Ensure memory safety and correct behavior
4. **Iterate**: Refine based on learnings
5. **Document**: Update ARCHITECTURE.md with new patterns

---

## References

- [libpython-clj Usage](https://github.com/clj-python/libpython-clj/blob/master/topics/Usage.md)
- [Python C API Reference](https://docs.python.org/3/c-api/)
- [Current ScryPy Implementation](src/lib/python.pl)
- [ARCHITECTURE.md](docs/ARCHITECTURE.md)
