# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commit Attribution Policy

**NEVER include Claude attribution in commits.** Do not add:
- "🤖 Generated with [Claude Code](https://claude.com/claude-code)"
- "Co-Authored-By: Claude <noreply@anthropic.com>"
- Any other AI attribution

Write normal, professional commit messages as if written by a human developer.

**README attribution**: The README.md notes that some documentation is procedurally generated to ground and guide development. This is sufficient acknowledgment.

## Project Status: Active Research

**THIS IS A RESEARCH PROJECT WITH UNSTABLE API/ARCHITECTURE/GOALS.**

This library explores Python integration for Scryer Prolog, largely based on [libpython-clj](https://github.com/clj-python/libpython-clj). Many open questions exist about reconciling declarative logic programming with Python's multi-paradigm, imperative nature. Expect rapid iteration and breaking changes.

## Core Architecture

### FFI-Based Direct Python C API Calls

The library uses **no C wrapper code**. Instead, it calls Python C API functions directly via Scryer Prolog's FFI (`library(ffi)`):

```
Prolog Code (user.pl)
    ↓ :- use_module('src/lib/python')
python.pl (our module)
    ↓ ffi:'PyDict_New'(...)
Scryer FFI (library(ffi))
    ↓ use_foreign_module(...)
libpython3.10.so (Python C API)
```

**Key insight**: Functions work, macros don't. `Py_XDecRef` is a C macro, so we implement it in Prolog. Type-check macros (`PyLong_Check`, etc.) aren't available, so we use a try-convert approach with `PyErr_Occurred`.

### Memory Management: Reference Counting

Python uses reference counting for garbage collection. **This is critical to understand:**

**NEW references** (we own, must decref when done):
- `PyDict_New()`, `PyLong_FromLong()`, `PyFloat_FromDouble()`, `PyUnicode_FromString()`
- `PyDict_Keys()`, `PyObject_Type()`, `PyRun_String()`

**BORROWED references** (we don't own, must NOT decref):
- `PyDict_GetItemString()`, `PyList_GetItem()`, `PyModule_GetDict()`

All predicates that create Python objects use `setup_call_cleanup` to ensure automatic cleanup even on errors:

```prolog
py_dict_set(DictPtr, Key, Value) :-
    prolog_value_to_pyobject(Value, PyValue),  % NEW ref!
    setup_call_cleanup(
        true,
        ffi:'PyDict_SetItemString'(DictPtr, Key, PyValue, Result),
        py_xdecref(PyValue)  % Always cleanup
    ).
```

For `py_run_simple_string/5`, we track **ownership** - only decref NEW refs, not BORROWED ones from `PyModule_GetDict`.

### State Management: Blackboard Abstraction

Uses `library(iso_ext)`'s blackboard system (`bb_put/bb_get`) instead of `dynamic`/`assert`/`retract`:

```prolog
% Internal abstraction
python_state_set(Key, Value) :- bb_put(Key, Value).
python_state_get(Key, Value) :- bb_get(Key, Value).

% Public API
is_python_initialized :- python_state_check(python_initialized).
```

**Why**: Cleaner, thread-safe, easy to swap implementation later.

### Module Structure (src/lib/python.pl)

The single-file module is organized into sections:
1. **State Management Abstraction** - Blackboard-based state tracking
2. **Reference Counting Abstraction** - `py_incref/1`, `py_decref/1`, `py_xdecref/1`, `with_new_pyobject/3`
3. **FFI Bindings** - 23 Python C API functions loaded via `use_foreign_module`
4. **Dictionary Operations** - Create/manipulate Python dicts, convert to/from Prolog lists
5. **Type Conversion** - Bidirectional Prolog ↔ Python (atoms, integers, floats, booleans)
6. **Code Execution** - `py_run_simple_string/1` (simple) and `/5` (with globals/locals)

## Development Commands

### Running Tests

```bash
# Core functionality tests
scryer-prolog test_all_types.pl           # Type conversion (atoms, ints, floats)
scryer-prolog test_dict_to_list.pl        # Dictionary operations
scryer-prolog test_globals_locals.pl      # Globals/locals execution

# Memory management stress tests
scryer-prolog test_memory_management.pl   # 1000+ cycle stress tests

# Examples
scryer-prolog examples/python_demo.pl     # Basic demo (v0.1.0)
scryer-prolog examples/python_demo_v2.pl  # Advanced demo (v0.2.0)
```

### Interactive Development

```prolog
% Load the library
?- use_module('src/lib/python').

% Initialize Python
?- py_initialize.

% Test a feature
?- py_run_simple_string('x = 42').

% Don't forget to finalize
?- py_finalize.
```

### Debugging FFI Issues

If FFI hangs or crashes:
1. **Check function signatures** - Wrong signature = hang/segfault
2. **Macro vs function** - If `nm -D libpython3.10.so | grep FuncName` returns nothing, it's a macro
3. **Test incrementally** - Add ONE function at a time to `use_foreign_module`
4. **Void functions syntax** - No parentheses: `'Py_Finalize'([], void)` not `'Py_Finalize'`

Example diagnostic:
```bash
nm -D /usr/lib/x86_64-linux-gnu/libpython3.10.so | grep -i "Py_XDecRef"
# No output = it's a macro, implement in Prolog
```

## Critical Patterns

### Adding New Python Object Creation Predicates

When creating predicates that return Python objects (NEW refs):

```prolog
% 1. Document memory ownership
%% my_predicate(-PyObject)
% MEMORY: Returns a NEW reference - caller must py_xdecref(PyObject) when done!

% 2. If caller will use immediately, add cleanup internally
my_predicate_with_use(Result) :-
    create_pyobject(PyObj),  % NEW ref
    setup_call_cleanup(
        true,
        use_pyobject(PyObj, Result),
        py_xdecref(PyObj)  % Cleanup
    ).
```

### Type Conversion: Try-Convert Pattern

Since type-checking macros aren't available via FFI:

```prolog
pyobject_to_prolog_value(PyObject, Value) :-
    % Try string conversion
    ffi:'PyUnicode_AsUTF8'(PyObject, StrResult),
    ffi:'PyErr_Occurred'(Err),
    (Err = 0 -> atom_chars(Value, StrResult)
    ;   % Not string, clear error and try int
        ffi:'PyErr_Clear',
        ffi:'PyLong_AsLong'(PyObject, IntResult),
        ffi:'PyErr_Occurred'(Err2),
        (Err2 = 0 -> Value = IntResult
        ;   % Try float, then boolean...
            ...
        )
    ).
```

### Handling Borrowed References

When working with borrowed references (e.g., from `PyModule_GetDict`):

```prolog
% Track ownership explicitly
(GlobalsIn = [] ->
    ffi:'PyModule_GetDict'(MainModule, GlobalsDict),
    GlobalsOwned = false  % BORROWED ref
;
    prolog_to_py_dict(GlobalsIn, GlobalsDict),
    GlobalsOwned = true   % NEW ref
),

% Only decref if we own it
(GlobalsOwned = true -> py_xdecref(GlobalsDict) ; true)
```

## Key Design Questions Being Explored

These are **active research questions**, not settled decisions:

1. **Imperative vs Declarative**: How do Python mutations interact with Prolog's logical variables?
2. **Type Mapping**: Should Python `None` be `false`, `[]`, a special term, or fail?
3. **Backtracking**: Can/should Python code participate in Prolog backtracking?
4. **Exception Handling**: How do Python exceptions map to Prolog errors?
5. **Multi-Interpreter**: With Python 3.12's per-interpreter GIL, what does parallel execution look like?
6. **API Level**: Expose raw C API or build higher-level abstractions?

When making changes, document trade-offs and alternative approaches. The "right" answer may not exist yet.

## Documentation

- **ARCHITECTURE.md** / **ARCHITECTURE.html** - Comprehensive architecture overview, current state, challenges, roadmap
- **DESIGN_STATE_MANAGEMENT.md** - Memory management and reference counting design
- **README.md** - User-facing API documentation (with big research warning)
- **PACKAGE.md** - Version history and roadmap

## Python Version

The library automatically detects and loads Python 3.10, 3.11, or 3.12 shared libraries. It searches common locations on Linux (x86_64) and macOS (Homebrew on both Intel and Apple Silicon).

Search order:
1. Linux: `/usr/lib/x86_64-linux-gnu/libpython3.{12,11,10}.so`
2. macOS (Apple Silicon): `/opt/homebrew/lib/libpython3.{12,11,10}.dylib`
3. macOS (Intel): `/usr/local/lib/libpython3.{12,11,10}.dylib`

See `python_library_path/1` and `candidate_python_library/1` in `src/lib/python.pl` for implementation details.

## Common Gotchas

1. **Memory leaks**: Always use `setup_call_cleanup` for NEW references
2. **FFI hangs**: Wrong function signature or calling a macro
3. **Segfaults**: Decrefing a BORROWED reference or decrefing twice
4. **Type conversion failures**: Check `PyErr_Occurred` after every conversion attempt
5. **State persistence**: Python state persists between `py_run_simple_string` calls until `py_finalize`
