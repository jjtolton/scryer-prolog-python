# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commit Attribution Policy

**NEVER include Claude attribution in commits.** Do not add:
- "🤖 Generated with [Claude Code](https://claude.com/claude-code)"
- "Co-Authored-By: Claude <noreply@anthropic.com>"
- Any other AI attribution

Write normal, professional commit messages as if written by a human developer.

**README attribution**: The README.md notes that some documentation is procedurally generated to ground and guide development. This is sufficient acknowledgment.

## Git Push Policy

**NEVER push to GitHub without explicit permission from the user.**

- Always commit changes locally first
- Wait for user to explicitly say "push" or "push to GitHub" before using `git push`
- When work is complete, inform the user that changes are committed and ready to push
- Example: "Changes committed locally. Ready to push to GitHub when you're ready."

## Test-Driven Development (ABSOLUTE REQUIREMENT)

**TESTS FIRST. ALWAYS.**

### Workflow for EVERY Feature

1. **RED**: Write test FIRST. See it FAIL.
2. **GREEN**: Implement minimal code to pass.
3. **REFACTOR**: Clean up while keeping tests green.

**NEVER** write code without writing the test first. This is non-negotiable.

### Test Framework

ScryPy uses a simple, custom test framework (see `tests/test_framework.pl`):

```prolog
:- use_module('../tests/test_framework').

test_feature_name :-
    reset_test_state,
    run_test('description of what is being tested', test_pred_1),
    run_test('another test description', test_pred_2),
    report_results.

test_pred_1 :-
    % Arrange
    Input = ...,
    Expected = ...,

    % Act
    actual_predicate(Input, Actual),

    % Assert
    Actual = Expected.
```

### Running Tests

```bash
# Run all type bridging tests
scryer-prolog tests/test_all.pl

# Run specific test suites
scryer-prolog tests/unit/test_py_none.pl
scryer-prolog tests/unit/test_py_list.pl
scryer-prolog tests/unit/test_py_tuple.pl

# Memory management stress tests
scryer-prolog tests/integration/test_memory.pl
```

### Test Organization

```
tests/
├── test_framework.pl          # Test infrastructure
├── test_all.pl               # Run all tests
├── unit/                     # Unit tests for individual predicates
│   ├── test_py_none.pl
│   ├── test_py_list.pl
│   ├── test_py_tuple.pl
│   └── test_py_attr.pl
├── integration/              # Cross-feature integration tests
│   ├── test_type_conversion.pl
│   ├── test_memory.pl
│   └── test_full_pipeline.pl
└── fixtures/                 # Test data and helpers
    └── python_test_values.pl
```

### Test Patterns

**Pure Transformation Test**:
```prolog
test(list_to_pylist) :-
    prolog_to_py_list([1, 2, 3], PyList),
    py_list_to_prolog(PyList, Result),
    Result = [1, 2, 3].
```

**Memory Management Test**:
```prolog
test(list_cleanup) :-
    % Create and destroy 1000 times
    between(1, 1000, _),
    py_list_new(L),
    py_xdecref(L),
    fail.
test(list_cleanup).  % Succeeds after backtracking
```

**Error Handling Test**:
```prolog
test(invalid_index_fails) :-
    py_list_from_prolog([1,2,3], L),
    \+ py_list_get(L, 99, _),  % Out of bounds should fail
    py_xdecref(L).
```

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
    ↓ use_foreign_module(LibPath, [...], [scope(global)])
libpython3.XX.so (Python C API - loaded with RTLD_GLOBAL)
```

**Key insight**: Functions work, macros don't. `Py_XDecRef` is a C macro, so we implement it in Prolog. Type-check macros (`PyLong_Check`, etc.) aren't available, so we use a try-convert approach with `PyErr_Occurred`.

**RTLD_GLOBAL (v0.3.0)**: The library now uses `scope(global)` option to load libpython with `RTLD_GLOBAL`, making symbols available for Python C extensions (NumPy, etc.). Requires Scryer Prolog fork with RTLD_GLOBAL support.

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
3. **FFI Bindings** - 23 Python C API functions loaded via `use_foreign_module/3` with `scope(global)`
4. **Dictionary Operations** - Create/manipulate Python dicts, convert to/from Prolog lists
5. **Type Conversion** - Bidirectional Prolog ↔ Python (atoms, integers, floats, booleans)
6. **Code Execution** - `py_run_simple_string/1` (simple) and `/5` (with globals/locals)

## Python Library Configuration

The library uses a 3-tier configuration system to locate the Python shared library:

**Priority (highest to lowest):**
1. **`python.pl`** - User configuration file (git-ignored)
2. **`LIBPYTHON_PATH`** - Environment variable
3. **Auto-detection** - Searches common system locations

### Configuration File (python.pl)

For permanent configuration, create `python.pl` from the example:

```bash
cp python.pl.example python.pl
```

Example configuration:
```prolog
% Define python_library_path_user/1 (multifile predicate)
python_library_path_user('/home/user/miniconda3/envs/myenv/lib/libpython3.11.so').
```

**IMPORTANT**: You must `consult('python.pl')` BEFORE loading the module:

```prolog
?- consult('python.pl').
?- use_module('src/lib/python').
?- py_initialize.
```

Or in a script:
```prolog
:- consult('python.pl').
:- use_module('src/lib/python').
:- initialization(main).
```

### Environment Variable

For temporary/per-session configuration:
```bash
export LIBPYTHON_PATH=/path/to/libpython3.11.so
scryer-prolog examples/python_demo.pl
```

### Auto-Detection

Automatically searches these locations for Python 3.10, 3.11, or 3.12:
- **Linux**: `/usr/lib/x86_64-linux-gnu/libpython3.{12,11,10}.so`
- **macOS (Apple Silicon)**: `/opt/homebrew/lib/libpython3.{12,11,10}.dylib`
- **macOS (Intel)**: `/usr/local/lib/libpython3.{12,11,10}.dylib`

See `python_library_path/1` in `src/lib/python.pl` for implementation.

### Environment Managers

**See [docs/PYTHON_ENVIRONMENTS.md](docs/PYTHON_ENVIRONMENTS.md) for detailed guides on:**
- pyenv (requires `--enable-shared` flag)
- Conda/Miniconda (requires `LD_LIBRARY_PATH`)
- virtualenv (inherits from base Python)
- uv (fast Rust-based package manager)

**Critical for Conda/pyenv**: Set `LD_LIBRARY_PATH` (Linux) or `DYLD_LIBRARY_PATH` (macOS):
```bash
export LD_LIBRARY_PATH="$(python3-config --prefix)/lib:$LD_LIBRARY_PATH"
```

## Development Commands

### Running Tests

```bash
# Core functionality tests
scryer-prolog examples/tests/test_all_types.pl           # Type conversion (atoms, ints, floats)
scryer-prolog examples/tests/test_dict_to_list.pl        # Dictionary operations
scryer-prolog examples/tests/test_globals_locals.pl      # Globals/locals execution

# Memory management stress tests
scryer-prolog examples/tests/test_memory_management.pl   # 1000+ cycle stress tests

# Examples
scryer-prolog examples/python_demo.pl     # Basic demo (v0.1.0)
scryer-prolog examples/python_demo_v2.pl  # Advanced demo (v0.2.0)
```

### String Quoting Convention

**CRITICAL**: ScryPy has different quoting requirements for Python CODE vs Python DATA.

**Python CODE (in `py_run_simple_string`)**: Use double quotes `"..."`
- Reason: Supports multi-line strings, consistent with Python conventions
- In Scryer Prolog, double quotes create strings (character code lists)
- Examples:
  ```prolog
  py_run_simple_string("x = 2 + 2")
  py_run_simple_string("result = math.sqrt(16)")
  py_run_simple_string("def factorial(n): return 1 if n <= 1 else n * factorial(n-1)")
  ```

**Python DATA (values, variable names)**: Use single quotes `'...'` (atoms)
- Reason: `prolog_value_to_pyobject/2` expects atoms for string conversion
- In Scryer Prolog, single quotes create atoms
- Examples:
  ```prolog
  py_dict_set(Dict, name, 'Alice')          % Value is an atom
  py_run_simple_string("...", ['P'-1000, r-0.05], ..., ...)  % Variable names are atoms
  member('A'-Amount, Globals)               % Variable name is an atom
  ```

**Why this matters**:
```prolog
% Type conversion in src/lib/python.pl expects atoms:
prolog_value_to_pyobject(Value, PyObject) :-
    atom(Value), !,  % Matches single-quoted 'text', not double-quoted "text"
    ffi:'PyUnicode_FromString'(Value, PyObject).
```

**Wrong usage** (will cause type errors):
```prolog
py_dict_set(Dict, name, "Alice")          % ERROR: type_error(python_convertible,"Alice")
py_run_simple_string('x = 2 + 2')         % DISCOURAGED: single quotes work but break multi-line
```

### Interactive Development

```prolog
% Load the library
?- use_module('src/lib/python').

% Initialize Python
?- py_initialize.

% Test a feature (note double quotes for Python code)
?- py_run_simple_string("x = 42").

% Don't forget to finalize
?- py_finalize.
```

### Debugging FFI Issues

If FFI hangs or crashes:
1. **Check function signatures** - Wrong signature = hang/segfault
2. **Macro vs function** - If `nm -D libpythonX.XX.so | grep FuncName` returns nothing, it's a macro
3. **Test incrementally** - Add ONE function at a time to `use_foreign_module`
4. **Multifile directive syntax** - MUST use parentheses: `:- multifile(pred/1).` not `:- multifile pred/1.`

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

## Scryer Prolog Syntax Requirements

**CRITICAL**: Scryer Prolog has strict syntax requirements that differ from other Prologs:

1. **Multifile directives MUST use parentheses**:
   ```prolog
   % CORRECT
   :- multifile(pred/1).

   % WRONG - causes syntax_error(incomplete_reduction)
   :- multifile pred/1.
   ```

2. **Single-quoted strings MUST be on one line**:
   ```prolog
   % CORRECT
   throw(error(my_error, 'Short message'))

   % WRONG - causes syntax_error(invalid_single_quoted_character)
   throw(error(my_error, 'Multi
   line string'))
   ```

3. **Dynamic directives also need parentheses**:
   ```prolog
   % CORRECT
   :- dynamic(flag/0).

   % WRONG
   :- dynamic flag/0.
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
- **DESIGN_GLOBALS_LOCALS.md** - Globals/locals execution model
- **PYTHON_ENVIRONMENTS.md** - Python environment manager configuration guide
- **README.md** - User-facing API documentation (with big research warning)
- **PACKAGE.md** - Version history and roadmap

## Common Gotchas

1. **Memory leaks**: Always use `setup_call_cleanup` for NEW references
2. **FFI hangs**: Wrong function signature or calling a macro
3. **Segfaults**: Decrefing a BORROWED reference or decrefing twice
4. **Type conversion failures**: Check `PyErr_Occurred` after every conversion attempt
5. **State persistence**: Python state persists between `py_run_simple_string` calls until `py_finalize`
6. **Syntax errors**: Use parentheses for multifile/dynamic directives, single-line single-quoted strings only
7. **Configuration order**: Must `consult('python.pl')` BEFORE `use_module('src/lib/python')`
