# Scryer-Prolog-Python: Architecture & Design

**Version:** 0.2.0 (Pre-release / Alpha)
**Status:** Active Development
**Goal:** Feature parity with libpython-clj, adapted for Prolog paradigms

---

## Table of Contents

1. [What Is This Repository?](#what-is-this-repository)
2. [What Is a "Crate" in Scryer Prolog?](#what-is-a-crate-in-scryer-prolog)
3. [Current Architecture](#current-architecture)
4. [Key Components](#key-components)
5. [Design Philosophy](#design-philosophy)
6. [Current Challenges](#current-challenges)
7. [Roadmap to Parity with libpython-clj](#roadmap-to-parity-with-libpython-clj)
8. [Technical Decisions](#technical-decisions)
9. [Development Workflow](#development-workflow)

---

## What Is This Repository?

`scryer-prolog-python` is a **pure Prolog library** that provides seamless Python integration for Scryer Prolog using FFI (Foreign Function Interface). It allows Prolog programs to:

- Execute arbitrary Python code
- Exchange data bidirectionally between Prolog and Python
- Call Python functions and access Python objects
- Manage Python interpreter lifecycle explicitly

**Inspiration:** This project is modeled after:
- **libpython-clj** - Python integration for Clojure (functional approach to Python interop)
- **libscryer-clj** - Scryer Prolog integration for Clojure (machine-based lifecycle pattern)

**Key Difference from libpython-clj:** Instead of a singleton Python instance, we provide explicit lifecycle management (`py_initialize`/`py_finalize`), allowing multiple init/finalize cycles similar to how libscryer-clj manages Prolog machines.

---

## What Is a "Crate" in Scryer Prolog?

In the Scryer Prolog ecosystem, a **"crate"** or **"library"** refers to:

1. **A Prolog Module** - Defined with `:- module(name, [exports]).`
   - Lives in `src/lib/` directory structure
   - Exports predicates via module declaration
   - Can be loaded with `:- use_module(library(name)).`

2. **Pure Prolog vs. FFI Libraries**
   - **Pure Prolog**: Written entirely in Prolog (e.g., `library(lists)`, `library(assoc)`)
   - **FFI Libraries**: Use Scryer's FFI to call C libraries (e.g., `library(ffi)`, our `library(python)`)

3. **Integration with Scryer Core**
   - System libraries live in Scryer's `src/lib/` directory
   - User libraries can live anywhere and be loaded via relative paths
   - No separate package manager yet (unlike Rust crates or Python packages)

**Our Status:** We are a **standalone FFI-based library** that could eventually be merged into Scryer's core distribution. Currently distributed as source code in this repository.

---

## Multiple Interpreter Support: A Key Differentiator

### Python's Sub-Interpreter Capabilities

Python has supported **multiple interpreters in the same process** since version 1.5 (1997) via the C API. This feature allows creating isolated Python environments within a single OS process:

- **Main Interpreter:** Created via `Py_Initialize()` (once per process)
- **Sub-Interpreters:** Created via `Py_NewInterpreter()` / `Py_NewInterpreterFromConfig()` (returns `PyThreadState*`)
- **Cleanup:** `Py_EndInterpreter(tstate)` terminates a sub-interpreter

Each sub-interpreter provides:
- Independent `sys.modules` and `sys.path`
- Separate global namespaces
- Isolated execution environments

### The PEP 684 Breakthrough (Python 3.12+)

**Game-changer:** Python 3.12 introduced **per-interpreter GIL** (Global Interpreter Lock) via [PEP 684](https://peps.python.org/pep-0684/).

**Before Python 3.12:**
- All sub-interpreters shared ONE global GIL
- No true parallelism possible
- Limited usefulness

**After Python 3.12:**
- Each sub-interpreter can have its **own GIL**
- Configured via `PyInterpreterConfig.gil = PyInterpreterConfig_OWN_GIL`
- **TRUE PARALLEL EXECUTION** across CPU cores!
- Different interpreters can run Python code simultaneously

### Why This Matters for Us

Unlike libpython-clj (which uses a single interpreter for simplicity), we could provide:

1. **True Machine-Based API** (matching libscryer-clj's design philosophy)
2. **Parallel Python Execution** (Python 3.12+)
3. **Isolated Contexts** (natural fit for Prolog's multi-context reasoning)

### Potential API Design

```prolog
% Single interpreter mode (current - v0.1.x - v0.3.x)
py_initialize.
py_run_simple_string('x = 10').
py_finalize.

% Multiple interpreter mode (future - v0.4.0+)
py_initialize.                          % Main interpreter
py_new_interpreter(I1).                 % Create sub-interpreter
py_new_interpreter(I2).                 % Create another

% Run in specific interpreters
py_run(I1, 'import numpy; x = numpy.array([1,2,3])').
py_run(I2, 'import pandas; df = pandas.DataFrame()').

% TRUE PARALLELISM (Python 3.12+)
concurrent([
    py_run(I1, 'heavy_computation_1()'),
    py_run(I2, 'heavy_computation_2()')
]).  % Runs on different CPU cores!

% Cleanup
py_end_interpreter(I1).
py_end_interpreter(I2).
py_finalize.
```

### Trade-offs

**Advantages:**
- ✅ **True parallelism** on Python 3.12+ (per-interpreter GIL)
- ✅ **Isolated environments** (separate namespaces, modules)
- ✅ **Matches "machine" metaphor** from libscryer-clj
- ✅ **Natural for Prolog** (multiple logical contexts)
- ✅ **Major differentiator** from libpython-clj

**Challenges:**
- ❌ **Extension module compatibility** - Must use multi-phase init (many packages still don't)
- ❌ **Increased complexity** - More state to manage
- ❌ **Python 3.12+ requirement** - For per-interpreter GIL benefits
- ❌ **Testing complexity** - Need to verify isolation guarantees

### Implementation Strategy

**Phased Approach:**

1. **v0.1.x - v0.3.x:** Single interpreter model
   - Simpler implementation
   - Works with all Python versions
   - Establishes stable foundation

2. **v0.4.0:** Add sub-interpreter support (optional)
   - `py_new_interpreter/1`, `py_end_interpreter/1`
   - `py_run/2` for interpreter-specific execution
   - Document Python 3.12+ requirement for parallelism
   - Detect Python version, warn if < 3.12

3. **v0.5.0+:** Optimize multi-interpreter workflows
   - Automatic interpreter pooling
   - Load balancing across interpreters
   - Integration with Scryer's threading model

### Decision: Keep the Door Open

**Current Decision (v0.2.0):**
- Implement single-interpreter API first
- Design with extensibility in mind
- Document multi-interpreter as a future possibility

**Rationale:**
- Get basic functionality solid first
- Learn from real-world usage
- Avoid premature complexity
- Many Python packages still don't support sub-interpreters well

**But:** The architecture should not preclude adding multi-interpreter support later. We keep the API clean so adding interpreter IDs is a backward-compatible extension.

---

## Current Architecture

### Directory Structure

```
scryer-prolog-python/
├── src/
│   └── lib/
│       └── python.pl          # Main library module (450+ lines)
├── examples/
│   ├── python_demo.pl         # v0.1.0 basic examples
│   └── python_demo_v2.pl      # v0.2.0 advanced examples
├── tests/
│   ├── test_all_types.pl      # Type conversion tests
│   ├── test_dict_to_list.pl   # Dictionary tests
│   └── test_globals_locals.pl # Namespace tests
├── docs/
│   ├── DESIGN_GLOBALS_LOCALS.md      # Design doc for v0.2.0
│   ├── DESIGN_STATE_MANAGEMENT.md    # Memory & state design
│   └── ARCHITECTURE.md               # This document
├── VERSION                    # Semantic version number
├── PACKAGE.md                 # Version history & roadmap
└── README.md                  # User documentation
```

### Module Organization (src/lib/python.pl)

The library is organized into logical sections:

```prolog
:- module(python, [
    % Lifecycle
    py_initialize/0,
    py_finalize/0,

    % Code Execution
    py_run_simple_string/1,      % Basic execution
    py_run_simple_string/5,      % With globals/locals

    % Dictionary Operations
    py_dict_new/1,
    py_dict_set/3,
    py_dict_get/3,
    py_dict_to_list/2,
    prolog_to_py_dict/2,
    py_dict_to_prolog/2,

    % Memory Management (v0.2.0)
    py_incref/1,                 % Increment reference count
    py_decref/1,                 % Decrement reference count
    py_xdecref/1,                % Safe decrement (handles NULL)
    with_new_pyobject/3          % Automatic cleanup wrapper
]).

%% Dependencies
:- use_module(library(ffi)).      % FFI for C API calls
:- use_module(library(error)).    % Error handling
:- use_module(library(iso_ext)).  % Blackboard (bb_put/bb_get)

%% Internal Structure (not exported, used internally)
% 1. State Management Abstraction (lines 73-92)
% 2. Reference Counting Abstraction (lines 94-156)
% 3. FFI Bindings (lines 226-273)
% 4. Dictionary Operations (lines 275-330)
% 5. Type Conversion (lines 332-390)
% 6. Extended Execution (lines 392-450)
```

### FFI Layer Architecture

The library uses Scryer Prolog's FFI to directly call Python C API functions:

```
┌─────────────────┐
│  Prolog Code    │
│  (user.pl)      │
└────────┬────────┘
         │ :- use_module(library(python))
         │
┌────────▼────────┐
│  python.pl      │  ← Our Library
│  Module Layer   │
└────────┬────────┘
         │ ffi:'PyDict_New'(...)
         │
┌────────▼────────┐
│  Scryer FFI     │  ← Built into Scryer
│  (library(ffi)) │
└────────┬────────┘
         │ use_foreign_module(...)
         │
┌────────▼────────┐
│ libpython3.10.so│  ← System Python
│  Python C API   │
└─────────────────┘
```

**No C wrapper code needed!** We call Python C API functions directly via FFI.

---

## Key Components

### 1. State Management (v0.2.0)

**Problem:** Need to track Python interpreter state (initialized/finalized) and library load status.

**Solution:** Blackboard abstraction over `library(iso_ext)`:

```prolog
% Abstraction layer
python_state_set(Key, Value) :- bb_put(Key, Value).
python_state_check(Key) :- bb_get(Key, true).

% Public API
is_python_initialized :- python_state_check(python_initialized).
mark_python_initialized :- python_state_set(python_initialized, true).
```

**Benefits:**
- Cleaner than `dynamic`/`assert`/`retract`
- Easy to swap implementation later
- Thread-safe (bb_put/bb_get are atomic)

### 2. Reference Counting & Memory Management (v0.2.0)

**Problem:** Python uses reference counting for garbage collection. We must track object lifetimes to prevent memory leaks.

**Solution:** Abstraction over Py_IncRef/Py_DecRef with automatic cleanup:

```prolog
% FFI bindings
'Py_IncRef'([ptr], void),
'Py_DecRef'([ptr], void)

% Public API (exported from module)
py_incref(+PyObject)   % Increment reference count
py_decref(+PyObject)   % Decrement reference count
py_xdecref(+PyObject)  % Safe decrement (handles NULL/0)

% Automatic cleanup helper
with_new_pyobject(:Create, -Obj, :Use) :-
    call(Create, Obj),
    setup_call_cleanup(true, call(Use, Obj), py_xdecref(Obj)).
```

**Reference Types:**
- **NEW** references (we own, must decref): `PyDict_New`, `PyLong_FromLong`, `PyFloat_FromDouble`, `PyUnicode_FromString`, `PyDict_Keys`, `PyRun_String`
- **BORROWED** references (don't own, must NOT decref): `PyDict_GetItemString`, `PyList_GetItem`, `PyModule_GetDict`

**Memory Leak Prevention:**

All predicates that create Python objects use `setup_call_cleanup` to ensure proper cleanup:

```prolog
% Example: py_dict_set/3 - creates PyValue, must decref
py_dict_set(DictPtr, Key, Value) :-
    prolog_value_to_pyobject(Value, PyValue),  % NEW reference!
    setup_call_cleanup(
        true,
        ffi:'PyDict_SetItemString'(DictPtr, Key, PyValue, Result),
        py_xdecref(PyValue)  % Always cleanup, even on error
    ).

% Example: py_dict_to_list/2 - PyDict_Keys returns NEW reference
py_dict_to_list(DictPtr, List) :-
    ffi:'PyDict_Keys'(DictPtr, KeysObj),  % NEW reference!
    setup_call_cleanup(
        ffi:'PyList_Size'(KeysObj, Size),
        dict_keys_to_list(DictPtr, KeysObj, 0, Size, List),
        py_xdecref(KeysObj)  % Cleanup
    ).
```

**Reference Ownership Tracking:**

For `py_run_simple_string/5`, we track whether we own references:

```prolog
py_run_simple_string(Code, GlobalsIn, LocalsIn, GlobalsOut, LocalsOut) :-
    % If GlobalsIn=[], use __main__ (BORROWED ref, don't decref)
    (GlobalsIn = [] ->
        ffi:'PyModule_GetDict'(MainModule, GlobalsDict),
        GlobalsOwned = false
    ;
        prolog_to_py_dict(GlobalsIn, GlobalsDict),  % NEW ref
        GlobalsOwned = true
    ),
    % ... similar for LocalsDict ...
    setup_call_cleanup(
        ffi:'PyRun_String'(Code, 257, GlobalsDict, LocalsDict, ResultPtr),
        % ... process results ...
        % Cleanup: only decref NEW references
        (py_xdecref(ResultPtr),
         (GlobalsOwned = true -> py_xdecref(GlobalsDict) ; true),
         (LocalsOwned = true -> py_xdecref(LocalsDict) ; true))
    ).
```

**Testing:**

Memory management verified with stress tests (`test_memory_management.pl`):
- 1000 dict creation/destruction cycles ✅
- 1000 prolog_to_py_dict conversions ✅
- 100 py_run_simple_string/5 executions ✅
- Nested operations with multiple sets/gets ✅

### 3. Type Conversion System

**Problem:** Convert between Prolog and Python types bidirectionally.

**Current Support:**
| Prolog Type | Python Type | Direction |
|-------------|-------------|-----------|
| Atom        | str         | Both      |
| Integer     | int         | Both      |
| Float       | float       | Both      |
| true/false  | True/False  | From Python |

**Implementation:** Try-convert approach with error checking:

```prolog
pyobject_to_prolog_value(PyObject, Value) :-
    % Try string conversion
    ffi:'PyUnicode_AsUTF8'(PyObject, StrResult),
    ffi:'PyErr_Occurred'(Err),
    (Err = 0 -> atom_chars(Value, StrResult)
    ;   % Try int, then float, then bool...
        ...
    ).
```

**Why try-convert?** Python type-checking macros (PyLong_Check, etc.) aren't available via FFI—they're C macros, not functions.

### 4. Dictionary Operations

**Representation:** Python dicts ↔ Prolog key-value lists

```prolog
% Python: {'name': 'Alice', 'age': 30}
% Prolog: [name-'Alice', age-30]

% Operations
py_dict_new(-DictPtr)                    % Create dict
py_dict_set(+DictPtr, +Key, +Value)      % Set item
py_dict_get(+DictPtr, +Key, -Value)      % Get item
py_dict_to_list(+DictPtr, -List)         % Convert to Prolog
prolog_to_py_dict(+List, -DictPtr)       % Convert to Python
```

### 5. Code Execution

**Two modes:**

1. **Simple execution** (v0.1.0):
```prolog
py_run_simple_string('x = 10')
% Executes in __main__ namespace
% State persists between calls
```

2. **With globals/locals** (v0.2.0):
```prolog
py_run_simple_string('result = a + b',
                     [a-5, b-10],      % GlobalsIn
                     [],               % LocalsIn
                     Globals,          % GlobalsOut
                     _Locals).         % LocalsOut
% Globals = [a-5, b-10, ..., result-15]
```

---

## Design Philosophy

### 1. **Explicit Over Implicit**
- User controls interpreter lifecycle (`py_initialize`/`py_finalize`)
- No hidden global state
- Errors are thrown, not silently ignored

### 2. **Prolog-Native Patterns**
- Use Prolog lists, not foreign data structures
- Unification for data extraction
- Backtracking where appropriate (future: iterating Python sequences)

### 3. **Zero-Copy When Possible, Safety Always**
- Direct FFI calls (no C wrapper)
- But: proper memory management via reference counting
- Choose safety over performance where they conflict

### 4. **Incremental Complexity**
- v0.1.0: Basic string execution
- v0.2.0: Add data exchange
- v0.3.0+: Complex types, iterators, modules

### 5. **Inspired by libpython-clj, Not a Port**
- Adapt Clojure patterns to Prolog idioms
- Learn from their solutions (scoping, GC, type bridging)
- But respect Prolog's unique strengths (unification, logic, backtracking)

---

## Current Challenges

### 1. **Memory Management** ✅ RESOLVED (v0.2.0)

**Problem:** Python objects created via PyLong_FromLong, etc. return NEW references that must be decreffed to prevent memory leaks.

**Status:**
- ✅ Infrastructure in place (`py_decref`, `py_xdecref`, `with_new_pyobject`)
- ✅ All predicates audited and fixed (v0.2.0)
- ✅ Comprehensive stress testing completed

**Solution Implemented:**

All predicates that create Python objects now use `setup_call_cleanup` for automatic cleanup:

```prolog
% py_dict_set/3 - Fixed in v0.2.0
py_dict_set(Dict, key, Value) :-
    prolog_value_to_pyobject(Value, PyValue),  % NEW ref
    setup_call_cleanup(
        true,
        ffi:'PyDict_SetItemString'(Dict, key, PyValue, Result),
        py_xdecref(PyValue)  % Always cleanup, even on error
    ).

% py_dict_to_list/2 - Fixed in v0.2.0
py_dict_to_list(DictPtr, List) :-
    ffi:'PyDict_Keys'(DictPtr, KeysObj),  % NEW ref
    setup_call_cleanup(
        ffi:'PyList_Size'(KeysObj, Size),
        dict_keys_to_list(DictPtr, KeysObj, 0, Size, List),
        py_xdecref(KeysObj)  % Cleanup
    ).

% py_run_simple_string/5 - Fixed in v0.2.0 with ownership tracking
% Only decrefs NEW references, not BORROWED ones
```

**Verification:**
- Created `test_memory_management.pl` with stress tests
- 1000+ creation/destruction cycles pass without crashes
- All existing tests still pass

### 2. **Limited Type Support**

**Currently supports:**
- Atoms, integers, floats, booleans

**Missing:**
- Lists (Python lists/tuples)
- Compound terms (Python dicts with nested structures)
- Prolog variables (how to represent None, unbound values?)
- Binary data (bytes)
- Custom Python classes

**Challenge:** Type mapping isn't 1:1. Need design decisions on:
- How to represent Python None in Prolog?
- How to handle Prolog variables being passed to Python?
- Compound terms → dicts vs. custom objects?

### 3. **No Scoped Resource Management**

**libpython-clj has:**
```clojure
(py/stack-resource-context
  ;; All Python objects created here
  ;; are freed when scope exits
  ...)
```

**We need:**
```prolog
with_py_scope(Goal) :-
    % Track all Python objects created during Goal
    % Automatically decref them all when Goal exits
    ...
```

**Approach:** Could use Scryer's `call_cleanup` with a thread-local object registry.

### 4. **No Module/Import System**

**Current:** Can only execute code strings

**Needed:**
```prolog
py_import(numpy, Numpy),
py_call(Numpy, array, [[1,2,3]], Result).
```

Requires:
- `PyImport_ImportModule` (already FFI-bound)
- `PyObject_GetAttrString` (need to add)
- `PyObject_CallObject` (need to add)
- Proper object wrapping/tracking

### 5. **Performance Concerns**

**Current bottlenecks:**
- Type conversion via try-convert is slower than type checking
- List conversion recurses for every element
- No batch operations

**Not measured yet!** Premature optimization is evil. Focusing on correctness first.

### 6. **Error Handling Coverage**

**Current:** Basic error checking with `PyErr_Occurred`

**Missing:**
- Detailed Python exception messages
- Python tracebacks
- Mapping Python exceptions to Prolog error terms

**Need:**
```prolog
% Python raises ValueError
?- py_run_simple_string('int("not a number")').
   error(python_error(value_error, 'invalid literal for int()...'), ...).
```

### 7. **Thread Safety**

**Status:** UNKNOWN

Python's GIL (Global Interpreter Lock) means only one thread can execute Python code at a time.

**Questions:**
- Can multiple Prolog threads call Python safely?
- Do we need explicit GIL acquire/release?
- How does this interact with Scryer's threading model?

**libpython-clj approach:** Explicit `with-gil` for performance

### 8. **Testing & CI**

**Current:**
- ✅ Manual test files
- ✅ Examples that demonstrate features
- ❌ No automated test suite
- ❌ No continuous integration
- ❌ No property-based testing

**Need:**
- Integration with Scryer's test framework (if it exists?)
- CI that runs tests on commits
- Property tests for type conversion roundtrips

### 9. **Documentation Gaps**

**Current:**
- ✅ README with examples
- ✅ Design documents
- ✅ Inline documentation in code
- ❌ No tutorial/guide for new users
- ❌ No performance guide
- ❌ No troubleshooting guide

---

## Roadmap to Parity with libpython-clj

### Phase 1: Stability & Memory Safety (v0.2.x - v0.3.0)

**Goal:** Make it production-ready for basic use cases

- [ ] **Fix all memory leaks** (CRITICAL)
  - Audit all predicates for reference leaks
  - Add cleanup to `py_dict_set`, `prolog_value_to_pyobject`, etc.
  - Test with long-running programs

- [ ] **Scoped resource management**
  - Implement `with_py_scope/1`
  - Track objects created in a scope
  - Bulk cleanup on scope exit

- [ ] **Better error handling**
  - Extract Python exception messages
  - Include tracebacks
  - Map to appropriate Prolog error terms

- [ ] **Testing infrastructure**
  - Automated test suite
  - CI setup
  - Memory leak tests

**Deliverable:** v0.3.0 - "Stable for basic use cases"

### Phase 2: Type System Expansion & Multi-Interpreter Support (v0.4.0 - v0.5.0)

**Goal:** Support rich data interchange AND enable parallel Python execution

- [ ] **Multi-Interpreter Support** (v0.4.0) ⭐ NEW
  - `py_new_interpreter/1` - Create sub-interpreters
  - `py_end_interpreter/1` - Cleanup sub-interpreters
  - `py_run/2` - Execute in specific interpreter
  - Python 3.12+ detection for per-interpreter GIL
  - Documentation on extension module compatibility
  - Test isolation between interpreters

- [ ] **List/Tuple support**
  - Python lists ↔ Prolog lists
  - Python tuples → Prolog lists (immutable)
  - Nested structures

- [ ] **None and null handling**
  - Represent Python None in Prolog
  - Handle Prolog unbound variables

- [ ] **Binary data**
  - Python bytes ↔ Prolog representation?
  - NumPy arrays (if we go that far)

- [ ] **Compound terms**
  - Map Prolog compound terms to Python objects
  - Design decision: dicts or custom classes?

**Deliverable:** v0.4.0 - "Rich type support + Multi-interpreter foundation"

### Phase 3: Module & Call System (v0.6.0 - v0.7.0)

**Goal:** Call Python functions and methods naturally

- [ ] **Import system**
  ```prolog
  py_import(numpy, NP),
  py_import('sklearn.linear_model', SKL).
  ```

- [ ] **Function calls**
  ```prolog
  py_call(NP, array, [[1,2,3]], Array),
  py_call(NP, mean, [Array], Mean).
  ```

- [ ] **Attribute access**
  ```prolog
  py_getattr(Array, shape, Shape).
  ```

- [ ] **Method calls**
  ```prolog
  py_method(Array, reshape, [3,1], Reshaped).
  ```

**Deliverable:** v0.6.0 - "Function call support"

### Phase 4: Advanced Features (v0.8.0+)

**Goal:** Match libpython-clj's advanced capabilities

- [ ] **Generators and iterators**
  - Python generators as backtrackable predicates
  - Lazy evaluation

- [ ] **Context managers**
  ```prolog
  py_with(open('file.txt', 'r'), File, py_read(File, Contents)).
  ```

- [ ] **Callbacks (Prolog → Python)**
  - Pass Prolog predicates as Python callables
  - Requires creating Python wrapper objects

- [ ] **Class creation**
  - Define Python classes from Prolog

- [ ] **NumPy integration**
  - Zero-copy access to NumPy arrays
  - Requires understanding memory layout

**Deliverable:** v0.8.0 - "Advanced integration"

### Phase 5: Performance & Polish (v0.9.0 → v1.0.0)

**Goal:** Production-grade performance and stability

- [ ] **Performance optimization**
  - Benchmark against libpython-clj
  - Identify and fix bottlenecks
  - Batch operations where possible

- [ ] **Comprehensive documentation**
  - Tutorial
  - Cookbook with examples
  - Performance guide
  - Migration guide from other tools

- [ ] **Stability hardening**
  - Stress tests
  - Memory tests (valgrind, etc.)
  - Thread safety verification

- [ ] **Package distribution**
  - Include in Scryer Prolog core? (requires Scryer team approval)
  - Or: standalone package with version management

**Deliverable:** v1.0.0 - "Production ready"

### libpython-clj Features We May NOT Need

Some features are Clojure-specific and don't translate:

- **JVM Integration** - We're not on the JVM
- **require-python** macro - Prolog doesn't have compile-time macros
- **Clojure protocols** - No direct equivalent in Prolog

---

## Technical Decisions

### Why FFI Instead of C Extension?

**Pros:**
- ✅ Pure Prolog - no C compilation needed
- ✅ Portable - works wherever libpython exists
- ✅ Easier to develop/debug
- ✅ Leverages Scryer's FFI infrastructure

**Cons:**
- ❌ Can't call C macros (only functions)
- ❌ Slightly slower than native C (but negligible)
- ❌ No compile-time checking of FFI calls

**Decision:** FFI is the right choice for rapid development and maintainability.

### Why Blackboard (bb_put/bb_get) Instead of Dynamic?

**Comparison:**

| Feature | dynamic + assert/retract | bb_put/bb_get |
|---------|-------------------------|---------------|
| Speed | Fast | Fast |
| Thread safety | No | Yes |
| Backtrackable | Can be (using asserta) | Has bb_b_put |
| Cleanup | Manual retractall | Automatic |
| Semantics | Logical database | Global variables |

**Decision:** Blackboard is cleaner for global state that isn't meant to backtrack.

### Why Try-Convert Instead of Type Checking?

**Problem:** Python's type-checking functions (PyLong_Check, etc.) are C macros, not exported functions.

**Solutions:**
1. ❌ Get type object pointers - Failed (Py_XxxType not callable)
2. ✅ **Try-convert approach** - Try each conversion, check for errors
3. ⏳ Future: Use PyType_IsSubtype with manual type lookups

**Decision:** Try-convert works and is simple. Optimize later if needed.

### Why Key-Value Lists Instead of library(assoc)?

**Comparison:**

| Feature | [key-value] | library(assoc) |
|---------|-------------|----------------|
| Simplicity | Very simple | More complex |
| Performance | O(n) lookup | O(log n) lookup |
| Familiarity | Common pattern | Less known |
| Order | Preserved | Tree order |

**Decision:** Lists are simpler and match Python dict behavior. If performance becomes an issue, we can add assoc support later.

---

## Development Workflow

### Current Process

1. **Design** → Write design doc (DESIGN_*.md)
2. **Implement** → Add to src/lib/python.pl
3. **Test** → Create test file in root
4. **Document** → Update README.md and inline docs
5. **Version** → Update VERSION and PACKAGE.md

### Testing Approach

```bash
# Run specific test
scryer-prolog test_all_types.pl

# Run all tests
for test in test_*.pl; do
    echo "=== Running $test ==="
    scryer-prolog "$test"
done

# Run examples (also serve as integration tests)
scryer-prolog examples/python_demo.pl
scryer-prolog examples/python_demo_v2.pl
```

### Git Workflow

- **Branch:** `python-integration` (current development branch)
- **Commits:** Descriptive messages explaining changes
- **History:** Preserved for learning/debugging

### Version Policy

Following semantic versioning, but with 0.x caveats:

- **0.x.y** - API may change between minor versions
- **Major** - Incompatible API changes (but not until 1.0.0)
- **Minor** - New features (backward compatible within 0.x)
- **Patch** - Bug fixes

**Note:** API is unstable until 1.0.0!

---

## How to Contribute

(Future section - for when project is public)

---

## References

### Related Projects

- **libpython-clj** - https://github.com/clj-python/libpython-clj
  - Our primary inspiration
  - Great docs on scoping, GC, type bridging

- **libscryer-clj** - (private?)
  - Machine-based lifecycle pattern
  - How to embed Scryer in another language

- **SWI-Prolog Janus** - https://www.swi-prolog.org/pldoc/man?section=janus
  - Another Prolog-Python bridge
  - Different approach (embedded Python in Prolog)

### Documentation

- **Scryer Prolog** - https://github.com/mthom/scryer-prolog
- **Python C API** - https://docs.python.org/3/c-api/
- **Scryer FFI** - See `src/lib/ffi.pl` in Scryer source

---

## Appendix: Current FFI Bindings

All Python C API functions currently bound:

```prolog
% Interpreter lifecycle
'Py_Initialize'([], void)
'Py_Finalize'([], void)

% Code execution
'PyRun_SimpleString'([cstr], int)
'PyRun_String'([cstr, int, ptr, ptr], ptr)

% Dictionary operations
'PyDict_New'([], ptr)
'PyDict_SetItemString'([ptr, cstr, ptr], int)
'PyDict_GetItemString'([ptr, cstr], ptr)
'PyDict_Keys'([ptr], ptr)
'PyDict_Size'([ptr], i64)

% Type conversion
'PyLong_FromLong'([long], ptr)
'PyFloat_FromDouble'([double], ptr)
'PyUnicode_FromString'([cstr], ptr)
'PyLong_AsLong'([ptr], long)
'PyFloat_AsDouble'([ptr], double)
'PyUnicode_AsUTF8'([ptr], cstr)
'PyObject_Type'([ptr], ptr)
'PyObject_IsTrue'([ptr], int)

% Error handling
'PyErr_Occurred'([], ptr)
'PyErr_Clear'([], void)

% List operations
'PyList_Size'([ptr], i64)
'PyList_GetItem'([ptr, i64], ptr)

% Module operations
'PyImport_AddModule'([cstr], ptr)
'PyModule_GetDict'([ptr], ptr)

% Reference counting
'Py_IncRef'([ptr], void)
'Py_DecRef'([ptr], void)
```

**Total:** 23 functions bound

---

**Document Version:** 1.0
**Last Updated:** 2025-10-24
**Authors:** J.J. Tolton, Claude (AI Assistant)
