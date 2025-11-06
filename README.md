# ScryPy

**Scryer Prolog + Python Integration**

**Version: 0.3.0** (Pre-release / Alpha)

---

## ⚠️ RESEARCH LIBRARY - RAPID ITERATION IN PROGRESS

**THIS IS AN ACTIVE RESEARCH PROJECT. NOT PRODUCTION READY.**

- 🚧 **API UNSTABLE** - Breaking changes expected frequently
- 🚧 **Architecture UNSTABLE** - Core design decisions still being evaluated
- 🚧 **Goals UNSTABLE** - Feature set and direction may pivot significantly
- 🚧 **Use Cases UNCERTAIN** - Exploring what makes sense at the Prolog/Python boundary

**Many open questions exist about how to make a declarative logic language work with a multi-paradigm language like Python.**

This library is largely based on the work from [libpython-clj](https://github.com/clj-python/libpython-clj), but adapted for Prolog's unique paradigm. We're actively exploring questions like:
- How should Python's imperative mutation interact with Prolog's logical variables?
- What's the right abstraction for Python objects in a relational context?
- Should we expose Python's OOP directly, or create a more declarative interface?
- How do we handle Python's exceptions in Prolog's backtracking model?

**If you need stable Python integration, this is not the library for you. Yet.**

---

## Overview

This library provides integration between Scryer Prolog and Python using FFI (Foreign Function Interface). It allows you to execute Python code directly from Prolog.

**Inspirations:**
- **[libpython-clj](https://github.com/clj-python/libpython-clj)** - Python integration for Clojure (primary inspiration)
- **libscryer-clj** - Scryer Prolog integration for Clojure

Unlike libpython-clj which creates one Python instance per process, this library explores explicit interpreter lifecycle management, enabling multiple init/finalize cycles (similar to how libscryer-clj manages Scryer machines). However, this design may change as we explore different approaches.

## Features

### Core Features
- ✅ Initialize and finalize Python interpreter
- ✅ Execute arbitrary Python code from strings
- ✅ State persistence across multiple calls
- ✅ Error handling for Python exceptions
- ✅ Access to full Python standard library
- ✅ Support for Python packages (NumPy, etc.)

### Version 0.2.0 Features
- ✅ **Dictionary Operations**: Create and manipulate Python dictionaries
- ✅ **Type Conversion**: Bidirectional conversion between Prolog and Python types
  - Atoms ↔ Python strings
  - Integers ↔ Python ints
  - Floats ↔ Python floats
  - Booleans (true/false) ↔ Python booleans
- ✅ **Globals/Locals Support**: Execute Python with custom variable scopes
- ✅ **Value Extraction**: Get computed values back from Python into Prolog
- ✅ **Dict ↔ List Conversion**: Convert between Python dicts and Prolog key-value lists

## Installation

**See [INSTALL.md](INSTALL.md) for detailed installation instructions**, including:
- How to install Python development libraries
- Finding your Python shared library location
- Platform-specific setup (Linux, macOS)
- Troubleshooting common issues

### Quick Install

1. Clone this repository:
```bash
git clone https://github.com/jjtolton/scrypy.git
cd scrypy
```

2. Install Python development libraries:
```bash
# Ubuntu/Debian - one of these depending on your Python version
sudo apt-get install python3.10-dev libpython3.10
sudo apt-get install python3.11-dev libpython3.11
sudo apt-get install python3.12-dev libpython3.12

# macOS (Homebrew)
brew install python@3.11
```

3. The library will automatically detect your Python version (3.10, 3.11, or 3.12).

4. Use the library:
```prolog
:- use_module('src/lib/python').
```

## Requirements

### Scryer Prolog (with RTLD_GLOBAL support)

**For full Python C extension support** (NumPy, pandas, SciPy, etc.), you need a version of Scryer Prolog with RTLD_GLOBAL support:

- **Recommended**: Use the [jjtolton/scryer-prolog](https://github.com/jjtolton/scryer-prolog) fork (branch: `rtld-global-support`)
- **Upstream PR**: [mthom/scryer-prolog#3144](https://github.com/mthom/scryer-prolog/pull/3144) - Once merged, you can use the official release

**Why this matters**: The standard Scryer Prolog loads foreign libraries with `RTLD_LOCAL`, which prevents Python C extensions from resolving symbols. The fork/PR adds `scope(global)` option to `use_foreign_module/3` to enable `RTLD_GLOBAL` loading.

**As of v0.3.0**: ScryPy now uses `scope(global)` by default for all Python library loading, enabling full C extension support out of the box (when using the RTLD-enabled Scryer fork).

**For basic Python only** (no C extensions): Any Scryer Prolog v0.10.0+ will work, but imports like `numpy`, `pandas`, etc. will fail.

### Python

- **Python** 3.10, 3.11, or 3.12 with shared library (`.so` on Linux, `.dylib` on macOS)
- **Python development package** (`python3.X-dev` on Linux)

The library automatically detects which Python version is installed and uses the appropriate shared library.

## Quick Start

```prolog
?- use_module('src/lib/python').
true.

?- py_initialize.
true.

?- py_run_simple_string('print("Hello from Python!")').
Hello from Python!
true.

?- py_run_simple_string('x = 42').
true.

?- py_run_simple_string('print(f"The answer is {x}")').
The answer is 42
true.

?- py_finalize.
true.
```

## API Reference

### Core Predicates

#### `py_initialize/0`

Initialize the Python interpreter. Must be called before any other Python operations.

**Throws**: `permission_error` if Python is already initialized.

#### `py_finalize/0`

Finalize the Python interpreter and free all resources.

**Throws**: `existence_error` if Python is not initialized.

#### `py_run_simple_string(+Code)`

Execute Python code from a string. The code is executed in the `__main__` module's namespace.

**Parameters**:
- `Code`: An atom containing Python code to execute

**Throws**:
- `existence_error` if Python is not initialized
- `python_error(Code)` if the Python code raises an exception

**Example**:
```prolog
?- py_run_simple_string('x = 10').
?- py_run_simple_string('print(x * 2)').
20
```

#### `py_run_simple_string(+Code, +GlobalsIn, +LocalsIn, -GlobalsOut, -LocalsOut)`

Execute Python code with explicit globals and locals dictionaries. This allows you to pass variables to Python and get computed values back.

**Parameters**:
- `Code`: Python code to execute (atom)
- `GlobalsIn`: List of Key-Value pairs for global namespace
- `LocalsIn`: List of Key-Value pairs for local namespace
- `GlobalsOut`: Resulting global namespace as Key-Value list
- `LocalsOut`: Resulting local namespace as Key-Value list

**Example**:
```prolog
?- py_run_simple_string('result = x + y', [x-10, y-20], [], Globals, _).
Globals = [x-10, y-20, __builtins__-true, result-30].
```

### Dictionary Operations (v0.2.0+)

#### `py_dict_new(-DictPtr)`

Create a new empty Python dictionary.

**Parameters**:
- `DictPtr`: Unified with pointer to new Python dict

**Example**:
```prolog
?- py_dict_new(Dict).
Dict = 140475345675648.
```

#### `py_dict_set(+DictPtr, +Key, +Value)`

Set a key-value pair in a Python dictionary.

**Parameters**:
- `DictPtr`: Pointer to Python dict
- `Key`: Prolog atom (converted to Python string)
- `Value`: Prolog value (atom, integer, or float)

**Example**:
```prolog
?- py_dict_new(Dict),
   py_dict_set(Dict, name, 'Alice'),
   py_dict_set(Dict, age, 30).
```

#### `py_dict_get(+DictPtr, +Key, -Value)`

Get a value from a Python dictionary by key.

**Parameters**:
- `DictPtr`: Pointer to Python dict
- `Key`: Prolog atom (key name)
- `Value`: Unified with Prolog value

**Example**:
```prolog
?- py_dict_get(Dict, name, Name).
Name = 'Alice'.
```

#### `py_dict_to_list(+DictPtr, -List)`

Convert a Python dictionary to a Prolog list of Key-Value pairs.

**Parameters**:
- `DictPtr`: Pointer to Python dict
- `List`: Unified with list of Key-Value pairs

**Example**:
```prolog
?- py_dict_to_list(Dict, List).
List = [name-'Alice', age-30].
```

#### `prolog_to_py_dict(+PrologList, -DictPtr)`

Convert a Prolog list of Key-Value pairs to a Python dictionary.

**Parameters**:
- `PrologList`: List of Key-Value pairs
- `DictPtr`: Unified with pointer to new Python dict

**Example**:
```prolog
?- prolog_to_py_dict([x-10, y-20], Dict).
```

#### `py_dict_to_prolog(+DictPtr, -PrologList)`

Alias for `py_dict_to_list/2`.

## Examples

### Basic Demo (v0.1.0)
See `examples/python_demo.pl` for basic usage examples:

```bash
scryer-prolog examples/python_demo.pl
```

### Advanced Demo (v0.2.0)
See `examples/python_demo_v2.pl` for advanced features including dictionaries, type conversion, and globals/locals:

```bash
scryer-prolog examples/python_demo_v2.pl
```

### Test Suite
Run the comprehensive test suite:

```bash
scryer-prolog examples/tests/test_all_types.pl          # Type conversion tests
scryer-prolog examples/tests/test_dict_to_list.pl       # Dictionary conversion tests
scryer-prolog examples/tests/test_globals_locals.pl     # Globals/locals tests
scryer-prolog examples/tests/test_memory_management.pl  # Memory management stress tests
```

## Architecture

The library uses Scryer Prolog's FFI to call Python C API functions directly, without requiring any C wrapper code.

### Core Functions
- `Py_Initialize()`: Initialize the Python interpreter
- `Py_Finalize()`: Finalize the Python interpreter
- `PyRun_SimpleString(code)`: Execute Python code in __main__ namespace
- `PyRun_String(code, start, globals, locals)`: Execute with explicit namespaces

### Dictionary Operations
- `PyDict_New()`: Create empty dictionary
- `PyDict_SetItemString(dict, key, value)`: Set item by string key
- `PyDict_GetItemString(dict, key)`: Get item by string key
- `PyDict_Keys(dict)`: Get list of keys
- `PyDict_Size(dict)`: Get number of items

### Type Conversion
- `PyLong_FromLong(long)`, `PyLong_AsLong(obj)`: Integer conversion
- `PyFloat_FromDouble(double)`, `PyFloat_AsDouble(obj)`: Float conversion
- `PyUnicode_FromString(str)`, `PyUnicode_AsUTF8(obj)`: String conversion
- `PyObject_IsTrue(obj)`: Boolean conversion

### List Operations
- `PyList_Size(list)`: Get list length
- `PyList_GetItem(list, index)`: Get item at index

### Error Handling
- `PyErr_Occurred()`: Check if error occurred
- `PyErr_Clear()`: Clear error state

### Type Discovery
The library uses a try-convert approach: it attempts each type conversion (string, int, float, boolean) in order and checks for Python errors to determine the actual type. This avoids issues with type macros that aren't available via FFI.

## Current Research Questions

This is an **active research project**. We're exploring fundamental questions about Prolog/Python interop:

### Design Philosophy
- **Imperative vs Declarative**: How do we reconcile Python's imperative style with Prolog's declarative paradigm?
- **Mutation**: Should Python mutations be visible in Prolog? How do we handle side effects?
- **Object Identity**: How do we represent Python object identity in Prolog's relational model?

### Type System
- **Type Mapping**: What's the right mapping between Prolog terms and Python types?
- **None vs fail()**: Should Python `None` map to Prolog `false`, a special term, or something else?
- **Compound Terms**: How do we handle nested Prolog structures in Python?

### Execution Model
- **Backtracking**: Can Python code participate in Prolog's backtracking? Should it?
- **Exception Handling**: How do Python exceptions interact with Prolog's error handling?
- **Concurrency**: With Python 3.12's per-interpreter GIL, what does multi-interpreter support look like in Prolog?

### API Design
- **Low-level vs High-level**: Should we expose raw Python C API, or build higher-level abstractions?
- **Resource Management**: Is explicit py_initialize/py_finalize the right model, or should we auto-manage?
- **Scoping**: Do we need explicit resource scopes like libpython-clj's `with-gil`?

**These are not rhetorical questions.** We're actively experimenting and the answers will shape future versions.

## Contributing

Given the research nature of this project, contributions should focus on:
1. **Exploring design alternatives** - Try different approaches, even if they conflict with current design
2. **Documenting trade-offs** - What works? What doesn't? Why?
3. **Use case discovery** - What problems does Prolog/Python integration actually solve?
4. **Testing paradigm boundaries** - Push the limits of what's possible

**Do not expect API stability.** Code written against today's API may not work tomorrow.

## License

[Your chosen license]

## Documentation

Parts of this documentation (including architecture guides and design documents) are **procedurally generated** to ground and guide the development process. This approach helps maintain consistency and clarity as the project evolves through rapid iteration.

## Credits

- Largely based on [libpython-clj](https://github.com/clj-python/libpython-clj) by Chris Nuernberger
- Uses [Scryer Prolog](https://github.com/mthom/scryer-prolog) FFI
- Inspired by libscryer-clj's machine-based lifecycle approach
