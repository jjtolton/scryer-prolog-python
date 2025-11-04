# Design: Globals and Locals Support

## Goal

Extend `py_run_simple_string` to accept and return `globals` and `locals` dictionaries, similar to libpython-clj.

## Python C API Functions Needed

### Dictionary Operations
- `PyDict_New()` - Create empty dict
- `PyDict_SetItemString(dict, key, value)` - Set string key
- `PyDict_GetItemString(dict, key)` - Get by string key
- `PyDict_Keys(dict)` - Get list of keys
- `PyDict_Size(dict)` - Number of items

### String/Value Conversion
- `PyUnicode_FromString(str)` - Create Python string from C string
- `PyLong_FromLong(long)` - Create Python int from C long
- `PyFloat_FromDouble(double)` - Create Python float from C double
- `PyUnicode_AsUTF8(str)` - Get C string from Python string
- `PyLong_AsLong(obj)` - Get C long from Python int
- `PyFloat_AsDouble(obj)` - Get C double from Python float

### Module Operations (already conceptually used)
- `PyImport_AddModule("__main__")` - Get __main__ module
- `PyModule_GetDict(module)` - Get module's dict

## Prolog Representation

We'll use a **simple list of Key-Value pairs**, not full assoc trees:

```prolog
% Example dictionary representation
Dict = [name-'Alice', age-30, score-95.5]
```

**Why not use library(assoc)?**
- Simpler for users (just lists)
- Matches Python's unordered dict nature
- Easier FFI conversion
- Similar to how libpython-clj returns maps

## API Design

### Current API
```prolog
py_run_simple_string(+Code)
```

### New API Options

**Option 1: Separate predicates**
```prolog
py_run_simple_string(+Code)
py_run_simple_string(+Code, +Globals, +Locals, -NewGlobals, -NewLocals)
```

**Option 2: Using options (more flexible)**
```prolog
py_run_simple_string(+Code)
py_run_simple_string(+Code, +Options)
% Options = [globals(GlobalsIn), locals(LocalsIn),
%            new_globals(GlobalsOut), new_locals(LocalsOut)]
```

**Recommendation: Option 1** - clearer, more Prolog-like

## Implementation Plan

### Phase 1: Basic Dictionary Support
1. Add FFI bindings for Python dict operations
2. Create helper predicates:
   - `py_dict_new(-DictPtr)` - Create empty Python dict
   - `py_dict_set(+DictPtr, +Key, +Value)` - Set item (atoms/numbers only)
   - `py_dict_get(+DictPtr, +Key, -Value)` - Get item
   - `py_dict_to_list(+DictPtr, -List)` - Convert to Key-Value list

### Phase 2: Prolog ↔ Python Conversion
3. Create conversion predicates:
   - `prolog_list_to_py_dict(+List, -DictPtr)` - Convert Prolog list to Python dict
   - `py_dict_to_prolog_list(+DictPtr, -List)` - Convert Python dict to Prolog list
   - Handle basic types: atoms (strings), integers, floats

### Phase 3: Extended run_simple_string
4. Implement `py_run_simple_string/5`:
   ```prolog
   py_run_simple_string(+Code, +GlobalsIn, +LocalsIn, -GlobalsOut, -LocalsOut)
   ```
5. Use `PyRun_String` with explicit globals/locals dicts

## Example Usage

```prolog
?- py_initialize.

% Run with custom globals
?- py_run_simple_string('x = a + b',
                        [a-5, b-10],
                        [],
                        NewGlobals,
                        _NewLocals).
NewGlobals = [a-5, b-10, x-15].

% Run with separate locals
?- py_run_simple_string('y = x * 2',
                        [x-10],
                        [z-3],
                        Globals,
                        Locals).
Globals = [x-10],
Locals = [z-3, y-20].

% Get specific value back
?- py_run_simple_string('result = 2 ** 10', [], [], G, _),
   member(result-R, G).
R = 1024.
```

## Type Conversion Strategy

### Phase 1 (Simple Types Only)
| Prolog Type | Python Type | Notes |
|-------------|-------------|-------|
| Integer | int | Direct conversion |
| Float | float | Direct conversion |
| Atom | str | UTF-8 string |

### Phase 2 (Future - Complex Types)
| Prolog Type | Python Type | Notes |
|-------------|-------------|-------|
| List | list | Recursive conversion |
| Compound | dict | Named args as keys |

## Implementation Steps

1. ✅ Study library(assoc) API
2. ⬜ Add Python dict C API FFI bindings
3. ⬜ Implement basic dict creation/manipulation
4. ⬜ Implement Prolog ↔ Python conversion
5. ⬜ Extend py_run_simple_string/5
6. ⬜ Add tests and examples
7. ⬜ Update documentation

## Questions to Consider

1. **Memory management**: Who owns the dict pointers? Need careful ref counting
2. **Error handling**: What if conversion fails?
3. **Backwards compatibility**: Keep py_run_simple_string/1 unchanged
4. **Performance**: Dict conversion overhead?
5. **Nested structures**: Do we support lists/dicts as values? (Phase 2)

## References

- libpython-clj `run-simple-string`: Lines 445-475 in python.clj
- Python C API: https://docs.python.org/3/c-api/dict.html
- Scryer Prolog assoc: src/lib/assoc.pl
