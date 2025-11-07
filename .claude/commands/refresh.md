# Context Refresh - ScryPy Project

You are about to continue working on ScryPy, a Python integration library for Scryer Prolog via FFI. This command refreshes your context by reviewing key project files and current implementation status.

## 1. Check Scryer-Index (Available Predicates & Libraries)

Read `../scryer-index/README.md` to understand what Scryer Prolog predicates and libraries are available:
- Core libraries we can use
- FFI capabilities (`library(ffi)`)
- Blackboard system (`library(iso_ext)` - for state management)
- Data structure libraries (`library(lists)`, `library(assoc)`)
- I/O and networking capabilities

## 2. Review Core Documentation (What We've Built)

Read these key project files to understand the current state:

### README.md
- Current version (v0.3.0 - RTLD_GLOBAL support)
- User-facing API and examples
- Installation instructions
- Current capabilities and limitations
- Research project status and warnings

### ARCHITECTURE.md
- FFI-based direct Python C API architecture
- Memory management patterns (reference counting)
- State management via blackboard (`bb_put/bb_get`)
- Module structure and organization
- Current type conversion implementations

### ROADMAP_TYPE_BRIDGING.md
- Comprehensive roadmap for type expansion
- Phase 1-7 implementation plan
- Design questions and decisions
- Success metrics per phase
- Inspiration from libpython-clj

### PACKAGE.md
- Version history and changelog
- Current phase: v0.3.0 (RTLD_GLOBAL support)
- Breaking changes and migrations
- Next planned features

## 3. Review Engineering Principles (CLAUDE.md)

Read `CLAUDE.md` sections on:

### TEST-DRIVEN DEVELOPMENT (ABSOLUTE REQUIREMENT)
1. **RED**: Write test FIRST. See it FAIL.
2. **GREEN**: Implement minimal code to pass.
3. **REFACTOR**: Clean up while keeping tests green.

**NEVER** write code without writing the test first. This is non-negotiable.

### STRING QUOTING CONVENTION (CRITICAL)

**Python CODE (in `py_run_simple_string`)**: Use double quotes `"..."`
- Supports multi-line strings
- Consistent with Python conventions
- Example: `py_run_simple_string("x = 2 + 2")`

**Python DATA (values, variable names)**: Use single quotes `'...'` (atoms)
- Required by `prolog_value_to_pyobject/2` which expects atoms
- Example: `py_dict_set(Dict, name, 'Alice')`

### MEMORY MANAGEMENT PATTERNS

- **NEW references** (we own, must decref): `PyDict_New()`, `PyLong_FromLong()`, etc.
- **BORROWED references** (don't own, must NOT decref): `PyDict_GetItemString()`, etc.
- Always use `setup_call_cleanup` for automatic cleanup
- Track ownership explicitly when mixing NEW and BORROWED refs

### GIT POLICIES

**NEVER include Claude attribution in commits**. Write normal, professional commit messages.

**NEVER push to GitHub without explicit permission**. Always commit locally first.

## 4. Review Current Implementation Status

After reading the docs, check the current codebase:

```bash
# Check implementation
ls -R src/
ls -R examples/
ls -R tests/

# Run core tests
scryer-prolog examples/tests/test_all_types.pl
scryer-prolog examples/tests/test_dict.pl
scryer-prolog examples/tests/test_globals_locals.pl
scryer-prolog examples/tests/test_memory_management.pl

# Run demos
scryer-prolog examples/python_demo.pl
scryer-prolog examples/python_demo_v2.pl
```

## 5. Check CI/CD Status

Review the continuous integration setup:
- `.github/workflows/test.yml` - GitHub Actions configuration
- `test-ci.sh` - Local Docker-based CI testing
- `test-ci.Dockerfile` - Mimics GitHub Actions environment

Run local CI test:
```bash
./test-ci.sh
```

## 6. Check Docker Demos

Review Docker integration examples:
- `examples/docker/` - Basic demo with uv and requests
- `examples/docker-conda/` - Conda environment with NumPy

Test Docker builds:
```bash
docker build -f examples/docker/Dockerfile -t scrypy-demo .
docker run --rm scrypy-demo

docker build -f examples/docker-conda/Dockerfile -t scrypy-conda .
docker run --rm scrypy-conda
```

## 7. Current Capabilities (v0.3.0)

### Type Conversions
**Prolog → Python**:
- Atoms → Python strings
- Integers → Python ints
- Floats → Python floats
- Booleans → Python True/False
- Lists of key-value pairs → Python dicts

**Python → Prolog**:
- Python strings → Atoms
- Python ints → Integers
- Python floats → Floats
- Python booleans → true/false
- Python dicts → Lists of key-value pairs

### Core Operations
- Dictionary create/get/set operations
- Code execution with globals/locals (`py_run_simple_string/5`)
- Module import and initialization
- RTLD_GLOBAL support for C extensions (NumPy, etc.)

### Known Limitations
- No Python lists/tuples (Phase 1)
- No Python None (Phase 1)
- No attribute access (Phase 2)
- No item access (Phase 2)
- No function calls (Phase 3)
- No iterators (Phase 4)

## 8. Context Summary

After reviewing all the above, provide a brief summary:

1. **Project Status**:
   - Current version
   - What works (types, dicts, code execution)
   - Test coverage (5 test suites passing)
   - CI/CD status

2. **Available Tools**:
   - FFI capabilities from Scryer Prolog
   - Python C API functions currently bound
   - Blackboard state management

3. **Engineering Discipline**: Confirm you understand:
   - TDD requirements (test first, always)
   - String quoting convention (double for code, single for data)
   - Memory management (setup_call_cleanup pattern)
   - Git policies (no attribution, no auto-push)

4. **Roadmap Context**:
   - Current phase: v0.3.0 (RTLD_GLOBAL support - complete)
   - Next phase: v0.4.0 (Python None, Lists, Tuples)
   - Long-term: Phases 2-7 (objects, functions, iteration, NumPy)

5. **Next Steps**: What makes sense to work on based on:
   - Roadmap priorities
   - User requests
   - Test coverage gaps
   - Integration opportunities

## Remember

**TESTS FIRST. MEMORY SAFETY. STRING QUOTING.**

This is how we build a robust Python integration that is:
- **Safe**: Proper reference counting, no memory leaks
- **Correct**: Comprehensive test coverage
- **Maintainable**: Clear patterns, good documentation
- **Research-grade**: Exploring declarative/imperative integration

Every line of code should be:
- **Testable**: Covered by unit or integration tests
- **Safe**: Proper memory management with cleanup
- **Documented**: Clear purpose and usage patterns
- **Correct**: Follows quoting and FFI conventions

## Key Design Questions (from Roadmap)

Be aware of open design questions as you work:
1. **Object lifetime** - Manual vs automatic vs scope-based?
2. **None representation** - `none` atom vs `[]` vs `false` vs fail?
3. **List conversion** - Deep copy vs lazy vs hybrid?
4. **Mutation** - Pure (threading) vs impure (side effects)?
5. **Error handling** - Throw vs term capture?

Now proceed with the context refresh by reading the files mentioned above.
