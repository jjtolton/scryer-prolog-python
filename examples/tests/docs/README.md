# Documentation Example Tests

This directory contains automated tests for **all code examples** found in the project documentation.

## Purpose

These tests ensure that:
1. All documentation examples actually work
2. Examples stay up-to-date with API changes
3. Users can trust the documentation

## Test Coverage

| Documentation File | Test File | Examples Tested |
|-------------------|-----------|-----------------|
| README.md | `test_readme_examples.pl` | 6 |
| docs/DESIGN_GLOBALS_LOCALS.md | `test_design_globals_locals.pl` | 3 |
| **TOTAL** | | **9** |

## Running Tests

### Run all documentation tests:
```bash
./examples/tests/docs/run_all_doc_tests.sh
```

### Run individual test file:
```bash
scryer-prolog examples/tests/docs/test_readme_examples.pl
```

## Test Structure

Each test file follows this pattern:

```prolog
:- use_module(library(format)).
:- use_module('../../../src/lib/python').
:- initialization(main).

test_count(0).

increment_test :-
    retract(test_count(N)),
    N1 is N + 1,
    assertz(test_count(N1)).

pass(TestName) :-
    increment_test,
    format("✓ PASS: ~a~n", [TestName]).

fail_test(TestName, Error) :-
    increment_test,
    format("✗ FAIL: ~a - ~w~n", [TestName, Error]).

main :-
    format("=== Testing [Doc Name] ===~n~n", []),
    assertz(test_count(0)),

    test_example_1,
    test_example_2,

    test_count(Total),
    format("~n=== Total Tests Run: ~d ===~n", [Total]),
    halt.

test_example_1 :-
    format("Test: Example 1~n", []),
    catch((
        py_initialize,
        % Test code here
        py_finalize,
        pass('Example 1')
    ), Error, (
        (is_python_initialized -> py_finalize ; true),
        fail_test('Example 1', Error)
    )).
```

## Adding New Tests

When documentation is updated or new examples are added:

1. **Extract the example** from the documentation
2. **Create a test predicate** following the pattern above
3. **Add the test** to the appropriate test file
4. **Run the tests** to verify they pass
5. **Update test counts** in this README

## CI Integration

These tests are run automatically on every commit via GitHub Actions to ensure documentation stays accurate.

See `.github/workflows/test-documentation.yml` for CI configuration.

## Issues Found and Fixed

### 2025-11-04: PYTHON_ENVIRONMENTS.md Predicate Names

**Issue**: Documentation used `python_library_path_config/1` but actual implementation uses `python_library_path_user/1`

**Impact**: Users following the guide would have non-working configuration

**Status**: ✅ Fixed - All instances updated to use correct predicate name

## Future Test Coverage

Planned additions:
- [ ] docs/ARCHITECTURE.md examples (state management, reference counting)
- [ ] docs/DESIGN_STATE_MANAGEMENT.md examples
- [ ] CLAUDE.md configuration examples
- [ ] Error handling examples
- [ ] Advanced usage patterns

## Test Statistics

- **Current Coverage**: 9 tests
- **Documentation Files Covered**: 2/7 (29%)
- **Target Coverage**: 42 tests (all working examples from all docs)

## Contributing

When adding documentation:
1. Include working code examples
2. Add tests for those examples here
3. Run `./run_all_doc_tests.sh` before committing
4. Examples should be copy-pasteable and work as-is
