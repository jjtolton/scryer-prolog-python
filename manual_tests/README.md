# Manual/Exploratory Tests

This directory contains **manual functional tests** for exploring and verifying different Python environment configurations. These are **not** automated test suites - they are interactive scripts for testing specific setups.

## Purpose

These tests are used for:
- Validating different Python environment managers (uv, conda, pyenv)
- Testing new configuration options
- Debugging environment-specific issues
- Documenting working configurations

## Running Tests

Each test file is a standalone Prolog script. Run them directly:

```bash
scryer-prolog manual_tests/test_uv.pl
scryer-prolog manual_tests/test_conda.pl
scryer-prolog manual_tests/test_verbose.pl
```

## Test Files

### test_uv.pl
Tests uv-managed Python with virtual environment using `python_executable` option.

**Requirements:**
- uv installed
- Python 3.11.14 installed via uv
- Virtual environment at `/path/to/.venv`

### test_conda.pl
Tests Conda/Anaconda Python environment using `python_home` option.

**Requirements:**
- Conda/Anaconda installed
- Python environment with conda

### test_verbose.pl
Tests verbose mode initialization option for debugging Python setup.

**Requirements:**
- Any working Python installation
- Used for debugging initialization issues

## Note

These are **exploratory tests**, not regression tests. They:
- Require manual verification of output
- May need path adjustments for your system
- Are meant to be modified and experimented with
- Serve as configuration examples

For comprehensive usage examples, see the `examples/` directory.
