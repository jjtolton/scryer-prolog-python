# Package Metadata

## Version: 0.3.0

**Status**: Pre-release / Alpha

⚠️ **API Stability Warning**: This package is version 0.x.x, which means the API is **not yet stable** and may change in future releases. Breaking changes may occur between minor versions.

## Version History

### 0.3.0 (Current)
- 🚀 **Major Feature**: RTLD_GLOBAL support for Python C extensions
  - Now uses `use_foreign_module/3` with `scope(global)` option
  - Enables Python C extension imports (NumPy, SciPy, pandas, etc.)
  - Requires Scryer Prolog fork with RTLD_GLOBAL support (jjtolton/scryer-prolog)
  - libpython symbols now available for resolution by C extension modules
- 🧪 **Test Improvement**: Removed external network calls from Docker demo tests
  - Removed `requests` library dependency from conda demo
  - Removed GitHub API call that could fail due to network issues
  - Tests now focus on NumPy C extension support (validates RTLD_GLOBAL FFI)

### 0.2.2
- 🔧 **API Change**: Removed `LIBPYTHON_PATH` environment variable support
  - Follows libpython-clj convention (no custom env var)
  - Configuration now via `python.pl` file or auto-detection only
  - `python_library_path_user/1` must use strings (double quotes), not atoms
  - All path handling now uses strings consistently
  - Credits: @sporeking for bug report, @triska for string/atom explanation

### 0.2.1
- 🐛 **Bug Fix**: Fixed string vs atom handling in path detection
  - Fixed incorrect `atom_chars/2` conversion
  - Removed custom `get_env_var/2` wrapper

### 0.2.0
- ✅ Dictionary support for globals/locals
- ✅ Extended `py_run_simple_string/5` with globals/locals arguments
- ✅ Prolog ↔ Python type conversion (atoms, integers, floats, booleans)
- ✅ Value extraction from Python execution
- ✅ Dictionary operations: `py_dict_new/1`, `py_dict_set/3`, `py_dict_get/3`, `py_dict_to_list/2`
- ✅ Bidirectional conversion: `prolog_to_py_dict/2`, `py_dict_to_prolog/2`

### 0.1.0
- Initial Python integration using FFI
- Basic predicates: `py_initialize/0`, `py_finalize/0`, `py_run_simple_string/1`
- Simple string-based code execution
- State persistence across calls
- Error handling for Python exceptions

### Planned for 1.0.0 (Stable API)
- Stable, documented API
- Comprehensive test suite
- Auto-detection of Python library path
- Full semantic versioning guarantees

## Semantic Versioning (Post 1.0.0)

Once we reach 1.0.0, we will follow strict semantic versioning:
- **MAJOR** version for incompatible API changes
- **MINOR** version for backwards-compatible functionality additions
- **PATCH** version for backwards-compatible bug fixes

## Package Name

`scrypy` (formerly `scryer-prolog-python`)

## Dependencies

- Scryer Prolog >= 0.10.0
- Python >= 3.10 (with libpython3.10.so or equivalent)
- FFI library (built into Scryer Prolog)

## License

[To be determined]

## Maintainers

- J.J. Tolton
- Claude (AI Assistant)
