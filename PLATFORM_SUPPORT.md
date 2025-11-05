# Platform Support

## Supported Platforms

### ✅ Linux x86_64
**Status**: Fully supported and tested

- **Python versions**: 3.10, 3.11, 3.12
- **Distributions**: Ubuntu, Debian, Fedora, Arch
- **Package managers**: System Python, pyenv, Conda, uv
- **C extensions**: ✅ Working (requires RTLD_GLOBAL support - see below)

### 🚧 macOS arm64 (Apple Silicon)
**Status**: In development

- **Python versions**: 3.10, 3.11, 3.12
- **Installation**: Homebrew, Conda
- **Known issues**: FFI type signature compatibility (see [Issue #1](https://github.com/jjtolton/scrypy/issues/1))
- **Testing**: Automated CI via GitHub Actions on this branch

### 🚧 macOS x86_64 (Intel)
**Status**: Should work but untested

- **Python versions**: 3.10, 3.11, 3.12
- **Installation**: Homebrew, Conda, pyenv
- **Status**: Similar to arm64, likely same FFI issues

### ❌ Windows
**Status**: Not yet supported

- **Blocker**: FFI signatures need Windows-specific adjustments
- **Future**: Planned support once Unix platforms are stable

## Python C Extensions (NumPy, SciPy, pandas, etc.)

### RTLD_GLOBAL Requirement

Python C extensions require symbols from libpython to be globally visible. By default, Scryer Prolog's FFI loads libraries with `RTLD_LOCAL`, which hides symbols.

**Solution**: Use a Scryer Prolog build with RTLD_GLOBAL support:
- **PR**: https://github.com/mthom/scryer-prolog/pull/3144
- **Status**: Submitted, awaiting merge
- **Workaround**: Use the fork: https://github.com/jjtolton/scryer-prolog/tree/rtld-global-support

### Supported C Extensions (with RTLD_GLOBAL)

✅ **Tested and working** (Linux x86_64):
- NumPy
- Standard library: `math`, `socket`, `_random`, `_datetime`, `_json`
- requests (pure Python with C extensions for speed)

🚧 **Should work** (untested):
- SciPy
- pandas
- matplotlib
- PyTorch
- TensorFlow
- polars
- pyarrow

## macOS arm64 Compatibility Issues

### Issue #1: FFI Type Signatures

**Problem**: Some FFI function signatures cause `use_foreign_module` to fail on arm64 macOS.

**Root cause**: Potential ABI differences between x86_64 and arm64:
- Calling conventions differ (System V AMD64 vs ARM64)
- Type sizes are the same, but marshalling might differ
- libffi might have platform-specific quirks

**Fix in progress**:
- Changed `i64` to `long` for Py_ssize_t types (PyDict_Size, PyList_Size, PyList_GetItem)
- Testing via GitHub Actions on real M1 hardware
- Diagnostic tests to identify exact breakpoint

### Testing on macOS arm64

If you have Apple Silicon hardware and want to help test:

1. **Clone this branch**:
   ```bash
   git clone -b test/arm64-compatibility https://github.com/jjtolton/scrypy.git
   cd scrypy
   ```

2. **Install dependencies**:
   ```bash
   brew install python@3.12 rust
   ```

3. **Build Scryer Prolog**:
   ```bash
   git clone https://github.com/mthom/scryer-prolog.git
   cd scryer-prolog
   cargo build --release
   sudo cp target/release/scryer-prolog /usr/local/bin/
   ```

4. **Run diagnostic tests**:
   ```bash
   cd ../scrypy
   scryer-prolog manual_tests/test_arm64_minimal.pl
   scryer-prolog manual_tests/test_arm64_incremental.pl
   scryer-prolog manual_tests/test_arm64_diagnostics.pl
   ```

5. **Report results**: Comment on [Issue #1](https://github.com/jjtolton/scrypy/issues/1)

## Continuous Integration

### GitHub Actions

We use GitHub Actions for automated testing:

**Workflow**: `.github/workflows/test-macos-arm64.yml`
- Runs on: `macos-14` (M1 runners)
- Tests: Minimal, incremental, and full diagnostic suites
- Triggered: On push to `test/arm64-compatibility`, manual dispatch

**View results**: https://github.com/jjtolton/scrypy/actions

## Future Platform Support

### Short term
- ✅ Stabilize macOS arm64 support
- ✅ Add automated testing for macOS Intel
- 🔄 Merge RTLD_GLOBAL support upstream

### Medium term
- 🔄 Windows support
- 🔄 Linux arm64 (Raspberry Pi, AWS Graviton)
- 🔄 Add more Python versions (3.13+)

### Long term
- 🔄 BSD support (FreeBSD, OpenBSD)
- 🔄 Android (via Termux)

## Contributing

If you're testing on a new platform:

1. **Run the minimal test first**: `manual_tests/test_arm64_minimal.pl`
2. **Report results**: Open an issue with:
   - Platform details (`uname -a`, Python version)
   - Full error output
   - Result of `nm -D /path/to/libpython*.{so,dylib} | grep PyRun_SimpleString`
3. **Try incremental test**: Helps identify which FFI signature breaks
4. **Share system details**: Architecture, Python installation method, etc.

## References

- **Python C API docs**: https://docs.python.org/3/c-api/
- **libffi**: https://sourceware.org/libffi/
- **Scryer Prolog FFI**: https://github.com/mthom/scryer-prolog/blob/master/src/lib/ffi.pl
- **ARM64 calling convention**: https://developer.arm.com/documentation/102374/0101/Procedure-Call-Standard
