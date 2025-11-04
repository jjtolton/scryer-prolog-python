Hi @sporeking,

Thanks for the detailed bug report! The issue you're experiencing is related to how Scryer Prolog's FFI loads shared libraries on macOS arm64.

## Quick Test

Before we dive into the fix, can you try this minimal test to confirm the basic FFI is working? Save this as `test_macos_arm64_python312.pl`:

```prolog
:- use_module(library(charsio)).
:- use_module(library(process)).
:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(library(iso_ext)).
:- use_module(library(lambda)).
:- use_module(library(format)).
:- use_module(library(reif)).
:- use_module(library(dif)).
:- use_module(library(debug)).
:- use_module(library(os)).

:- use_module('../src/lib/python').

:- initialization(main).

main :-
    py_initialize([
        shared_library_path('/opt/homebrew/opt/python@3.12/Frameworks/Python.framework/Versions/3.12/lib/libpython3.12.dylib'),
        python_executable('/opt/homebrew/opt/python@3.12/bin/python3.12')
    ]),
    py_run_simple_string("import sys"),
    py_run_simple_string("print(f'Python version: {sys.version}')"),
    py_run_simple_string("print(f'Python prefix: {sys.prefix}')"),
    py_run_simple_string("print(f'Platform: {sys.platform}')"),
    py_run_simple_string("print(f'Architecture: {sys.implementation.name}')"),
    py_run_simple_string("print('macOS arm64 Python 3.12 test: SUCCESS!')"),
    py_finalize,
    halt.
```

**If using Homebrew Python**, adjust the paths if needed:
```bash
# Find your actual libpython path
ls -la /opt/homebrew/opt/python@3.12/Frameworks/Python.framework/Versions/3.12/lib/libpython3.12.dylib
```

**If using Conda Python**, use these paths instead:
```prolog
main :-
    py_initialize([
        shared_library_path('/opt/homebrew/Caskroom/miniconda/base/envs/YOUR_ENV_NAME/lib/libpython3.12.dylib'),
        python_executable('/opt/homebrew/Caskroom/miniconda/base/envs/YOUR_ENV_NAME/bin/python')
    ]),
    % ... rest of the test
```

**Run it:**
```bash
cd /path/to/scryer-python
scryer-prolog manual_tests/test_macos_arm64_python312.pl
```

## Expected Output

If working correctly, you should see:
```
Python version: 3.12.12 ...
Python prefix: /opt/homebrew/opt/python@3.12/Frameworks/Python.framework/Versions/3.12
Platform: darwin
Architecture: cpython
macOS arm64 Python 3.12 test: SUCCESS!
```

## Potential Issues

Based on your debugging, the issue is likely one of these:

1. **RTLD_GLOBAL issue** (most likely): Python C extensions and standard library modules need symbols from libpython to be globally visible. I just submitted [PR #3144](https://github.com/mthom/scryer-prolog/pull/3144) to add RTLD_GLOBAL support to Scryer Prolog's FFI. Once merged, this should fix the issue.

2. **FFI signature mismatch**: Some Python C API functions might have different signatures on arm64 macOS. However, since your minimal test with `Py_Initialize` and `Py_Finalize` worked, this is less likely.

3. **Library linking order**: On macOS arm64, the order libraries are loaded can matter.

## Workaround: Use RTLD_GLOBAL Branch

You can test with my RTLD_GLOBAL branch right now:

```bash
# Clone my fork with RTLD_GLOBAL support
git clone --branch rtld-global-support https://github.com/jjtolton/scryer-prolog.git
cd scryer-prolog
cargo build --release
./target/release/scryer-prolog /path/to/scryer-python/manual_tests/test_macos_arm64_python312.pl
```

This should work even with conda Python and will support importing NumPy, requests, and other packages.

## Please Report Back

Can you:
1. Try the test above and paste the exact output/error?
2. Confirm which Python you're using (Homebrew or Conda)?
3. Run `file $(which scryer-prolog)` and `file /path/to/libpython3.12.dylib` to confirm architectures match?

This will help us narrow down the exact issue!
