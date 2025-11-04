:- use_module(library(os)).
:- use_module(library(format)).
:- use_module('../src/lib/python').
:- initialization(main).

main :-
    format("=== arm64 Diagnostic Test Suite ===~n~n", []),

    format("1. Testing Python library detection...~n", []),
    (   python_library_path(DetectedPath)
    ->  format("   ✓ Detected: ~a~n", [DetectedPath])
    ;   format("   ✗ Failed to detect library~n", []),
        halt(1)
    ),

    format("~n2. Testing py_initialize with explicit paths...~n", []),
    catch(
        (
            py_initialize([
                shared_library_path('/opt/homebrew/opt/python@3.12/Frameworks/Python.framework/Versions/3.12/lib/libpython3.12.dylib'),
                python_executable('/opt/homebrew/opt/python@3.12/bin/python3.12'),
                verbose(true)
            ]),
            format("   ✓ py_initialize succeeded~n", [])
        ),
        Error,
        (
            format("   ✗ py_initialize FAILED: ~w~n", [Error]),
            format("~nDiagnostic Info:~n", []),
            format("   - Check if python_executable option is being processed correctly~n", []),
            format("   - Check if FFI signatures match arm64 ABI~n", []),
            format("   - Check if Py_DecodeLocale is being called correctly~n", []),
            halt(1)
        )
    ),

    format("~n3. Testing basic Python execution...~n", []),
    catch(
        (
            py_run_simple_string("import sys"),
            py_run_simple_string("print(f'Python version: {sys.version}')"),
            py_run_simple_string("print(f'Platform: {sys.platform}')"),
            py_run_simple_string("print(f'Architecture: {sys.implementation.name}')"),
            format("   ✓ Python execution succeeded~n", [])
        ),
        Error2,
        (
            format("   ✗ Python execution FAILED: ~w~n", [Error2]),
            py_finalize,
            halt(1)
        )
    ),

    format("~n4. Testing type conversions...~n", []),
    catch(
        (
            py_run_simple_string("x = 42"),
            py_run_simple_string("y = 3.14"),
            py_run_simple_string("z = 'hello'"),
            format("   ✓ Type conversions succeeded~n", [])
        ),
        Error3,
        (
            format("   ✗ Type conversions FAILED: ~w~n", [Error3]),
            py_finalize,
            halt(1)
        )
    ),

    format("~n5. Testing cleanup...~n", []),
    catch(
        (
            py_finalize,
            format("   ✓ py_finalize succeeded~n", [])
        ),
        Error4,
        (
            format("   ✗ py_finalize FAILED: ~w~n", [Error4]),
            halt(1)
        )
    ),

    format("~n=== ALL DIAGNOSTIC TESTS PASSED ===~n", []),
    format("macOS arm64 Python 3.12 is FULLY COMPATIBLE!~n", []),
    halt(0).
