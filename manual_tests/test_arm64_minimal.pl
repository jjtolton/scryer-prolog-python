:- use_module(library(ffi)).
:- use_module(library(format)).
:- initialization(main).

main :-
    format("=== Minimal arm64 FFI Test ===~n", []),
    format("Testing ONLY Py_Initialize and Py_Finalize~n~n", []),

    LibPath = '/opt/homebrew/opt/python@3.12/Frameworks/Python.framework/Versions/3.12/lib/libpython3.12.dylib',

    format("Loading Python library: ~a~n", [LibPath]),

    catch(
        (
            use_foreign_module(LibPath, [
                'Py_Initialize'([], void),
                'Py_Finalize'([], void)
            ]),
            format("✓ FFI module loaded successfully~n", [])
        ),
        Error,
        (
            format("✗ FFI module loading FAILED: ~w~n", [Error]),
            halt(1)
        )
    ),

    format("Calling Py_Initialize...~n", []),
    catch(
        (
            ffi:'Py_Initialize',
            format("✓ Py_Initialize succeeded~n", [])
        ),
        Error2,
        (
            format("✗ Py_Initialize FAILED: ~w~n", [Error2]),
            halt(1)
        )
    ),

    format("Calling Py_Finalize...~n", []),
    catch(
        (
            ffi:'Py_Finalize',
            format("✓ Py_Finalize succeeded~n", [])
        ),
        Error3,
        (
            format("✗ Py_Finalize FAILED: ~w~n", [Error3]),
            halt(1)
        )
    ),

    format("~n=== MINIMAL TEST PASSED ===~n", []),
    halt(0).
