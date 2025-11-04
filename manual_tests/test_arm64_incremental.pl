:- use_module(library(ffi)).
:- use_module(library(format)).
:- initialization(main).

main :-
    format("=== Incremental arm64 FFI Test ===~n", []),
    format("Adding functions one at a time to find breakpoint~n~n", []),

    LibPath = '/opt/homebrew/opt/python@3.12/Frameworks/Python.framework/Versions/3.12/lib/libpython3.12.dylib',

    test_function_set(1, LibPath, [
        'Py_Initialize'([], void),
        'Py_Finalize'([], void)
    ]),

    test_function_set(2, LibPath, [
        'Py_Initialize'([], void),
        'Py_Finalize'([], void),
        'PyRun_SimpleString'([cstr], int)
    ]),

    test_function_set(3, LibPath, [
        'Py_Initialize'([], void),
        'Py_Finalize'([], void),
        'PyRun_SimpleString'([cstr], int),
        'Py_SetPythonHome'([ptr], void),
        'Py_SetProgramName'([ptr], void),
        'Py_DecodeLocale'([cstr, ptr], ptr)
    ]),

    test_function_set(4, LibPath, [
        'Py_Initialize'([], void),
        'Py_Finalize'([], void),
        'PyRun_SimpleString'([cstr], int),
        'Py_SetPythonHome'([ptr], void),
        'Py_SetProgramName'([ptr], void),
        'Py_DecodeLocale'([cstr, ptr], ptr),
        'PyDict_New'([], ptr),
        'PyDict_Size'([ptr], i64),
        'PyList_Size'([ptr], i64)
    ]),

    format("~n=== ALL INCREMENTAL TESTS PASSED ===~n", []),
    halt(0).

test_function_set(N, LibPath, Functions) :-
    format("~nTest Set ~d: Loading ~d functions...~n", [N, N]),
    catch(
        (
            use_foreign_module(LibPath, Functions),
            format("  ✓ Set ~d loaded successfully~n", [N])
        ),
        Error,
        (
            format("  ✗ Set ~d FAILED: ~w~n", [N, Error]),
            format("  This is the breakpoint! Check the last function added.~n", []),
            halt(1)
        )
    ).
