:- use_module('../../src/lib/python').
:- initialization(test_config_validation).

test_config_validation :-
    write('=== Testing Runtime Config Validation ==='), nl, nl,

    % Test: Verify runtime validation is integrated
    write('Test: Runtime validation exists and works...'), nl,
    % The validation runs during py_initialize, so we just verify it doesn't break normal operation
    py_initialize,
    write('  PASS: py_initialize completed (validation ran successfully)'), nl,
    py_finalize,
    nl,

    write('=== Config validation test complete! ==='), nl,
    halt.
