:- module(test_framework, [
    run_test/2,
    reset_test_state/0,
    report_results/0,
    note_success/1,
    note_failure/1
]).

:- use_module(library(lists)).

:- dynamic(test_failure/1).
:- dynamic(test_success/1).

reset_test_state :-
    retractall(test_failure(_)),
    retractall(test_success(_)).

note_failure(TestName) :-
    assertz(test_failure(TestName)).

note_success(TestName) :-
    assertz(test_success(TestName)).

run_test(TestName, Goal) :-
    (   call(Goal) ->
        note_success(TestName),
        format("✅ ~w~n", [TestName])
    ;   note_failure(TestName),
        format("❌ ~w~n", [TestName])
    ).

report_results :-
    findall(F, test_failure(F), Failures),
    findall(S, test_success(S), Successes),
    length(Failures, FailCount),
    length(Successes, PassCount),
    nl,
    write('========================================'), nl,
    write('Test Results:'), nl,
    format('  Passed: ~d~n', [PassCount]),
    format('  Failed: ~d~n', [FailCount]),
    write('========================================'), nl,
    (   FailCount > 0 ->
        nl, write('Failed tests:'), nl,
        maplist(report_failure, Failures),
        nl
    ;   nl, write('✅ All tests passed!'), nl, nl
    ).

report_failure(TestName) :-
    format('  FAIL: ~w~n', [TestName]).
