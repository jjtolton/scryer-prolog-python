:- use_module(library(lists)).
:- use_module('module_inspector').
:- initialization(main).

main :-
    write('=== Testing Module Inspector ==='), nl, nl,

    write('Test 1: Get public predicates from python.pl'), nl,
    get_public_predicates('src/lib/python.pl', Preds),
    length(Preds, N),
    write('  Found '), write(N), write(' public predicates'), nl,
    write('  First 5: '), nl,
    take(5, Preds, First5),
    maplist(write_pred, First5),
    nl,

    halt.

take(0, _, []) :- !.
take(_, [], []) :- !.
take(N, [H|T], [H|R]) :-
    N > 0,
    N1 is N - 1,
    take(N1, T, R).

write_pred(Pred) :-
    write('    - '), write(Pred), nl.
