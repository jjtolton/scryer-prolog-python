:- use_module('module_inspector').
:- use_module('doc_parser').
:- use_module('test_coverage').
:- use_module('config_validator').
:- use_module(library(lists)).
:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- initialization(main).

main :-
    write('=== Documentation Validation System ==='), nl, nl,

    % 1. Get public predicates
    write('Step 1: Extracting public predicates...'), nl,
    get_public_predicates("src/lib/python.pl", PublicPreds),
    length(PublicPreds, NumPreds),
    write('  Found '), write(NumPreds), write(' public predicates'), nl, nl,

    % 2. Validate inline documentation examples in source code
    write('Step 2: Validating inline documentation examples...'), nl,
    validate_source_docs("src/lib/python.pl", DocErrors),
    (   DocErrors = []
    ->  write('  ✓ All inline documentation examples use correct syntax'), nl
    ;   write('  ✗ Found errors in inline documentation:'), nl,
        maplist(print_doc_error, DocErrors)
    ),
    nl,

    % 3. Validate README examples
    write('Step 3: Validating README.md examples...'), nl,
    validate_readme_docs("README.md", ReadmeErrors),
    (   ReadmeErrors = []
    ->  write('  ✓ All README examples use correct syntax'), nl
    ;   write('  ✗ Found errors in README:'), nl,
        maplist(print_doc_error, ReadmeErrors)
    ),
    nl,

    % 4. Check test coverage for string argument enforcement
    write('Step 4: Checking test coverage for string argument enforcement...'), nl,
    check_string_arg_coverage(CoverageReport),
    (   CoverageReport = complete
    ->  write('  ✓ All predicates with string arguments have type enforcement tests'), nl
    ;   CoverageReport = missing(Missing),
        write('  ✗ Missing tests for: '), write(Missing), nl
    ),
    nl,

    % 5. Validate config example file
    write('Step 5: Validating python.pl.example...'), nl,
    validate_config("python.pl.example"),
    nl,

    % 6. Summary
    write('=== Validation Summary ==='), nl,
    (   DocErrors = [], ReadmeErrors = [], CoverageReport = complete
    ->  write('✓ All validation checks passed!'), nl,
        halt(0)
    ;   write('✗ Some validation checks failed'), nl,
        halt(1)
    ).

validate_source_docs(SourceFile, Errors) :-
    % For each public predicate, get its docs and validate examples
    get_public_predicates(SourceFile, Predicates),
    validate_predicates_docs(SourceFile, Predicates, Errors).

validate_predicates_docs(_, [], []).
validate_predicates_docs(SourceFile, [Pred|Rest], AllErrors) :-
    get_predicate_docs(SourceFile, Pred, Docs),
    extract_code_examples(Docs, Examples),
    maplist(validate_string_arguments, Examples, ErrorLists),
    flatten_list(ErrorLists, PredErrors),
    validate_predicates_docs(SourceFile, Rest, RestErrors),
    append(PredErrors, RestErrors, AllErrors).

validate_readme_docs(ReadmeFile, Errors) :-
    % Read README and extract code examples
    open(ReadmeFile, read, Stream),
    collect_readme_lines(Stream, Lines),
    close(Stream),
    extract_code_examples(Lines, Examples),
    maplist(validate_string_arguments, Examples, ErrorLists),
    flatten_list(ErrorLists, Errors).

%% flatten_list(+ListOfLists, -FlatList)
% Flatten implementation using DCG seqq//1 from library(dcgs)
flatten_list(Lists, Flattened) :-
    phrase(seqq(Lists), Flattened).

collect_readme_lines(Stream, Lines) :-
    get_line_to_chars(Stream, FirstLine, []),
    (   FirstLine = []
    ->  Lines = []
    ;   collect_readme_lines(Stream, RestLines),
        Lines = [FirstLine|RestLines]
    ).

print_doc_error(error(Line, Predicate, Error)) :-
    write('    Line '), write(Line), write(': '),
    write(Predicate), write(' - '), write(Error), nl.
