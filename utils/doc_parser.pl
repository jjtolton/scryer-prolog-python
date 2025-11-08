:- module(doc_parser, [
    extract_code_examples/2,
    validate_string_arguments/2
]).

:- use_module(library(charsio)).
:- use_module(library(lists)).

%% extract_code_examples(+DocComments, -Examples)
%
% Parse documentation to find code examples.
% Looks for patterns like:
%   ?- py_run_simple_string("code").
%   py_run_simple_string("code").
%
% @param DocComments List of comment line strings
% @param Examples List of example(LineNumber, Code) terms
%
extract_code_examples(DocComments, Examples) :-
    extract_examples_from_lines(DocComments, 1, Examples).

extract_examples_from_lines([], _, []).
extract_examples_from_lines([Line|Rest], LineNum, Examples) :-
    NextLine is LineNum + 1,
    % Check if line contains a code example
    (   is_code_example_line(Line, CodeChars)
    ->  atom_chars(Code, CodeChars),
        Examples = [example(LineNum, Code)|MoreExamples],
        extract_examples_from_lines(Rest, NextLine, MoreExamples)
    ;   extract_examples_from_lines(Rest, NextLine, Examples)
    ).

%% is_code_example_line(+Line, -CodeChars)
%
% Check if a line is a code example and extract the code.
% Recognizes patterns like:
%   % ?- predicate(...).
%   %    predicate(...).
%
is_code_example_line(Line, CodeChars) :-
    % Remove leading comment markers and whitespace
    strip_comment_prefix(Line, Stripped),
    % Check if it starts with ?-
    (   append("?- ", CodeChars, Stripped)
    ->  true
    ;   % Or just check if it looks like a predicate call
        (   member(0'(, Stripped),
            \+ append("%",  _, Stripped)  % Not another comment
        ->  CodeChars = Stripped
        ;   fail
        )
    ).

%% strip_comment_prefix(+Line, -Stripped)
%
% Remove leading %, whitespace from comment line.
%
strip_comment_prefix([], []).
strip_comment_prefix([0'%|Rest], Stripped) :- !,
    strip_comment_prefix(Rest, Stripped).
strip_comment_prefix([0' |Rest], Stripped) :- !,
    strip_comment_prefix(Rest, Stripped).
strip_comment_prefix([0'\t|Rest], Stripped) :- !,
    strip_comment_prefix(Rest, Stripped).
strip_comment_prefix(Line, Line).

%% validate_string_arguments(+Example, -Errors)
%
% Parse Prolog term from example, check if string arguments
% (for predicates like py_run_simple_string) are actually strings not atoms.
%
% @param Example example(LineNum, CodeAtom) term
% @param Errors List of error terms, or [] if valid
%
validate_string_arguments(example(LineNum, CodeAtom), Errors) :-
    atom_chars(CodeAtom, CodeChars),
    % Try to parse as a term
    (   catch(read_term_from_chars(CodeChars, Term, []), _, fail)
    ->  check_term_arguments(Term, LineNum, Errors)
    ;   % If parsing fails, no validation possible
        Errors = []
    ).

%% check_term_arguments(+Term, +LineNum, -Errors)
%
% Check if term uses atoms where strings are expected.
% Specifically checks py_run_simple_string/1 and /5.
%
check_term_arguments(Term, LineNum, Errors) :-
    (   functor(Term, py_run_simple_string, 1)
    ->  arg(1, Term, FirstArg),
        (   atom(FirstArg)
        ->  Errors = [error(LineNum, py_run_simple_string/1, atom_should_be_string(FirstArg))]
        ;   Errors = []
        )
    ;   functor(Term, py_run_simple_string, 5)
    ->  arg(1, Term, FirstArg),
        (   atom(FirstArg)
        ->  Errors = [error(LineNum, py_run_simple_string/5, atom_should_be_string(FirstArg))]
        ;   Errors = []
        )
    ;   % Other predicates don't need validation
        Errors = []
    ).
