:- module(module_inspector, [
    get_public_predicates/2,
    get_predicate_docs/3
]).

:- use_module(library(lists)).
:- use_module(library(charsio)).

%% get_public_predicates(+ModuleFile, -PublicPredicates)
%
% Parse module directive to extract list of exported predicates.
% Reads the :- module(Name, ExportList) directive from the file.
%
% @param ModuleFile Path to the module file (string or atom)
% @param PublicPredicates List of Name/Arity terms for exported predicates
%
% Example:
%   ?- get_public_predicates("src/lib/python.pl", Preds).
%   Preds = [py_initialize/0, py_initialize/1, py_finalize/0, ...].
%
get_public_predicates(ModuleFile, PublicPredicates) :-
    open(ModuleFile, read, Stream),
    read_module_directive(Stream, ExportList),
    close(Stream),
    PublicPredicates = ExportList.

%% read_module_directive(+Stream, -ExportList)
%
% Read terms from stream until we find the module directive.
% Extracts the export list from :- module(Name, ExportList).
%
read_module_directive(Stream, ExportList) :-
    read_term(Stream, Term, []),
    (   Term = (:- module(_, ExportList))
    ->  true
    ;   Term = end_of_file
    ->  ExportList = []
    ;   read_module_directive(Stream, ExportList)
    ).

%% get_predicate_docs(+ModuleFile, +PredicateFunctor, -DocComments)
%
% Extract documentation comments for a specific predicate.
% Returns list of comment lines before the predicate definition.
%
% @param ModuleFile Path to the module file (string or atom)
% @param PredicateFunctor Predicate name/arity (e.g., py_initialize/0)
% @param DocComments List of documentation comment strings
%
% Example:
%   ?- get_predicate_docs("src/lib/python.pl", py_initialize/0, Docs).
%   Docs = ["%% py_initialize", "%", "% Initialize Python interpreter.", ...].
%
get_predicate_docs(ModuleFile, PredicateFunctor, DocComments) :-
    PredicateFunctor = Name/Arity,
    atom_chars(Name, NameChars),
    open(ModuleFile, read, Stream),
    collect_lines(Stream, Lines),
    close(Stream),
    find_predicate_docs(Lines, NameChars, Arity, DocComments).

%% collect_lines(+Stream, -Lines)
%
% Read all lines from stream into a list of character lists.
%
collect_lines(Stream, Lines) :-
    get_line_to_chars(Stream, FirstLine, []),
    (   FirstLine = []
    ->  Lines = []
    ;   collect_lines(Stream, RestLines),
        Lines = [FirstLine|RestLines]
    ).

%% find_predicate_docs(+Lines, +NameChars, +Arity, -DocComments)
%
% Search through lines to find predicate definition and collect preceding comments.
%
find_predicate_docs([], _, _, []).
find_predicate_docs([Line|Rest], NameChars, Arity, DocComments) :-
    % Check if this line is the predicate definition
    (   is_predicate_definition(Line, NameChars, Arity)
    ->  % Found it! Collect comments backwards from here
        collect_preceding_comments([Line|Rest], [], DocComments)
    ;   % Keep searching
        find_predicate_docs(Rest, NameChars, Arity, DocComments)
    ).

%% is_predicate_definition(+Line, +NameChars, +Arity)
%
% Check if a line is a predicate definition matching Name/Arity.
% Looks for patterns like "predicate_name(" at start of line.
%
is_predicate_definition(Line, NameChars, _Arity) :-
    append(NameChars, ['('|_], Line).

%% collect_preceding_comments(+AllLines, +Accumulator, -Comments)
%
% Collect comment lines before the current position.
% This is called after finding the predicate definition,
% and works backwards through preceding lines.
%
collect_preceding_comments([], Acc, Acc).
collect_preceding_comments([Line|Rest], Acc, Comments) :-
    % Check if line is a comment
    (   is_comment_line(Line)
    ->  collect_preceding_comments(Rest, [Line|Acc], Comments)
    ;   % Stop at first non-comment line
        Comments = Acc
    ).

%% is_comment_line(+Line)
%
% Check if a line is a comment (starts with %).
%
is_comment_line([Char|_]) :-
    Char = 0'%.
is_comment_line([Char|Rest]) :-
    (Char = 0' ; Char = 0'\t),  % Skip whitespace
    is_comment_line(Rest).
