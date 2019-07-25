:- module turbolisp.
%=============================================================================%
% Main module for TurboLisp. Includes wrappers for the parser.
%
% This does NOT include the runtime (see tl_runtime), and instead operates
% simply as an S-expression parser.
:- interface.
%=============================================================================%

:- use_module io.
:- import_module list.

:- type element ---> atom(string) ; list(list.list(element)).

%-----------------------------------------------------------------------------%

:- pred parse(io.res(list.list(element)), io.io, io.io).
:- mode parse(out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- pred parse(io.text_input_stream, io.res(list.list(element)), io.io, io.io).
:- mode parse(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- pred parse_string(string, io.res(list.list(element))).
:- mode parse_string(in, out) is det.

%-----------------------------------------------------------------------------%
% parse_string_between(Str, Start, End, Result)
:- pred parse_string_between(string, int, int, io.res(list.list(element))).
:- mode parse_string_between(in, in, in, out) is det.

%=============================================================================%
:- implementation.
%=============================================================================%

:- use_module stream.

:- include_module turbolisp.parser.
:- include_module turbolisp.string_stream.

:- use_module turbolisp.parser.
:- use_module turbolisp.string_stream.

%-----------------------------------------------------------------------------%

:- type list == list.list(element).

%-----------------------------------------------------------------------------%

:- pred parse(io.text_input_stream,
    list.list(element),
    io.res(list.list(element)),
    io.io, io.io).
:- mode parse(in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

parse(Result, !IO) :- io.input_stream(Stream, !IO), parse(Stream, Result, !IO).

%-----------------------------------------------------------------------------%

parse(Stream, Result, !IO) :- parse(Stream, [], Result, !IO).

%-----------------------------------------------------------------------------%

:- func io_errors = turbolisp.parser.errors(io.error).
io_errors = turbolisp.parser.errors(
    io.make_io_error("Unexpected EOF"),
    io.make_io_error("Unmatched close paren")).

%-----------------------------------------------------------------------------%

:- func ss_errors = turbolisp.parser.errors(turbolisp.string_stream.error).
ss_errors = turbolisp.parser.errors(
    turbolisp.string_stream.error("Unexpected EOF"),
    turbolisp.string_stream.error("Unmatched close paren")).

%-----------------------------------------------------------------------------%

parse(Stream, List, Result, !IO) :-
    turbolisp.parser.parse(Stream, io_errors, ElementResult, !IO),
    (
        ElementResult = stream.ok(Element),
        parse(Stream, [Element|List], Result, !IO)
    ;
        ElementResult = stream.eof,
        Result = io.ok(list.reverse(List))
    ;
        ElementResult = stream.error(E),
        Result = io.error(E)
    ).

%-----------------------------------------------------------------------------%

:- pred parse_string_stream(string,
    list.list(turbolisp.element),
    turbolisp.string_stream.string_range,
    io.res(list.list(element))).
:- mode parse_string_stream(in, in, di, out) is det.

parse_string_stream(String, List, StringRangeIn, Result) :-
    turbolisp.parser.parse(String, ss_errors, ElementResult, StringRangeIn, StringRangeOut),
    (
        ElementResult = stream.ok(Element),
        parse_string_stream(String, [Element|List], StringRangeOut, Result)
    ;
        ElementResult = stream.eof,
        Result = io.ok(list.reverse(List))
    ;
        ElementResult = stream.error(turbolisp.string_stream.error(E)),
        Result = io.error(io.make_io_error(E))
    ).

%-----------------------------------------------------------------------------%

parse_string(Str, Result) :-
    parse_string_stream(Str, [], turbolisp.string_stream.init(Str), Result).

%-----------------------------------------------------------------------------%

parse_string_between(Str, Start, End, Result) :-
    parse_string_stream(Str, [], turbolisp.string_stream.init(Str, Start, End), Result).

%-----------------------------------------------------------------------------%
