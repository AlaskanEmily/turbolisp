% Any copyright is dedicated to the Public Domain.
% http://creativecommons.org/publicdomain/zero/1.0/

:- module turbo_s.

%=============================================================================%
% S-expression parser for TurboLisp
:- interface.
%=============================================================================%

:- use_module io.
:- import_module list.
:- use_module maybe.

:- include_module turbo_s.parser.
:- include_module turbo_s.string_stream.

:- use_module turbo_s.string_stream.

%-----------------------------------------------------------------------------%

:- type element ---> atom(string) ; list(list.list(element)).

%-----------------------------------------------------------------------------%

:- pred parse(io.text_input_stream, list.list(element), io.res(list.list(element)), io.io, io.io).
:- mode parse(in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- pred parse_string_stream(string,
    list.list(element),
    turbo_s.string_stream.string_range,
    maybe.maybe_error(list.list(element))).
:- mode parse_string_stream(in, in, di, out) is det.

%=============================================================================%
:- implementation.
%=============================================================================%

:- use_module stream.

:- use_module turbo_s.parser.

%-----------------------------------------------------------------------------%

:- func ss_errors = turbo_s.parser.errors(turbo_s.string_stream.error).
ss_errors = turbo_s.parser.errors(
    turbo_s.string_stream.error("Unexpected EOF"),
    turbo_s.string_stream.error("Unmatched close paren")).

%-----------------------------------------------------------------------------%

parse_string_stream(String, List, StringRangeIn, Result) :-
    turbo_s.parser.parse(String, ss_errors, ElementResult, StringRangeIn, StringRangeOut),
    (
        ElementResult = stream.ok(Element),
        parse_string_stream(String, [Element|List], StringRangeOut, Result)
    ;
        ElementResult = stream.eof,
        Result = maybe.ok(list.reverse(List))
    ;
        ElementResult = stream.error(turbo_s.string_stream.error(E)),
        Result = maybe.error(E)
    ).

%-----------------------------------------------------------------------------%

:- func io_errors = turbo_s.parser.errors(io.error).
io_errors = turbo_s.parser.errors(
    io.make_io_error("Unexpected EOF"),
    io.make_io_error("Unmatched close paren")).

%-----------------------------------------------------------------------------%

parse(Stream, List, Result, !IO) :-
    turbo_s.parser.parse(Stream, io_errors, ElementResult, !IO),
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