% Copyright (c) 2019  AlaskanEmily, Transnat Games
%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.

:- module turbolisp.

%=============================================================================%
% Main module for TurboLisp. Includes wrappers for the S-expression parser.
:- interface.
%=============================================================================%

:- use_module io.
:- import_module list.
:- use_module maybe.

:- import_module turbo_s.

%-----------------------------------------------------------------------------%
% Parses using the current text_input_stream
:- pred parse(io.res(list.list(element)), io.io, io.io).
:- mode parse(out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- pred parse(io.text_input_stream, io.res(list.list(element)), io.io, io.io).
:- mode parse(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- pred parse_string(string, maybe.maybe_error(list.list(element))).
:- mode parse_string(in, out) is det.

%-----------------------------------------------------------------------------%
% parse_string_between(Str, Start, End, Result)
:- pred parse_string_between(string, int, int, maybe.maybe_error(list.list(element))).
:- mode parse_string_between(in, in, in, out) is det.

%=============================================================================%
:- implementation.
%=============================================================================%

:- use_module stream.

:- use_module turbo_s.parser.
:- use_module turbo_s.string_stream.

:- include_module turbolisp.runtime.

%-----------------------------------------------------------------------------%

:- pred parse(io.text_input_stream,
    list.list(element),
    io.res(list.list(element)),
    io.io, io.io).
:- mode parse(in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

parse(Result, !IO) :-
    io.input_stream(Stream, !IO),
    parse(Stream, Result, !IO).

%-----------------------------------------------------------------------------%

parse(Stream, Result, !IO) :-
    turbo_s.parse(Stream, [], Result, !IO).

%-----------------------------------------------------------------------------%

parse(Stream, List, Result, !IO) :-
    turbo_s.parse(Stream, List, Result, !IO).

%-----------------------------------------------------------------------------%

parse_string(Str, Result) :-
    turbo_s.parse_string_stream(Str, [], turbo_s.string_stream.init(Str), Result).

%-----------------------------------------------------------------------------%

parse_string_between(Str, Start, End, Result) :-
    Stream = turbo_s.string_stream.init(Str, Start, End),
    turbo_s.parse_string_stream(Str, [], Stream, Result).

%-----------------------------------------------------------------------------%
