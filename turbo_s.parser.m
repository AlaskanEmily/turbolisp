% Any copyright is dedicated to the Public Domain.
% http://creativecommons.org/publicdomain/zero/1.0/

:- module turbo_s.parser.

%=============================================================================%
% Parser core for TurboLisps's S-expression parser.
:- interface.
%=============================================================================%

:- import_module list.
:- use_module stream.

:- type errors(E) ---> errors(
    eof::E, % Used when an EOF is found inside a list.
    unmatched_paren::E). % Used when a list close is found when not in a list.

%-----------------------------------------------------------------------------%
% parse(Stream, Errors, Result, !State)
% Parses an element from the stream.
% Result is EOF if an unexpected EOF is encountered in a list.
% An EOF in an atom simply ends the atom.
:- pred parse(Stream, errors(E), stream.result(element, E), S, S)
    <= (stream.reader(Stream, character, S, E),
    stream.putback(Stream, character, S, E)).
:- mode parse(in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
% parse_list(Stream, Errors, PartialList, Result, !State)
% Parses a list from the stream. The start of the list is PartialList.
% Result is EOF if an unexpected EOF is encountered in a list.
% The PartialList list is reversed (each call uses cons on the existing list),
% and it is reversed when the end of the list is returned (Result is in the
% right order).
:- pred parse_list(Stream, errors(E), list.list(element), stream.res(element, E), S, S)
    <= (stream.reader(Stream, character, S, E),
    stream.putback(Stream, character, S, E)).
:- mode parse_list(in, in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
% parse_atom(Stream, PartialAtom, Result, !State)
% Parses an atom from the stream.
% The PartialAtom list is reversed (each call uses cons on the existing list),
% and it is reversed when the end of the atom is returned (Result is in the
% right order).
:- pred parse_atom(Stream, list.list(character), stream.res(string, E), S, S)
    <= (stream.reader(Stream, character, S, E),
    stream.putback(Stream, character, S, E)).
:- mode parse_atom(in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
% parse_string(Stream, PartialString, Result, !State)
% Parses a string from the stream.
% A string goes from a " to another ". Any \", \v, \t, \n, or \r is escaped.
% Regardless, \t and \n are both allowed as literals in the string.
% The PartialString list is reversed (each call uses cons on the existing
% list), and it is reversed when the end of the string is returned (Result is
% in the right order).
:- pred parse_string(Stream, list.list(character), stream.res(string, E), S, S)
    <= (stream.reader(Stream, character, S, E),
    stream.putback(Stream, character, S, E)).
:- mode parse_string(in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
% parse_comment(Stream, Result, !State)
% Skips until a newline, or until eof.
% Result is ok on \n, eof on eof
:- pred parse_comment(Stream, stream.result(E), S, S)
    <= stream.reader(Stream, character, S, E).
:- mode parse_comment(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
% parse_term(Stream, Errors, Char, Result, !State)
% Parses a term starting with Char.
% Result is EOF if an unexpected EOF is encountered in a list.
% An EOF in an atom simply ends the atom.
:- pred parse_term(Stream, errors(E), character, stream.result(element, E), S, S)
    <= (stream.reader(Stream, character, S, E),
    stream.putback(Stream, character, S, E)).
:- mode parse_term(in, in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
% parse_term(Stream, Errors, Char, Result, !State)
% Parses a term.
% Result is EOF if an unexpected EOF is encountered in a list.
% An EOF in an atom simply ends the atom.
:- pred parse_term(Stream, errors(E), stream.result(element, E), S, S)
    <= (stream.reader(Stream, character, S, E),
    stream.putback(Stream, character, S, E)).
:- mode parse_term(in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
% Unifies if the input is whitespace.
:- pred is_whitespace(character::in) is semidet.

%=============================================================================%
:- implementation.
%=============================================================================%

:- use_module string.

is_whitespace(' ').
is_whitespace('\t').
is_whitespace('\v').
is_whitespace('\n').
is_whitespace('\r').

%-----------------------------------------------------------------------------%

parse(Stream, Errors, Result, !State) :- parse_term(Stream, Errors, Result, !State).

%-----------------------------------------------------------------------------%

parse_list(Stream, Errors, InList, Result, !State) :-
    stream.get(Stream, CharResult, !State),
    (
        CharResult = stream.eof,
        Result = stream.error(Errors ^ eof)
    ;
        CharResult = stream.error(E),
        Result = stream.error(E)
    ;
        CharResult = stream.ok(Char),
        ( if
            Char = (')')
        then
            % List is closed.
            Result = stream.ok(list(list.reverse(InList)))
        else if
            Char = (';')
        then
            % Skip the comment
            parse_comment(Stream, CommentResult, !State),
            (
                CommentResult = stream.eof,
                Result = stream.error(Errors ^ eof) 
            ;
                CommentResult = stream.error(E),
                Result = stream.error(E)
            ;
                % Parse more of the list.
                CommentResult = stream.ok,
                parse_list(Stream, Errors, InList, Result, !State)
            )
        else if
            is_whitespace(Char)
        then
            % Skip whitespace
            parse_list(Stream, Errors, InList, Result, !State)
        else if
            Char = ('(')
        then
            % Begin a sublist
            parse_list(Stream, Errors, [], ListResult, !State),
            (
                ListResult = stream.error(E),
                Result = stream.error(E)
            ;
                ListResult = stream.ok(SubList),
                % Add the sublist as an element to the list we are parsing
                parse_list(Stream, Errors, [SubList|InList], Result, !State)
            )
        else
            ( if
                Char = ('"')
            then
                % Parse a string literal
                parse_string(Stream, [], AtomResult, !State)
            else
                % Everything else is an atom.
                parse_atom(Stream, [Char|[]], AtomResult, !State)
            ),
            (
                AtomResult = stream.error(E),
                Result = stream.error(E)
            ;
                AtomResult = stream.ok(Atom),
                % Add the atom as an element to the list we are parsing
                parse_list(Stream, Errors, [atom(Atom)|InList], Result, !State)
            )
        )
    ).

%-----------------------------------------------------------------------------%

parse_atom(Stream, Chars, Result, !State) :-
    stream.get(Stream, CharResult, !State),
    (
        CharResult = stream.eof,
        Result = stream.ok(string.from_rev_char_list(Chars))
    ;
        CharResult = stream.error(E),
        Result = stream.error(E)
    ;
        CharResult = stream.ok(Char),
        % If the char is a comment or list component, end the atom and put the
        % char back. This char will be handled by parse_term. For a list
        ( if
            Char = ('(') ; Char = (')') ; Char = (';') ; is_whitespace(Char)
        then
            stream.unget(Stream, Char, !State),
            Result = stream.ok(string.from_rev_char_list(Chars))
        else
            parse_atom(Stream, [Char|Chars], Result, !State)
        )
    ).

%-----------------------------------------------------------------------------%

parse_string(Stream, Chars, Result, !State) :-
    stream.get(Stream, CharResult, !State),
    (
        CharResult = stream.eof,
        Result = stream.ok(string.from_rev_char_list(Chars))
    ;
        CharResult = stream.error(E),
        Result = stream.error(E)
    ;
        CharResult = stream.ok(Char),
        % If the char is a comment or list component, end the atom and put the
        % char back. This char will be handled by parse_term. For a list
        ( if
            Char = ('"')
        then
            Result = stream.ok(string.from_rev_char_list(Chars))
        else if
            Char = ('\\')
        then
            stream.get(Stream, EscapeResult, !State),
            (
                EscapeResult = stream.eof,
                Result = stream.ok(string.from_rev_char_list(Chars))
            ;
                EscapeResult = stream.error(E),
                Result = stream.error(E)
            ;
                EscapeResult = stream.ok(EscapedChar),
                ( EscapedChar = ('n') -> C = ('\n')
                ; EscapedChar = ('t') -> C = ('\t')
                ; EscapedChar = ('r') -> C = ('\r')
                ; EscapedChar = ('v') -> C = ('\v')
                ; EscapedChar = C
                ),
                parse_string(Stream, [C|Chars], Result, !State)
            )
        else
            parse_string(Stream, [Char|Chars], Result, !State)
        )
    ).

%-----------------------------------------------------------------------------%

parse_comment(Stream, Result, !State) :-
    stream.get(Stream, CharResult, !State),
    (
        CharResult = stream.eof,
        Result = stream.eof
    ;
        CharResult = stream.error(E),
        Result = stream.error(E)
    ;
        CharResult = stream.ok(Char),
        ( if
            Char = ('\n')
        then
            Result = stream.ok
        else
            parse_comment(Stream, Result, !State)
        )
    ).

%-----------------------------------------------------------------------------%

parse_term(Stream, Errors, Result, !State) :-
    stream.get(Stream, CharResult, !State),
    (
        CharResult = stream.eof,
        Result = stream.eof
    ;
        CharResult = stream.error(E),
        Result = stream.error(E)
    ;
        CharResult = stream.ok(Char),
        parse_term(Stream, Errors, Char, Result, !State)
    ).

%-----------------------------------------------------------------------------%

parse_term(Stream, Errors, Char, Result, !State) :-
    ( if
        Char = (')')
    then
        Result = stream.error(Errors ^ unmatched_paren)
    else if
        Char = (';')
    then
        % Comment
        parse_comment(Stream, CommentResult, !State),
        (
            CommentResult = stream.eof,
            Result = stream.eof
        ;
            CommentResult = stream.error(E),
            Result = stream.error(E)
        ;
            CommentResult = stream.ok,
            parse_term(Stream, Errors, Result, !State)
        )
    else if
        is_whitespace(Char) 
    then
        % Skip whitespace.
        parse_term(Stream, Errors, Result, !State)
    else 
        ( if
            Char = ('(')
        then
            % List
            parse_list(Stream, Errors, [], ParseResult, !State),
            (
                ParseResult = stream.ok(Element),
                Result = stream.ok(Element)
            ;
                ParseResult = stream.error(E),
                Result = stream.error(E)
            )
        else
            % Atom
            parse_atom(Stream, [Char|[]], AtomResult, !State),
            (
                AtomResult = stream.ok(Atom),
                Result = stream.ok(atom(Atom))
            ;
                AtomResult = stream.error(E),
                Result = stream.error(E)
            )
        )
    ).
