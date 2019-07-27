:- module turbo_s.string_stream.
%=============================================================================%
% Types and typeclass instances for streams using string ranges
:- interface.
%=============================================================================%

:- use_module stream.

%-----------------------------------------------------------------------------%

:- type string_range.

%-----------------------------------------------------------------------------%

:- type error ---> error(string).

%-----------------------------------------------------------------------------%

:- type tl_err == turbo_s.string_stream.error.

%-----------------------------------------------------------------------------%

:- instance stream.stream(string, string_range).

%-----------------------------------------------------------------------------%

:- instance stream.input(string, string_range).

%-----------------------------------------------------------------------------%

:- instance stream.error(tl_err).

%-----------------------------------------------------------------------------%

:- instance stream.reader(string, character, string_range, tl_err).

%-----------------------------------------------------------------------------%

:- instance stream.putback(string, character, string_range, tl_err).

%-----------------------------------------------------------------------------%

:- func init(string::in) = (string_range::uo) is det.

%-----------------------------------------------------------------------------%

:- func init(string::in, int::in, int::in) = (string_range::uo) is det.

%=============================================================================%
:- implementation.
%=============================================================================%

:- use_module string.
:- use_module exception.
:- use_module stack.
:- use_module maybe.
:- import_module int.

%-----------------------------------------------------------------------------%

:- func unique_char(character::in) = (character::uo) is det.
unique_char(In) = Out :-
    string.unsafe_index(string.duplicate_char(In, 1), 0, Out).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    unique_char(In::in) = (Out::uo),
    [may_call_mercury, will_not_throw_exception, promise_pure, thread_safe,
    does_not_terminate, does_not_affect_liveness, may_duplicate],
    "Out = In;").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C#",
    unique_char(In::in) = (Out::uo),
    [may_call_mercury, will_not_throw_exception, promise_pure, thread_safe,
    does_not_terminate, does_not_affect_liveness, may_duplicate],
    "Out = In;").
    
%-----------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    unique_char(In::in) = (Out::uo),
    [may_call_mercury, will_not_throw_exception, promise_pure, thread_safe,
    does_not_terminate, does_not_affect_liveness, may_duplicate],
    "Out = In;").

%-----------------------------------------------------------------------------%

:- type string_range --->
    string_range(index::int, length::int) ;
    string_range(int, int, character).

%-----------------------------------------------------------------------------%

:- instance stream.stream(string, string_range) where [
    name(_, "<string>", !State)
].

%-----------------------------------------------------------------------------%

:- instance stream.input(string, string_range) where [].

%-----------------------------------------------------------------------------%

:- instance stream.error(tl_err) where [
    error_message(error(E)) = E
].

%-----------------------------------------------------------------------------%

:- instance stream.reader(string, character, string_range, tl_err) where [
    (get(Str, Result, string_range(Index, Length), SOut) :-
        ( if
            Index > Length
        then
            exception.throw(exception.software_error("Index is out range"))
        else if
            Index = Length
        then
            SOut = string_range(Index + 0, Length + 0),
            Result = stream.eof
        else
            SOut = string_range(Index + 1, Length + 0),
            Result = stream.ok(string.unsafe_index(Str, Index))
        )
    ),
    get(_, stream.ok(Char), string_range(I, L, Char), string_range(I, L))
].

%-----------------------------------------------------------------------------%

:- instance stream.putback(string, character, string_range, tl_err) where [
    (unget(_, Char, Range, string_range(I, L, unique_char(Char))) :-
        (
            string_range(I, L) = Range
        ;
            string_range(I, L, _) = Range
        )
    )
].

%-----------------------------------------------------------------------------%

init(Str) = init(Str, 0, string.length(Str)+0).

%-----------------------------------------------------------------------------%

init(Str, Index, Length) = Range :-
    ( if
        string.length(Str) < Length
    then
        exception.throw(exception.software_error("Length is out range"))
    else
        Range = string_range(Index + 0, Length + 0)
    ).
