% Copyright (c) 2019  AlaskanEmily, Transnat Games
%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.

:- module turbolisp.runtime.builtin.

%=============================================================================%
% TurboLisp builtins
:- interface.
%=============================================================================%

:- use_module enum.

%-----------------------------------------------------------------------------%

:- type arithmetic --->
    plus ;
    minus ;
    times ;
    divide.

%-----------------------------------------------------------------------------%

:- instance enum.enum(arithmetic).

%-----------------------------------------------------------------------------%

:- type logic --->
    int_and ;
    int_or ;
    int_xor.

%-----------------------------------------------------------------------------%

:- instance enum.enum(logic).

%-----------------------------------------------------------------------------%

:- type comparison --->
    eq ;
    ne ;
    lt ;
    gt ;
    le ;
    ge.

%-----------------------------------------------------------------------------%

:- instance enum.enum(comparison).

%-----------------------------------------------------------------------------%

:- pred comparison_tag(comparison, string).
:- mode comparison_tag(in, out) is det.
:- mode comparison_tag(out, in) is semidet.
:- mode comparison_tag(out, ui) is semidet. % Iffy
:- mode comparison_tag(in, in) is semidet. % Implied

%-----------------------------------------------------------------------------%

:- type define --->
    def ;
    let ;
    fn.

%-----------------------------------------------------------------------------%

:- instance enum.enum(define).

%-----------------------------------------------------------------------------%

:- type builtin_op --->
    arithmetic(arithmetic) ;
    logic(logic) ;
    comparison(comparison) ;
    define(define).

%-----------------------------------------------------------------------------%

:- instance enum.enum(builtin_op).

%-----------------------------------------------------------------------------%

:- pred builtin_op_enum(builtin_op, int).
:- mode builtin_op_enum(in, out) is det.
:- mode builtin_op_enum(out, in) is semidet.
:- mode builtin_op_enum(in, in) is semidet. % Implied

%-----------------------------------------------------------------------------%

:- pred builtin_op_tag(builtin_op, string).
:- mode builtin_op_tag(in, out) is det.
:- mode builtin_op_tag(out, in) is semidet.
:- mode builtin_op_tag(out, ui) is semidet. % Iffy
:- mode builtin_op_tag(in, in) is semidet. % Implied

%-----------------------------------------------------------------------------%
% Numeric components shared between comparison, arithmetic, and logic.
%-----------------------------------------------------------------------------%

% Used to determine if a number is a float or an int.
:- type number ---> float(float) ; int(int).

%-----------------------------------------------------------------------------%
% Promotes both numbers to floats.
:- pred promote(number::in, number::in, float::out, float::out) is det.

%-----------------------------------------------------------------------------%
% Unifies iff both numbers are the integer functor for the ints
:- pred as_int(number, number, int, int).
:- mode as_int(in, in, out, out) is semidet.
:- mode as_int(di, di, uo, uo) is semidet.
:- mode as_int(out, out, in, in) is det.
:- mode as_int(uo, uo, di, di) is det.
:- mode as_int(in, in, in, in) is semidet. % Implied.

%-----------------------------------------------------------------------------%
% number_type(NumStr, Num)
:- pred number_type(string::in, number::uo) is semidet.

%-----------------------------------------------------------------------------%
% Comparison components.
%-----------------------------------------------------------------------------%

% Result of the builtin comparisons.
:- type cmp_result ---> yes ; no ; error(string).

%-----------------------------------------------------------------------------%
% Runs the builtin comparison predicate.
:- pred comparison(comparison, element, element, cmp_result).
:- mode comparison(in, in, in, uo) is det.

%-----------------------------------------------------------------------------%

:- type cmp_pred == (pred(element, element, cmp_result)).
:- mode cmp_pred == (pred(in, in, uo) is det).
:- inst cmp_pred == (pred(in, in, uo) is det).

%-----------------------------------------------------------------------------%
% Comparison builtins. These are aggressively inlined by the compilation
% phase, so they must be exported to the runtime.
:- pred builtin_eq `with_type` cmp_pred `with_inst` cmp_pred.
:- pred builtin_ne `with_type` cmp_pred `with_inst` cmp_pred.
:- pred builtin_lt `with_type` cmp_pred `with_inst` cmp_pred.
:- pred builtin_gt `with_type` cmp_pred `with_inst` cmp_pred.
:- pred builtin_le `with_type` cmp_pred `with_inst` cmp_pred.
:- pred builtin_ge `with_type` cmp_pred `with_inst` cmp_pred.

%-----------------------------------------------------------------------------%

:- pred builtin_eq_bind `with_type` execute_pred `with_inst` execute_pred.
:- pred builtin_ne_bind `with_type` execute_pred `with_inst` execute_pred.
:- pred builtin_lt_bind `with_type` execute_pred `with_inst` execute_pred.
:- pred builtin_gt_bind `with_type` execute_pred `with_inst` execute_pred.
:- pred builtin_le_bind `with_type` execute_pred `with_inst` execute_pred.
:- pred builtin_ge_bind `with_type` execute_pred `with_inst` execute_pred.
    
%-----------------------------------------------------------------------------%
% Arithmetic components.
%-----------------------------------------------------------------------------%

:- pred builtin_plus_bind `with_type` execute_pred `with_inst` execute_pred.
:- pred builtin_minus_bind `with_type` execute_pred `with_inst` execute_pred.
:- pred builtin_times_bind `with_type` execute_pred `with_inst` execute_pred.
:- pred builtin_divide_bind `with_type` execute_pred `with_inst` execute_pred.
    
%-----------------------------------------------------------------------------%
% Define components.
%-----------------------------------------------------------------------------%

%:- pred builtin_let_bind `with_type` execute_pred `with_inst` execute_pred.
%:- pred builtin_def_bind `with_type` execute_pred `with_inst` execute_pred.
:- pred builtin_fn_bind `with_type` execute_pred `with_inst` execute_pred.

%=============================================================================%
:- implementation.
%=============================================================================%

:- use_module char.
:- use_module exception.

:- include_module turbolisp.runtime.builtin.comparison.
:- import_module turbolisp.runtime.builtin.comparison.

:- include_module turbolisp.runtime.builtin.arithmetic.
:- import_module turbolisp.runtime.builtin.arithmetic.

%-----------------------------------------------------------------------------%

:- pragma inline(comparison/4).
:- pragma inline(builtin_op_tag/2).
:- pragma inline(builtin_op_enum/2).

%-----------------------------------------------------------------------------%

:- pragma inline(comparison/4).
:- pragma inline(builtin_eq/3).
:- pragma inline(builtin_lt/3).
:- pragma inline(builtin_gt/3).
:- pragma inline(builtin_le/3).
:- pragma inline(builtin_ge/3).

%-----------------------------------------------------------------------------%

:- instance enum.enum(arithmetic) where [
    ( to_int(E) = I :- builtin_op_enum(arithmetic(E), I) ),
    ( from_int(I) = E :- builtin_op_enum(arithmetic(E), I) )
].

%-----------------------------------------------------------------------------%

:- instance enum.enum(logic) where [
    ( to_int(E) = I :- builtin_op_enum(logic(E), I) ),
    ( from_int(I) = E :- builtin_op_enum(logic(E), I) )
].

%-----------------------------------------------------------------------------%

:- instance enum.enum(comparison) where [
    ( to_int(E) = I :- builtin_op_enum(comparison(E), I) ),
    ( from_int(I) = E :- builtin_op_enum(comparison(E), I) )
].

%-----------------------------------------------------------------------------%

comparison_tag(Cmp, Tag) :-
    builtin_op_tag(comparison(Cmp), Tag).

%-----------------------------------------------------------------------------%

:- instance enum.enum(define) where [
    ( to_int(E) = I :- builtin_op_enum(define(E), I) ),
    ( from_int(I) = E :- builtin_op_enum(define(E), I) )
].

%-----------------------------------------------------------------------------%

:- instance enum.enum(builtin_op) where [
    ( to_int(E) = I :- builtin_op_enum(E, I) ),
    ( from_int(I) = E :- builtin_op_enum(E, I) )
].

%-----------------------------------------------------------------------------%

builtin_op_enum(arithmetic(plus), 0).
builtin_op_enum(arithmetic(minus), 1).
builtin_op_enum(arithmetic(times), 2).
builtin_op_enum(arithmetic(divide), 3).
builtin_op_enum(logic(int_and), 4).
builtin_op_enum(logic(int_or), 5).
builtin_op_enum(logic(int_xor), 6).
builtin_op_enum(comparison(eq), 7).
builtin_op_enum(comparison(ne), 8).
builtin_op_enum(comparison(lt), 9).
builtin_op_enum(comparison(gt), 10).
builtin_op_enum(comparison(le), 11).
builtin_op_enum(comparison(ge), 12).
builtin_op_enum(define(def), 13).
builtin_op_enum(define(let), 14).
builtin_op_enum(define(fn), 15).

%-----------------------------------------------------------------------------%

builtin_op_tag(arithmetic(plus), "+").
builtin_op_tag(arithmetic(minus), "-").
builtin_op_tag(arithmetic(times), "*").
builtin_op_tag(arithmetic(divide), "/").
builtin_op_tag(logic(int_and), "&").
builtin_op_tag(logic(int_or), "|").
builtin_op_tag(logic(int_xor), "^").
builtin_op_tag(comparison(eq), "=").
builtin_op_tag(comparison(ne), "!").
builtin_op_tag(comparison(lt), "<").
builtin_op_tag(comparison(gt), ">").
builtin_op_tag(comparison(le), "<=").
builtin_op_tag(comparison(ge), ">=").
builtin_op_tag(define(def), "def").
builtin_op_tag(define(let), "let").
builtin_op_tag(define(fn), "fn").

%-----------------------------------------------------------------------------%

comparison(eq, E1, E2, Result) :- builtin_eq(E1, E2, Result).
comparison(ne, E1, E2, Result) :- builtin_ne(E1, E2, Result).
comparison(lt, E1, E2, Result) :- builtin_lt(E1, E2, Result).
comparison(gt, E1, E2, Result) :- builtin_gt(E1, E2, Result).
comparison(le, E1, E2, Result) :- builtin_le(E1, E2, Result).
comparison(ge, E1, E2, Result) :- builtin_ge(E1, E2, Result).

%-----------------------------------------------------------------------------%

promote(float(A), float(B), A, B).
promote(int(A), float(B), int_to_float(A), B).
promote(float(A), int(B), A, int_to_float(B)).
promote(int(A), int(B), int_to_float(A), int_to_float(B)).

%-----------------------------------------------------------------------------%

as_int(int(A), int(B), A, B).

%-----------------------------------------------------------------------------%

:- pred digit_or_dot(character::in) is semidet.
digit_or_dot(C) :-
    ( not C = ('.') ) => char.is_digit(C).

%-----------------------------------------------------------------------------%

number_type(In, Out) :-
    ( if
        string.all_match(char.is_digit, In)
    then
        string.to_int(In, Int),
        builtin__copy(Int, UniqInt),
        Out = int(UniqInt)
    else if
        string.all_match(digit_or_dot, In)
    then
        string.to_float(In, Float),
        builtin__copy(Float, UniqFloat),
        Out = float(UniqFloat)
    else
        string.remove_prefix("0x", In, InP),
        string.all_match(char.is_hex_digit, InP),
        string.base_string_to_int(16, InP, Int),
        builtin__copy(Int, UniqInt),
        Out = int(UniqInt)
    ).

%-----------------------------------------------------------------------------%

builtin_eq(A, B, Result) :- ( A = B -> Result = yes ; Result = no ).

builtin_ne(A, B, Result) :- ( A = B -> Result = no ; Result = yes ).

builtin_lt(list(_), list(_), error("Error: `lt/2` -> test two lists")).
builtin_lt(atom(_), list(_), error("Error: `lt/2` -> test atom and list")).
builtin_lt(list(_), atom(_), error("Error: `lt/2` -> test list and atom")).
builtin_lt(atom(A), atom(B), Result) :-
    ( atom_compare(A, B, (<)) -> Result = yes ; Result = no ).

builtin_gt(list(_), list(_), error("Error: `gt/2` -> test two lists")).
builtin_gt(atom(_), list(_), error("Error: `gt/2` -> test atom and list")).
builtin_gt(list(_), atom(_), error("Error: `gt/2` -> test list and atom")).
builtin_gt(atom(A), atom(B), Result) :-
    ( atom_compare(A, B, (>)) -> Result = yes ; Result = no ).

builtin_le(list(_), list(_), error("Error: `le/2` -> test two lists")).
builtin_le(atom(_), list(_), error("Error: `le/2` -> test atom and list")).
builtin_le(list(_), atom(_), error("Error: `le/2` -> test list and atom")).
builtin_le(atom(A), atom(B), Result) :-
    ( atom_compare(A, B, (>)) -> Result = no ; Result = yes ).

builtin_ge(list(_), list(_), error("Error: `ge/2` -> test two lists")).
builtin_ge(atom(_), list(_), error("Error: `ge/2` -> test atom and list")).
builtin_ge(list(_), atom(_), error("Error: `ge/2` -> test list and atom")).
builtin_ge(atom(A), atom(B), Result) :-
    ( atom_compare(A, B, (<)) -> Result = no ; Result = yes ).

%-----------------------------------------------------------------------------%

builtin_eq_bind(E, R, !RT) :- builtin_comparison_bind(builtin_eq, E, R, !RT).
builtin_ne_bind(E, R, !RT) :- builtin_comparison_bind(builtin_ne, E, R, !RT).
builtin_lt_bind(E, R, !RT) :- builtin_comparison_bind(builtin_lt, E, R, !RT).
builtin_gt_bind(E, R, !RT) :- builtin_comparison_bind(builtin_gt, E, R, !RT).
builtin_le_bind(E, R, !RT) :- builtin_comparison_bind(builtin_le, E, R, !RT).
builtin_ge_bind(E, R, !RT) :- builtin_comparison_bind(builtin_ge, E, R, !RT).

%-----------------------------------------------------------------------------%

builtin_plus_bind(E, R, !RT) :- builtin_arithmetic_bind(builtin_plus, plus, E, R).
builtin_minus_bind(E, R, !RT) :- builtin_arithmetic_bind(builtin_minus, minus, E, R).
builtin_times_bind(E, R, !RT) :- builtin_arithmetic_bind(builtin_times, times, E, R).
builtin_divide_bind(E, R, !RT) :- builtin_arithmetic_bind(builtin_divide, divide, E, R).

%-----------------------------------------------------------------------------%
% Used to implement let and def

%-----------------------------------------------------------------------------%
% Used to parse argument names in fn/3
:- pred fn_arg(element, string, int, int, maybe.maybe_error, maybe.maybe_error).
:- mode fn_arg(in, out, in, out, di, uo) is det.
%:- mode fn_arg(di, uo, in, out, di, uo) is det.

fn_arg(_, "", I, inc(I), maybe.error(E), maybe.error(E)).
fn_arg(list(_), "", I, inc(I), maybe.ok,
    maybe.error(string.append(string.append(
        "Error: `fn/3` -> Arg list element ",
        string.from_int(I)),
        " is a list"))).
fn_arg(atom(Str), Str, I, inc(I), maybe.ok, maybe.ok).

%-----------------------------------------------------------------------------%
% Used to implement fn
:- type fn_parse_result == {string, list(string), list(element), int}.
:- pred fn_parse(list.list(element), maybe.maybe_error(fn_parse_result)).
:- mode fn_parse(in, res_uo) is det.

fn_parse(Element, Result) :-
    ( if
        Element = [NameElement|[ArgsElement|Body]]
    then
        (
            NameElement = list(_),
            Result = maybe.error("Error: `fn/3` -> arg 1 is a list")
        ;
            NameElement = atom(Name),
            (
                ArgsElement = atom(_),
                Result = maybe.error("Error: `fn/3` -> arg 2 is an atom")
            ;
                ArgsElement = list(Args),
                % Validate the arguments, and construct a list of names.
                list.map_foldl2(fn_arg,
                    Args, ArgNames, 0, Arity, maybe.ok, ArgsResult),
                (
                    ArgsResult = maybe.ok,
                    Result = maybe.ok({Name, ArgNames, Body, Arity})
                ;
                    ArgsResult = maybe.error(Error),
                    Result = maybe.error(Error)
                )
            )
        )
    else
        exception.throw(exception.software_error(
            "Wrong arity in `fn/3` (builtin_bind is probably broken)"))
    ).

%-----------------------------------------------------------------------------%

builtin_fn_bind(Element, Result, !Runtime) :-
    fn_parse(Element, FnResult),
    (
        FnResult = maybe.error(Error),
        Result = maybe.error(Error)
    ;
        FnResult = maybe.ok({Name, ArgNames, Body, Arity}),
        def_bind(args(Name, Arity), lisp_bind(ArgNames, Body), !Runtime),
        Result = maybe.ok(atom(Name))
    ).
