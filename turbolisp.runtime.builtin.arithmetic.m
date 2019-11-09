% Copyright (c) 2019  AlaskanEmily, Transnat Games
%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.

:- module turbolisp.runtime.builtin.arithmetic.

%=============================================================================%
% TurboLisp implementation details for arithmetic builtins.
:- interface.
%=============================================================================%

:- func int_to_float(int::in) = (float::uo) is det.

%-----------------------------------------------------------------------------%

:- func inc(int) = int.

%-----------------------------------------------------------------------------%

:- type math_pred == (pred(number, number, string)).
:- mode math_pred == (pred(in, in, uo) is det).
:- inst math_pred == (pred(in, in, uo) is det).

%-----------------------------------------------------------------------------%
% TODO: Variadic + and *?
:- pred builtin_plus `with_type` math_pred `with_inst` math_pred.
:- pred builtin_minus `with_type` math_pred `with_inst` math_pred.
:- pred builtin_times `with_type` math_pred `with_inst` math_pred.
:- pred builtin_divide `with_type` math_pred `with_inst` math_pred.

%-----------------------------------------------------------------------------%

:- pred builtin_arithmetic_bind(math_pred, arithmetic, list(element), result).
:- mode builtin_arithmetic_bind(math_pred, in, in, res_uo) is det.

%=============================================================================%
% Most of the implementation of the arithmetic submodule is private.
:- implementation.
%=============================================================================%

:- use_module int.
:- import_module float.

%-----------------------------------------------------------------------------%

:- pragma inline(int_to_float/1).

%-----------------------------------------------------------------------------%

int_to_float(I) = ((0.0)+float(I)).

%-----------------------------------------------------------------------------%

inc(I) = int.plus(I, 1).

%-----------------------------------------------------------------------------%

:- func float_plus(float::in, float::in) = (float::uo) is det.
:- func float_minus(float::in, float::in) = (float::uo) is det.
:- func float_times(float::in, float::in) = (float::uo) is det.
:- func float_divide(float::in, float::in) = (float::uo) is det.

float_plus(A, B) = (A+B).
float_minus(A, B) = (A-B).
float_times(A, B) = (A*B).
float_divide(A, B) = (A/B).

%-----------------------------------------------------------------------------%

:- pred two_atoms(element, element, maybe.maybe_error({string, string})).
:- mode two_atoms(in, in, out(maybe_unique_error)) is det.

two_atoms(list(_), list(_), maybe.error("Args 1 and 2 not atoms")).
two_atoms(atom(_), list(_), maybe.error("Arg 2 not atom")).
two_atoms(list(_), atom(_), maybe.error("Arg 1 not atom")).
two_atoms(atom(A), atom(B), maybe.ok({A, B})).

:- pragma inline(two_atoms/3).

%-----------------------------------------------------------------------------%

:- pred two_atoms(list.list(element), maybe.maybe_error({string, string})).
:- mode two_atoms(in, out(maybe_unique_error)) is det.

:- pragma inline(two_atoms/2).

two_atoms(Args, Result) :-
    (
        Args = [],
        Result = maybe.error("no values")
    ;
        Args = [_|[]],
        Result = maybe.error("not 2 values (1)")
    ;
        Args = [_|[_|[_|_]]],
        Result = maybe.error(string.append(string.append(
            "not 2 values (", string.from_int(list.length(Args))),
            ")"))
    ;
        Args = [A|[B|[]]],
        two_atoms(A, B, Result)
    ).

%-----------------------------------------------------------------------------%

:- func arithmetic(func(int, int) = int, func(float, float) = float,
    number, number) = (string).
:- mode arithmetic(func(in, in) = (out) is det, func(in, in) = (uo) is det,
    in, in) = (uo) is det.

arithmetic(Func, _, int(A), int(B)) = string.from_int(Func(A, B)).
arithmetic(_, Func, float(A), float(B)) = string.from_float(Func(A, B)).
arithmetic(_, Func, int(A), float(B)) = string.from_float(Func(float(A), B)).
arithmetic(_, Func, float(A), int(B)) = string.from_float(Func(A, float(B))).

%-----------------------------------------------------------------------------%

builtin_plus(ANum, BNum, arithmetic(int.plus, float_plus, ANum, BNum)).
builtin_minus(ANum, BNum, arithmetic(int.minus, float_minus, ANum, BNum)).
builtin_times(ANum, BNum, arithmetic(int.times, float_times, ANum, BNum)).
builtin_divide(ANum, BNum, arithmetic('int__div', float_divide, ANum, BNum)).

%-----------------------------------------------------------------------------%
% Implementation of arithmetic operators.
:- pred arithmetic(math_pred,
    arithmetic, list.list(element), result).
:- mode arithmetic(math_pred,
    in, in, res_uo) is det.

:- pragma inline(arithmetic/4).

arithmetic(Pred, Op, Args, Result) :-
    two_atoms(Args, ArgsResult),
    (
        ArgsResult = maybe.error(Error),
        builtin_op_tag(arithmetic(Op), Tag),
        Result = maybe.error(func_error(Tag, 2, Error))
    ;
        ArgsResult = maybe.ok({AStr, BStr}),
        ( if
            number_type(AStr, ANum)
        then
            ( if
                number_type(BStr, BNum)
            then
                Pred(ANum, BNum, Out),
                Result = maybe.ok(atom(Out))
            else
                builtin_op_tag(arithmetic(Op), Tag),
                Result = maybe.error(func_error(
                    Tag,
                    2,
                    string.append(string.append(
                        "arg 2 not a number (", BStr), ")")))
            )
        else
            builtin_op_tag(arithmetic(Op), Tag),
            Result = maybe.error(func_error(
                Tag,
                2,
                string.append(string.append(
                    "arg 1 not a number (", AStr), ")")))
        )
    ).

%-----------------------------------------------------------------------------%

builtin_arithmetic_bind(Pred, Op, Args, Out) :- arithmetic(Pred, Op, Args, Out).
