:- module turbolisp.runtime.builtin.comparison.

%=============================================================================%
% Turbolisp implementation details for comparison builtins.
:- interface.
%=============================================================================%

:- func inverse(cmp_result) = cmp_result.
:- mode inverse(di) = (uo) is det.
:- mode inverse(in) = (out) is det.

%-----------------------------------------------------------------------------%

:- pred builtin_comparison_bind(cmp_pred, list(element), result, runtime, runtime).
:- mode builtin_comparison_bind(cmp_pred, in, res_uo, in, out) is det.

%-----------------------------------------------------------------------------%

:- pred atom_compare(string::in, string::in, comparison_result::uo) is det.

%=============================================================================%
:- implementation.
%=============================================================================%

:- use_module exception.

%-----------------------------------------------------------------------------%

:- pragma inline(builtin_comparison_bind/5).
:- pragma inline(inverse/1).

%-----------------------------------------------------------------------------%

inverse(error(Error)) = error(Error).
inverse(yes) = no.
inverse(no) = yes.

%-----------------------------------------------------------------------------%

builtin_comparison_bind(Pred, Args, Result, !Runtime) :-
    ( if
        Args = [A, B, Y, N]
    then
        Pred(A, B, CmpResult),
        (
            CmpResult = yes,
            Result = maybe.ok(Y)
        ;
            CmpResult = no,
            Result = maybe.ok(N)
        ;
            CmpResult = error(Error),
            Result = maybe.error(Error)
        )
    else
        exception.throw(exception.software_error(
            "Wrong arity in comparison func (builtin_bind is probably broken)"))
    ).

%-----------------------------------------------------------------------------%

atom_compare(A, B, Cmp) :-
    ( if
        number_type(A, ANum),
        number_type(B, BNum)
    then
        ( if
            as_int(ANum, BNum, AInt, BInt)
        then
            builtin.compare(Cmp, AInt, BInt)
        else
            promote(ANum, BNum, AFloat, BFloat),
            builtin.compare(Cmp, AFloat, BFloat)
        )
    else
        builtin.compare(Cmp, A, B)
    ).
