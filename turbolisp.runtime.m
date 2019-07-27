:- module turbolisp.runtime.

%=============================================================================%
% TurboLisp runtime components.
:- interface.
%=============================================================================%

:- import_module list.
:- use_module assoc_list.
:- use_module rbtree.
:- use_module maybe.

%-----------------------------------------------------------------------------%
% TODO!
:- func nil = element.

%-----------------------------------------------------------------------------%
% Frames use an assoc list, as they are not expected to have a lot of elements,
% and the extra allocations of a tree would quickly overwhelm the gains in
% lookup speed.
:- type frame --->
    frame(variables::assoc_list.assoc_list(string, element)).

%-----------------------------------------------------------------------------%

:- func init_frame = frame.

%-----------------------------------------------------------------------------%

:- func init_frame(assoc_list.assoc_list(string, element)) = frame.

%-----------------------------------------------------------------------------%

:- type result == maybe.maybe_error(element).

%-----------------------------------------------------------------------------%

:- inst maybe_unique_error --->
    maybe.ok(ground) ;
    maybe.error(unique).

:- inst maybe_clobbered_error --->
    maybe.ok(ground) ;
    maybe.error(clobbered).

:- mode res_uo == free >> maybe_unique_error.
:- mode res_di == maybe_unique_error >> maybe_clobbered_error.

%-----------------------------------------------------------------------------%

:- type execute_pred == (pred(list.list(element), result, runtime, runtime)).
:- inst execute_pred == (pred(in, res_uo, in, out) is det).
:- mode execute_pred == (pred(in, res_uo, in, out) is det).

:- type bind --->
    mercury_bind(pred(list.list(element)::in, result::res_uo, runtime::in, runtime::out) is det) ;
    lisp_bind(arg_names::list.list(string), body::list.list(element)).

%-----------------------------------------------------------------------------%

:- type bind_spec --->
    variadic(string) ;
    args(string, int).

%-----------------------------------------------------------------------------%

:- type variables == rbtree.rbtree(string, element).

%-----------------------------------------------------------------------------%

:- type runtime ---> runtime(
    globals::variables,
    binds::rbtree.rbtree(bind_spec, bind),
    stack_frames::list.list(frame),
    pending_io::list.list(string)).

%-----------------------------------------------------------------------------%

:- func init = runtime.

%-----------------------------------------------------------------------------%

:- pred push_stack_frame(runtime::in, runtime::out) is det.

%-----------------------------------------------------------------------------%

:- pred push_stack_frame(assoc_list.assoc_list(string, element)::in,
    runtime::in, runtime::out) is det.

%-----------------------------------------------------------------------------%

:- pred pop_stack_frame(runtime::in, runtime::out) is det.

%-----------------------------------------------------------------------------%

:- pred push_stack_frame_check(int::out, runtime::in, runtime::out) is det.

%-----------------------------------------------------------------------------%

:- pred push_stack_frame_check(assoc_list.assoc_list(string, element)::in,
    int::out, runtime::in, runtime::out) is det.

%-----------------------------------------------------------------------------%

:- pred pop_stack_frame_check(int::in, runtime::in, runtime::out) is det.

%-----------------------------------------------------------------------------%

:- pred def_var(string::in, element::in, runtime::in, runtime::out) is det.

%-----------------------------------------------------------------------------%

:- pred find_var(list.list(frame), rbtree.rbtree(string, element), string, element).
:- mode find_var(in, in, in, out) is semidet.

%-----------------------------------------------------------------------------%

:- pred builtin_bind(bind_spec::in, bind::out) is semidet.

%-----------------------------------------------------------------------------%

:- pred def_bind(bind_spec::in, bind::in, runtime::in, runtime::out) is det.

%-----------------------------------------------------------------------------%

:- pred find_bind(string, int, rbtree.rbtree(bind_spec, bind), bind).
:- mode find_bind(in, in, in, out) is semidet.

%-----------------------------------------------------------------------------%
% This is a workaround, as the Mercury compiler gets confused when disjuncting
% on functors which contain predicates as elements in the functor.
:- pred call_bind(bind, list.list(element), result, runtime, runtime).
:- mode call_bind(in, in, res_uo, in, out) is det.

%-----------------------------------------------------------------------------%

:- type run_pred1 == (pred(element, result, runtime, runtime)).
:- inst run_pred1 == (pred(in, res_uo, in, out) is det).
:- mode run_pred1 == (pred(in, res_uo, in, out) is det).

%-----------------------------------------------------------------------------%
% Same as run_pred1, but is suitable for use with list.map_foldl2
:- type run_pred2 == (pred(element, element,
    runtime, runtime,
    maybe.maybe_error, maybe.maybe_error)).
:- inst run_pred2 == (pred(in, out, in, out, di, uo) is det).
:- mode run_pred2 == (pred(in, out, in, out, di, uo) is det).

%-----------------------------------------------------------------------------%
% Same as run_pred1, but is suitable for use with list.map_foldl3 while
% counting elements in the list.
:- type run_pred3 == (pred(element, element,
    runtime, runtime,
    int, int,
    maybe.maybe_error, maybe.maybe_error)).
:- inst run_pred3 == (pred(in, out, in, out, in, out, di, uo) is det).
:- mode run_pred3 == (pred(in, out, in, out, in, out, di, uo) is det).

%-----------------------------------------------------------------------------%

:- pred reduce `with_type` run_pred1 `with_inst` run_pred1.
:- pred reduce `with_type` run_pred2 `with_inst` run_pred2.
:- pred reduce `with_type` run_pred3 `with_inst` run_pred3.

%-----------------------------------------------------------------------------%

:- pred execute `with_type` run_pred1 `with_inst` run_pred1.
:- pred execute `with_type` run_pred2 `with_inst` run_pred2.
:- pred execute `with_type` run_pred3 `with_inst` run_pred3.

%=============================================================================%
:- implementation.
%=============================================================================%

:- use_module exception.
:- use_module int.
:- use_module string.
:- use_module pair.

:- include_module turbolisp.runtime.builtin.
:- use_module turbolisp.runtime.builtin.

%-----------------------------------------------------------------------------%

nil = list([]).

%-----------------------------------------------------------------------------%
% Used for the optimized C routines.
:- pragma foreign_decl("C", "
#ifdef _MSC_VER

#define TL_YIELD_ARITY(ARITY, DST, OUT) \\
    _ltoa_s((ARITY), (OUT), 77, 10); \\
    (OUT)[76] = 0; \\
    const MR_Integer DST = strnlen_s((OUT), 77)

#else

#define TL_YIELD_ARITY(ARITY, DST, OUT) \\
    const MR_Integer DST = sprintf((OUT), ""%i"", (ARITY))

#endif

#define TL_YIELD_FUNC_NAME(NAME, NAME_LEN, ARITY, END, OUT) do { \\
    (OUT)[0] = '`'; \\
    memcpy((OUT)+1, Name, (NAME_LEN)); \\
    (OUT)[(NAME_LEN)+1] = '/'; \\
    { \\
        const MR_Integer arity_start = (NAME_LEN)+2; \\
        TL_YIELD_ARITY((ARITY), ZZ_end, (OUT) + arity_start) + arity_start; \\
        (OUT)[ZZ_end] = '`'; \\
        (END) = ZZ_end+1; \\
    } \\
    \\
}while(0)

").

%-----------------------------------------------------------------------------%

:- func yield_func_name(string::in, int::in) = (string::uo) is det.
yield_func_name(Name, Arity) = string.append(TickFuncArity, "`") :-
    string.first_char(ArityString, ('/'), string.from_int(Arity)),
    string.first_char(TickFuncName, ('`'), Name),
    string.append(TickFuncName, ArityString, TickFuncArity).

% Optimized C version.
:- pragma foreign_proc("C", yield_func_name(Name::in, Arity::in) = (Out::uo),
    [promise_pure, thread_safe, will_not_call_mercury, will_not_modify_trail,
     does_not_affect_liveness, may_duplicate],
    "
    const MR_Integer name_len = strlen(Name);
    MR_allocate_aligned_string_msg(Out, name_len + 80, MR_ALLOC_ID);
    MR_Integer end;
    TL_YIELD_FUNC_NAME(Name, name_len, Arity, end, Out);
    Out[end] = 0;
    ").

%-----------------------------------------------------------------------------%

:- func func_error(string::in, int::in, string::in) = (string::uo) is det.
func_error(Name, Arity, Error) =
    string.append(func_error_prefix(Name, Arity), Error).

%-----------------------------------------------------------------------------%

:- func func_error_prefix(string::in, int::in) = (string::uo) is det.
func_error_prefix(Name, Arity) =
    string.append(
    string.append(
        "Error ",
        yield_func_name(Name, Arity)),
        " -> ").

% Optimized C version.
:- pragma foreign_proc("C", func_error(Name::in, Arity::in, Error::in) = (Out::uo),
    [promise_pure, thread_safe, will_not_call_mercury, will_not_modify_trail,
     does_not_affect_liveness, may_duplicate],
    "
    const char head[] = {'E', 'r', 'r', 'o', 'r', ':', ' '};
    const char tail[] = {' ', '-', '>', ' '};
    const MR_Integer name_len = strlen(Name);
    const MR_Integer error_len = strlen(Error);
    MR_allocate_aligned_string_msg(Out, name_len + error_len + 90, MR_ALLOC_ID);
    MR_Integer end;
    memcpy(Out, head, sizeof(head));
    TL_YIELD_FUNC_NAME(Name, name_len, Arity, end, Out+sizeof(head));
    memcpy(Out+sizeof(head)+end, tail, sizeof(tail));
    memcpy(Out+sizeof(head)+sizeof(tail)+end, Error, error_len+1);
    ").

%-----------------------------------------------------------------------------%

:- func list_index_error(int::in, int::in) = (string::uo) is det.
list_index_error(At, Length) = Result :-
    string.append("`at` -> index of '", string.from_int(At), Err0),
    string.append(Err0, "' out of bounds for list of length '", Err1),
    string.append(Err1, string.from_int(Length), Err2),
    string.append(Err2, "'", Result).

% Optimized C version.
:- pragma foreign_proc("C", list_index_error(At::in, Length::in) = (Out::uo),
    [promise_pure, thread_safe, will_not_call_mercury, will_not_modify_trail,
     does_not_affect_liveness, may_duplicate],
    "
        MR_allocate_aligned_string_msg(Out, 160, MR_ALLOC_ID);
        snprintf(Out, 159,
            ""`at` -> index of '%i' out of bounds for list of length '%i'"",
            At, Length);
        Out[159] = 0;
    ").

%-----------------------------------------------------------------------------%

init_frame(Variables) = frame(Variables).
init_frame = init_frame([]).

%-----------------------------------------------------------------------------%

init = runtime(rbtree.init, rbtree.init, [], []).

%-----------------------------------------------------------------------------%

push_stack_frame(Variables, runtime(G, B, Frames, PIO),
    runtime(G, B, [init_frame(Variables)|Frames], PIO)).

%-----------------------------------------------------------------------------%

push_stack_frame(runtime(G, B, Frames, PIO),
    runtime(G, B, [init_frame|Frames], PIO)).

%-----------------------------------------------------------------------------%

pop_stack_frame(runtime(G, B, [_Head|Frames], PIO),
    runtime(G, B, Frames, PIO)) :-
    % trace [io(!IO)] (
    %     rbtree.keys(Head ^ variables, Keys),
    %     io.write_string("Pop losing ", !IO),
    %     io.write_int(list.length(Keys), !IO), io.nl(!IO),
    %     list.foldl(
    %         (pred(Str::in, I::di, O::uo) is semidet :-
    %             io.write_string(Str, I, M), io.nl(M, O)),
    %         Keys, !IO)
    % ),
    true.

pop_stack_frame(runtime(_, _, [], _), _) :-
    exception.throw(exception.software_error("Stack underflow")).

%-----------------------------------------------------------------------------%

push_stack_frame_check(Check, !Runtime) :-
    push_stack_frame(!Runtime),
    list.length(!.Runtime ^ stack_frames, Check).

%-----------------------------------------------------------------------------%

push_stack_frame_check(Variables, Check, !Runtime) :-
    push_stack_frame(Variables, !Runtime),
    list.length(!.Runtime ^ stack_frames, Check).

%-----------------------------------------------------------------------------%

pop_stack_frame_check(Check, !Runtime) :-
    ( if
        list.length(!.Runtime ^ stack_frames, Check)
    then
       pop_stack_frame(!Runtime)
    else
        exception.throw(exception.software_error("Stack mismatch"))
    ).

%-----------------------------------------------------------------------------%

def_var(Name, Value, !Runtime) :-
    !.Runtime ^ stack_frames = StackFrames,
    (
        StackFrames = [frame(In)|Tail],
        
        ( assoc_list.remove(In, Name, _, V) -> Out = V ; Out = In ),
        
        !Runtime ^ stack_frames := [frame([pair.pair(Name, Value)|Out])|Tail]
    ;
        StackFrames = [],
        
        !.Runtime ^ globals = In,
        rbtree.set(Name, Value, In, Out),
        !Runtime ^ globals := Out
    ).

%-----------------------------------------------------------------------------%

find_var([], Globals, Name, Value) :- rbtree.search(Globals, Name, Value).
find_var([frame(Head)|Tail], Globals, Name, Value) :-
    ( if
        assoc_list.search(Head, Name, SemiValue)
    then
        Value = SemiValue
    else
        find_var(Tail, Globals, Name, Value)
    ).

%-----------------------------------------------------------------------------%

builtin_bind(args("=", 4), mercury_bind(turbolisp__runtime__builtin__builtin_eq_bind)).
builtin_bind(args("!", 4), mercury_bind(turbolisp__runtime__builtin__builtin_ne_bind)).
builtin_bind(args("<", 4), mercury_bind(turbolisp__runtime__builtin__builtin_lt_bind)).
builtin_bind(args(">", 4), mercury_bind(turbolisp__runtime__builtin__builtin_gt_bind)).
builtin_bind(args("<=", 4), mercury_bind(turbolisp__runtime__builtin__builtin_le_bind)).
builtin_bind(args(">=", 4), mercury_bind(turbolisp__runtime__builtin__builtin_ge_bind)).

builtin_bind(args("+", 2), mercury_bind(turbolisp__runtime__builtin__builtin_plus_bind)).
builtin_bind(args("-", 2), mercury_bind(turbolisp__runtime__builtin__builtin_minus_bind)).
builtin_bind(args("*", 2), mercury_bind(turbolisp__runtime__builtin__builtin_times_bind)).
builtin_bind(args("/", 2), mercury_bind(turbolisp__runtime__builtin__builtin_divide_bind)).

builtin_bind(args("fn", 3), mercury_bind(turbolisp__runtime__builtin__builtin_fn_bind)).

%-----------------------------------------------------------------------------%

def_bind(BindSpec, Bind, !Runtime) :-
    Binds = !.Runtime ^ binds,
    !Runtime ^ binds := rbtree.set(Binds, BindSpec, Bind).

%-----------------------------------------------------------------------------%

find_bind(Name, Arity, Tree, Out) :-
    % Try for set args before trying for variadic args.
    Args = args(Name, Arity), Variadic = variadic(Name),
    ( if
        rbtree.search(Tree, Args, Bind)
    then
        Out = Bind
    else if
        builtin_bind(Args, Bind)
    then
        Out = Bind
    else if
        rbtree.search(Tree, Variadic, Bind)
    then
        Out = Bind
    else
        builtin_bind(Variadic, Out)
    ).

%-----------------------------------------------------------------------------%

call_bind(mercury_bind(Pred), Args, Result, !Runtime) :-
    call(Pred, Args, Result:result, !Runtime).

call_bind(lisp_bind(ArgNames, Body), Args, Result, !Runtime) :-
    
    assoc_list.from_corresponding_lists(ArgNames, Args, Variables),
    
    % This is needed both for a func call, and just to yield the reduced
    % version of this list if it is not executable.
    push_stack_frame_check(Variables, Check, !Runtime),
    % trace [io(!IO)] ( io.write_string("Push stack from in call_bind\n", !IO) ),
    
    list.map_foldl2(execute, Body, Values, !Runtime, maybe.ok, CallResult),
    
    % trace [io(!IO)] ( io.write_string("Pop stack from in call_bind\n", !IO) ),
    pop_stack_frame_check(Check, !Runtime),
    
    (
        CallResult = maybe.ok,
        ( if
            list.last(Values, Last)
        then
            Result = maybe.ok(Last)
        else
            Result = maybe.ok(nil)
        )
    ;
        CallResult = maybe.error(Error),
        Result = maybe.error(Error)
    ).

%-----------------------------------------------------------------------------%
% Result of preprocessing.
% Comparison is a special case because of laziness.
:- type preprocess_result --->
    reduced(element) ; % Result is fully reduced.
    execute(string, list(element), preprocess_arity::int) ; % Result is a call.
    comparison(turbolisp.runtime.builtin.comparison, element, element, list(element)).

%-----------------------------------------------------------------------------%
% Performs preprocessing logic which is shared between reduce and execute.
:- pred preprocess(run_pred3, element, maybe.maybe_error(preprocess_result), runtime, runtime).
:- mode preprocess(run_pred3, in, res_uo, in, out) is det.

% Pass atoms through unchanged.
preprocess(_, atom(Str), maybe.ok(reduced(atom(Str))), !Runtime).

% Empty list, nothing to do.
preprocess(_, list([]), maybe.ok(reduced(list([]))), !Runtime).

% Do a maybe-reduce on a list with a list as its head.
preprocess(Pred, list(ElementsRaw @ [list(_)|_]), Result, !Runtime) :-
    list.map_foldl3(Pred, ElementsRaw, Elements,
        !Runtime,
        0, ArgNum,
        maybe.ok, ElementsError),
    (
        ElementsError = maybe.error(Error),
        Result = maybe.error(Error)
    ;
        ElementsError = maybe.ok,
        (
            ( Elements = [] ; Elements = [list(_)|_] ),
            Result = maybe.ok(reduced(list(Elements)))
        ;
            Elements = [atom(Tag)|Tail],
            Result = maybe.ok(execute(Tag, Tail, ArgNum))
        )
    ).

% Report a call for a list consisting of just an atom.
preprocess(_, list([atom(Tag)|[]]), maybe.ok(execute(Tag, [], 0)), !Runtime).

% Do a maybe-reduce on a list with an atom as its head.
preprocess(Pred, In @ list([atom(Tag)|Tail]), Result, !Runtime) :-
    Tail = [_|_],
    ( if
        Tag = "."
    then
        % Escaped list.
        Result = maybe.ok(reduced(In))
    else if
        % Special handling for comparisons, since they must be laziy evaluated.
        turbolisp.runtime.builtin.builtin_op_tag(Op, Tag),
        turbolisp.runtime.builtin.comparison(Cmp) = Op
    then
        % Sort of punt on argument lists less than size 2.
        % These will be errors later anyway.
        (
            Tail = [_|[]],
            Result = maybe.ok(execute(Tag, Tail, 1))
        ;
            [E1|[E2|Tail2]] = Tail,
            Pred(E1, R1, !Runtime, 0, _, maybe.ok, ResultMid),
            Pred(E2, R2, !Runtime, 0, _, ResultMid, PredResult),
            (
                PredResult = maybe.ok,
                Result = maybe.ok(comparison(Cmp, R1, R2, Tail2))
            ;
                PredResult = maybe.error(Error),
                Result = maybe.error(Error)
            )
        )
    else
        list.map_foldl3(Pred, Tail, ReducedTail,
            !Runtime,
            0, ArgNum,
            maybe.ok, ElementsError),
        (
            ElementsError = maybe.error(Error),
            Result = maybe.error(Error)
        ;
            ElementsError = maybe.ok,
            Result = maybe.ok(execute(Tag, ReducedTail, ArgNum))
        )
    ).

%-----------------------------------------------------------------------------%

:- pred is_atom(element).
:- mode is_atom(in) is semidet.

is_atom(atom(_)).

%-----------------------------------------------------------------------------%

:- pred is_atom_or_list_of_atoms(element).
:- mode is_atom_or_list_of_atoms(in) is semidet.

is_atom_or_list_of_atoms(atom(_)).
is_atom_or_list_of_atoms(list([])).
is_atom_or_list_of_atoms(list(List @ [_|_])) :- list.all_true(is_atom, List).

%-----------------------------------------------------------------------------%
% Reduces an element. This is mainly different in how it handles results from
% binds, and how it handles comparisons.
reduce(Element, Result, !Runtime) :-
    preprocess(reduce, Element, PreprocessResult, !Runtime),
    (
        PreprocessResult = maybe.error(Error),
        Result = maybe.error(Error)
    ;
        PreprocessResult = maybe.ok(PreprocessOutput),
        (
            PreprocessOutput = reduced(Reduced),
            Result = maybe.ok(Reduced)
        ;
            PreprocessOutput = comparison(Cmp, A, B, Tail),
            
            % Try to inline the result of the comparison, if possible.
            % This also allows us to not even compile the side which was not used.
            turbolisp.runtime.builtin.comparison_tag(Cmp, Tag),
            FallbackResult = maybe.ok(list([atom(Tag)|Tail])),
            (
                % Incorrect tail length for comparison builtin. Good luck kid.
                ( Tail = [] ; Tail = [_|[]] ; Tail = [_|[_|[_|_]]] ),
                Result = FallbackResult
            ;
                Tail = [Y|[N|[]]],
                turbolisp.runtime.builtin.comparison(Cmp, A, B, CmpResult),
                (
                    CmpResult = turbolisp.runtime.builtin.error(_),
                    Result = FallbackResult
                ;
                    (
                        CmpResult = turbolisp.runtime.builtin.yes, Choice = Y
                    ;
                        CmpResult = turbolisp.runtime.builtin.no, Choice = N
                    ),
                    
                    % It should be safe to reduce the result. EIther it is known at
                    % compile-time, or the comparison will have failed to yield a
                    % result and we won't be in this arm.
                    reduce(Choice, ChoiceResult, !Runtime),
                    (
                        ChoiceResult = maybe.error(_),
                        Result = FallbackResult
                    ;
                        ChoiceResult = maybe.ok(_),
                        Result = ChoiceResult
                    )
                )
            )
        ;
            PreprocessOutput = execute(Tag, Tail, Arity),
            ( if
                % Do NOT use the results of define ops during reduction.
                % For let's, the existence of the let will be erased by popping
                % the stack frame, and the value will not show up later in the
                % actual execution.
                % For fn's def's, this would erase the definition entirely as
                % we may lose the entire runtime between reduction and
                % execution (as in copmilation model).
                % We can still retain the reduced tail, however.
                % It is also useful to actually bind the value anyway, since
                % this lets us inline functions and variables.
                % See below for inlining determination.
                turbolisp__runtime__builtin__builtin_op_tag(
                    turbolisp__runtime__builtin__define(Op), Tag)
            then
                (
                    Op = turbolisp.runtime.builtin.fn,
                    % Super rudimentary inline test.
                    % Only inline fn if we have a body consisting of less than
                    % 64 elements, and all the elements are either atoms or a
                    % list of atoms (as opposed to a list with list elements).
                    ( if
                        list.index0(Tail, 1, Body),
                        (
                            Body = atom(_)
                        ;
                            Body = list(List),
                            builtin__compare((<), list.length(List), 64),
                            list.all_true(is_atom_or_list_of_atoms, List)
                        )
                    then
                        turbolisp.runtime.builtin.builtin_fn_bind(Tail, _, !Runtime)
                    else
                        true
                    )
                ;
                    Op = turbolisp.runtime.builtin.let
                ;
                    Op = turbolisp.runtime.builtin.def
                ),
                Result = maybe.ok(list([atom(Tag)|Tail]))
            else if
                find_bind(Tag, Arity, !.Runtime ^ binds, Bind)
            then
                call_bind(Bind, Tail, CallResult, !Runtime),
                
                (
                    CallResult = maybe.error(Error),
                    Result = maybe.error(func_error(Tag, Arity, Error))
                ;
                    CallResult = maybe.ok(_),
                    Result = CallResult
                )
            else
                Result = maybe.ok(list([atom(Tag)|Tail]))
            )
        )
    ).

%-----------------------------------------------------------------------------%

reduce(!E, !R, maybe.error(E), maybe.error(E)).
reduce(In, Out, !Runtime, maybe.ok, Result) :-
    reduce(In, OutResult, !Runtime),
    (
        OutResult = maybe.error(Error),
        Result = maybe.error(Error),
        In = Out
    ;
        OutResult = maybe.ok(Out),
        Result = maybe.ok
    ).

%-----------------------------------------------------------------------------%

reduce(!Element, !Runtime, N, int.plus(N, 1), !Error) :-
    reduce(!Element, !Runtime, !Error).

%-----------------------------------------------------------------------------%

execute(Element, Result, !Runtime) :-
    preprocess(reduce, Element, PreprocessResult, !Runtime),
    (
        PreprocessResult = maybe.error(Error),
        Result = maybe.error(Error)
    ;
        PreprocessResult = maybe.ok(PreprocessOutput),
        (
            PreprocessOutput = reduced(list(ReducedList)),
            % Remove escaping during execution.
            ( if
                ReducedList = [atom(".")|Tail]
            then
                Result = maybe.ok(list(Tail))
            else
                Result = maybe.ok(list(ReducedList))
            )
        ;
            PreprocessOutput = reduced(atom(ReducedAtom)),
            ( if
                find_var(!.Runtime ^ stack_frames,
                    !.Runtime ^ globals,
                    ReducedAtom, SemiValue)
            then
                Result = maybe.ok(SemiValue)
            else
                Result = maybe.ok(atom(ReducedAtom))
            )
        ;
            PreprocessOutput = comparison(Cmp, A, B, Tail),
            
            (
                % Incorrect tail length for comparison builtin. Good luck kid.
                ( Tail = [] ; Tail = [_|[]] ; Tail = [_|[_|[_|_]]] ),
                turbolisp.runtime.builtin.comparison_tag(Cmp, Tag),
                Result = maybe.error(func_error(Tag, 2, "Comparison must have arity of 2"))
            ;
                Tail = [Y|[N|[]]],
                turbolisp.runtime.builtin.comparison(Cmp, A, B, CmpResult),
                (
                    CmpResult = turbolisp.runtime.builtin.error(Error),
                    turbolisp.runtime.builtin.comparison_tag(Cmp, Tag),
                    Result = maybe.error(func_error(Tag, 2, Error))
                ;
                    (
                        CmpResult = turbolisp.runtime.builtin.yes, Choice = Y
                    ;
                        CmpResult = turbolisp.runtime.builtin.no, Choice = N
                    ),
                    
                    % It should be safe to reduce the result. EIther it is known at
                    % compile-time, or the comparison will have failed to yield a
                    % result and we won't be in this arm.
                    reduce(Choice, ChoiceResult, !Runtime),
                    (
                        ChoiceResult = maybe.error(Error),
                        turbolisp.runtime.builtin.comparison_tag(Cmp, Tag),
                        Result = maybe.error(func_error(Tag, 2, Error))
                    ;
                        ChoiceResult = maybe.ok(_),
                        Result = ChoiceResult
                    )
                )
            )
        ;
            PreprocessOutput = execute(Tag, Tail, Arity),
            
            ( if
                find_bind(Tag, Arity, !.Runtime ^ binds, Bind)
            then
                call_bind(Bind, Tail, CallResult, !Runtime),
                (
                    CallResult = maybe.error(Error),
                    Result = maybe.error(func_error(Tag, Arity, Error))
                ;
                    CallResult = maybe.ok(_),
                    Result = CallResult
                )
            else
                Result = maybe.ok(list([atom(Tag)|Tail]))
            )
        )
    ).

%-----------------------------------------------------------------------------%

execute(!E, !R, maybe.error(E), maybe.error(E)).
execute(In, Out, !Runtime, maybe.ok, Result) :-
    execute(In, OutResult, !Runtime),
    (
        OutResult = maybe.error(Error),
        Result = maybe.error(Error),
        In = Out
    ;
        OutResult = maybe.ok(Out),
        Result = maybe.ok
    ).

%-----------------------------------------------------------------------------%

execute(!Element, !Runtime, N, int.plus(N, 1), !Error) :-
    execute(!Element, !Runtime, !Error).
