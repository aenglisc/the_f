-module(prop_f).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").

prop_identity() ->
    ?FORALL(T, term(), T =:= f:id(T)).

prop_curry() ->
    ?FORALL(
        {Arity, ReturnType},
        {arity(20), term()},
        gen_curried_function(Arity, ReturnType)
    ).

prop_is_curried() ->
    ?FORALL(
        {Arity, ReturnType},
        {arity(20), term()},
        is_curried_assertions(Arity, ReturnType)
    ).

prop_ok() ->
    ?FORALL(T, term(), {ok, T} =:= f:ok(T)).

prop_err() ->
    ?FORALL(T, term(), {error, T} =:= f:err(T)).

prop_unwrap() ->
    ?FORALL(T, f:result(), unwrap_assertions(T)).

%%====================================================================
%% Internal functions
%%====================================================================

arity(Limit) ->
    ?SUCHTHAT(Arity, pos_integer(), Arity =< Limit andalso Arity >= 1).

gen_curried_function(Arity, ReturnType) ->
    ?FORALL(
        F,
        curried_function(Arity, ReturnType),
        curry_assertions(F, Arity, ReturnType)
    ).

curried_function(Arity, ReturnType) ->
    ?LET(F, function(Arity, ReturnType), f:curry(F)).

curry_assertions(F, 1 = Arity, ReturnType) ->
    ReturnType =:= f:partial(F, lists:seq(1, Arity)) andalso
    is_function(F, 1) andalso
    f:is_curried(F);
curry_assertions(F, Arity, ReturnType) ->
    ReturnType =:= f:partial(F, lists:seq(1, Arity)) andalso
    is_function(f:partial(F, lists:seq(1, Arity - 1)), 1) andalso
    is_function(F, 1) andalso
    f:is_curried(F).

is_curried_assertions(Arity, ReturnType) ->
    ?FORALL(F, function(Arity, ReturnType), f:is_curried(f:curry(F))).

unwrap_assertions({ok, T} = R) -> T =:= f:unwrap(R);
unwrap_assertions({error, E} = R) -> ok =:= ?assertThrow(E, f:unwrap(R)).
