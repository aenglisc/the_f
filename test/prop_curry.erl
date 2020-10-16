-module(prop_curry).

-import(curry, [curry/1, papply/2]).

-include_lib("proper/include/proper.hrl").

-export([prop_curry/0]).

prop_curry() ->
    ?FORALL(
        {Arity, ReturnType},
        {arity(20), term()},
        gen_curried_function(Arity, ReturnType)
    ).

gen_curried_function(Arity, ReturnType) ->
    ?FORALL(
        F,
        curried_function(Arity, ReturnType),
        assertions(F, Arity, ReturnType)
    ).

assertions(F, Arity, ReturnType) ->
    ReturnType =:= papply(F, lists:seq(1, Arity)) andalso
        is_function(F, 1) andalso
        is_function(papply(F, lists:seq(1, Arity - 1)), 1).

arity(Limit) ->
    ?SUCHTHAT(Arity, pos_integer(), Arity =< Limit andalso Arity > 1).

curried_function(Arity, ReturnType) ->
    ?LET(F, function(Arity, ReturnType), curry(F)).
