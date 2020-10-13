-module(curry_SUITE).

-import(curry, [curry/1, papply/2]).

-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).

-export([curry_zero_arity_fails/1]).
-export([curry_single_arity_fails/1]).
-export([curry_success/1]).
-export([curry_application/1]).

-export([papply_non_single_arity_fails/1]).
-export([papply_too_many_args/1]).
-export([papply_success/1]).
-export([papply_returns_curried/1]).

all() ->
    [
        {group, curry},
        {group, papply}
    ].

groups() ->
    [
        {curry, [parallel], [
            curry_zero_arity_fails,
            curry_single_arity_fails,
            curry_success,
            curry_application
        ]},
        {papply, [parallel], [
            papply_non_single_arity_fails,
            papply_too_many_args,
            papply_success,
            papply_returns_curried
        ]}
    ].

curry_zero_arity_fails(_) ->
    ?assertError(badarg, curry(fun node/0)).

curry_single_arity_fails(_) ->
    ?assertError(badarg, curry(fun node/1)).

curry_success(_) ->
    ?assert(is_function(curry(fun lists:foldl/3), 1)).

curry_application(_) ->
    Multiply = curry(fun erlang:'*'/2),
    Double = Multiply(2),
    ?assert(is_function(Multiply, 1)),
    ?assert(is_function(Double, 1)),
    ?assertEqual(20, Double(10)).

papply_non_single_arity_fails(_) ->
    ?assertError(badarg, papply(fun lists:foldl/3, [])).

papply_too_many_args(_) ->
    Multiply = curry(fun erlang:'*'/2),
    ?assertError(badarg, papply(Multiply, [1, 2, 4])).

papply_success(_) ->
    Multiply = curry(fun erlang:'*'/2),
    ?assertEqual(2, papply(Multiply, [1, 2])).

papply_returns_curried(_) ->
    Foldl = curry(fun lists:foldl/3),
    Sum = papply(Foldl, [fun erlang:'+'/2]),
    ?assert(is_function(Foldl, 1)),
    ?assert(is_function(Sum, 1)),
    ?assertEqual(55, (Sum(0))(lists:seq(1, 10))).
