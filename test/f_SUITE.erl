-module(f_SUITE).

-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).

-export([identity_success/1]).

-export([curry_not_a_function/1]).
-export([curry_zero_arity/1]).
-export([curry_success/1]).
-export([curry_application/1]).

-export([is_curried_not_a_function/1]).
-export([is_curried_true/1]).
-export([is_curried_false/1]).

-export([flip_not_a_function/1]).
-export([flip_ordinary/1]).
-export([flip_curried/1]).

-export([partial_not_a_function/1]).
-export([partial_too_many_args/1]).
-export([partial_success/1]).
-export([partial_returns_curried/1]).

-export([pipe_success/1]).
-export([pipe_invalid_lambda/1]).

-export([ok_success/1]).
-export([err_success/1]).

-export([unwrap_ok/1]).
-export([unwrap_err/1]).

-export([do_ok/1]).
-export([do_err/1]).

all() ->
    [
        {group, identity},
        {group, curry},
        {group, is_curried},
        {group, flip},
        {group, partial},
        {group, pipe},
        {group, ok},
        {group, err},
        {group, unwrap},
        {group, do}
    ].

groups() ->
    [
        {identity, [parallel], [
            identity_success
        ]},
        {curry, [parallel], [
            curry_not_a_function,
            curry_zero_arity,
            curry_success,
            curry_application
        ]},
        {is_curried, [parallel], [
            is_curried_not_a_function,
            is_curried_true,
            is_curried_false
        ]},
        {flip, [parallel], [
            flip_not_a_function,
            flip_ordinary,
            flip_curried
        ]},
        {partial, [parallel], [
            partial_not_a_function,
            partial_too_many_args,
            partial_success,
            partial_returns_curried
        ]},
        {pipe, [parallel], [
            pipe_success,
            pipe_invalid_lambda
        ]},
        {ok, [parallel], [
            ok_success
        ]},
        {err, [parallel], [
            err_success
        ]},
        {unwrap, [parallel], [
            unwrap_ok,
            unwrap_err
        ]},
        {do, [parallel], [
            do_ok,
            do_err
        ]}
    ].

%%====================================================================
%% id/1
%%====================================================================

identity_success(_) ->
    F = fun(_) -> hello end,
    ?assertEqual(F, f:id(F)).

%%====================================================================
%% curry/1
%%====================================================================

curry_not_a_function(_) ->
    ?assertError({'Not a function', kek}, f:curry(kek)).

curry_zero_arity(_) ->
    ?assertError({'Not curriable', _}, f:curry(fun node/0)).

curry_success(_) ->
    ?assert(is_function(f:curry(fun lists:foldl/3), 1)).

curry_application(_) ->
    Multiply = f:curry(fun erlang:'*'/2),
    Double = Multiply(2),
    ?assert(is_function(Multiply, 1)),
    ?assert(is_function(Double, 1)),
    ?assertEqual(20, Double(10)).

%%====================================================================
%% is_curried/1
%%====================================================================

is_curried_not_a_function(_) ->
    ?assertError({'Not a function', kek}, f:is_curried(kek)).

is_curried_true(_) ->
    ?assert(f:is_curried(f:curry(fun maps:get/3))),
    ?assert(f:is_curried(fun lists:sum/1)).

is_curried_false(_) ->
    ?assertNot(f:is_curried(fun maps:get/3)).

%%====================================================================
%% flip/1
%%====================================================================

flip_not_a_function(_) ->
    ?assertError({'Not a function', kek}, f:flip(kek)).

flip_ordinary(_) ->
    SubtractCurried = f:curry(fun erlang:'-'/2),
    SubtractFlipped = f:flip(fun erlang:'-'/2),
    ?assertEqual(1, (SubtractCurried(2))(1)),
    ?assertEqual(-1, (SubtractFlipped(2))(1)).

flip_curried(_) ->
    Get = (f:curry(fun maps:get/3))(a),
    GetWithDefault = (f:flip(Get))(42),
    ?assertEqual(42, GetWithDefault(#{})).

%%====================================================================
%% partial/2
%%====================================================================

partial_not_a_function(_) ->
    ?assertError({'Not a function', kek}, f:partial(kek, [lul])).

partial_too_many_args(_) ->
    Multiply = fun erlang:'*'/2,
    Args = [1, 2, 4],
    ?assertError('Too many arguments', f:partial(Multiply, Args)).

partial_success(_) ->
    ?assertEqual(2, f:partial(fun erlang:'*'/2, [1, 2])).

partial_returns_curried(_) ->
    Sum = f:partial(fun lists:foldl/3, [fun erlang:'+'/2]),
    ?assert(is_function(Sum, 1)),
    ?assertEqual(55, (Sum(0))(lists:seq(1, 10))).

%%====================================================================
%% pipe/2
%%====================================================================

pipe_success(_) ->
    Pipe = (f:flip(fun f:pipe/2))([
        (f:curry(fun erlang:'+'/2))(1),
        (fun(X) -> X * 2 end),
        (f:partial(fun lists:seq/3, [1, 10]))
    ]),
    ?assertEqual([1, 5, 9], Pipe(1)).

pipe_invalid_lambda(_) ->
    Pipe = f:flip(fun f:pipe/2),
    Funs = [
       (f:curry(fun erlang:'+'/2))(1),
       (fun(X, _) -> X * 2 end),
       (f:partial(fun lists:seq/3, [1, 10]))
    ],
    ?assertError({'Invalid lambda', _}, ((Pipe)(Funs))(1)).

%%====================================================================
%% ok/1
%%====================================================================

ok_success(_) ->
    ?assertEqual({ok, boomer}, f:ok(boomer)).

%%====================================================================
%% err/1
%%====================================================================

err_success(_) ->
    ?assertEqual({error, zoomer}, f:err(zoomer)).

%%====================================================================
%% unwrap/1
%%====================================================================

unwrap_ok(_) ->
    ?assertEqual(boomer, f:unwrap(f:ok(boomer))).

unwrap_err(_) ->
    ?assertThrow(zoomer, f:unwrap(f:err(zoomer))).

%%====================================================================
%% do/2
%%====================================================================

do_ok(_) ->
    Name = <<"Joe">>,
    Do = (f:flip(fun f:do/2))([
        (fun(X) -> f:ok(<<"Hello, ", X/binary>>) end),
        (fun(X) -> f:ok(<<X/binary, $!>>) end)
    ]),
    ?assertEqual(f:ok(<<"Hello, Joe!">>), Do(Name)).

do_err(_) ->
    Do = (f:flip(fun f:do/2))([
        (fun(X) -> f:ok(X) end),
        (fun(_) -> f:err(go_away) end),
        (fun(X) -> f:ok(X * 2) end)
    ]),
    ?assertEqual(f:err(go_away), Do(1)).
