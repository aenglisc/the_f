%%====================================================================
%% @author Roman Pushkov <pushkovroman@me.com>
%% @copyright (C) 2020, Roman Pushkov
%%
%% @doc
%% Currying functionality for Erlang.
%% @end
%%====================================================================
-module(curry).

%% API exports

-export([curry/1]).
-export([papply/2]).

-type curried(T) :: fun((term()) -> curried(T) | T).

%%====================================================================
%% API functions
%%====================================================================

%%====================================================================
%% @doc
%% The curry function
%%
%% ```
%% Turns a multiple-arity function into a sequence of single-arity
%% functions, so that F(X, Y, Z) becomes Fcurried(X)(Y)(Z).
%%
%% Note that Erlang's syntax will require extra parentheses
%% if you wish to pass the arguments in a sequence.
%%
%% Example:
%% > Multiply = curry:curry(fun erlang:'*'/2),
%% > Double = Multiply(2),
%% > Double(10).
%% 20
%% > Map = curry:curry(fun lists:map/2),
%% > Sequence = curry:curry(fun lists:seq/2),
%% > From1To = Sequence(1),
%% > (Map(Double))(From1To(10)),
%% [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
%%
%% '''
%% @end
%%====================================================================
-spec curry((fun((...) -> T))) -> curried(T).
curry(Fun) when not is_function(Fun) -> error(badarg);
curry(Fun) when is_function(Fun, 0) -> error(badarg);
curry(Fun) when is_function(Fun, 1) -> error(badarg);
curry(Fun) -> curry_(Fun, function_arity(Fun), []).

%%====================================================================
%% @doc
%% The papply function
%%
%% ```
%% Applies a list of arguments to a curried function.
%%
%% Example:
%% > Foldl = curry:curry(fun lists:foldl/3),
%% > Sum = curry:papply(Foldl, [fun erlang:'+'/2, 0]),
%% > Sum(lists:seq(1, 10)).
%% 55
%%
%% '''
%% @end
%%====================================================================
-spec papply(curried(T), [term()]) -> curried(T) | T.
papply(Fun, Args) when not is_function(Fun, 1); not is_list(Args) ->
    error(badarg);
papply(Fun, Args) ->
    lists:foldl(fun applicator/2, Fun, Args).

%%====================================================================
%% Internal functions
%%====================================================================

-spec curry_(fun((...) -> T), pos_integer(), list()) -> curried(T).
curry_(Fun, Arity, Args) when Arity =:= 1 ->
    fun(LazyArg) -> apply(Fun, lists:reverse([LazyArg | Args])) end;
curry_(Fun, Arity, Args) ->
    fun(LazyArg) -> curry_(Fun, Arity - 1, [LazyArg | Args]) end.

-spec function_arity(fun()) -> non_neg_integer().
function_arity(Fun) ->
    erlang:element(2, erlang:fun_info(Fun, arity)).

-spec applicator(term(), curried(T)) -> curried(T).
applicator(X, F) when is_function(F, 1) -> F(X);
applicator(_, _) -> error(badarg).
