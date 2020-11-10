%%====================================================================
%% @author Roman Pushkov <pushkovroman@me.com>
%% @copyright (C) 2020, Roman Pushkov
%%
%% @doc
%% A collection of functions and types to make your Erlang code more
%% functional, funky and/or fancy.
%% @end
%%====================================================================
-module(f).

%% API exports

%% Functions
-export([id/1]).

-export([curry/1]).
-export([is_curried/1]).
-export([flip/1]).
-export([partial/2]).
-export([pipe/2]).

-export([ok/1]).
-export([err/1]).
-export([unwrap/1]).
-export([do/2]).

%% Types
-export_type([func/0]).
-export_type([func/1]).

-export_type([lambda/0]).
-export_type([lambda/1]).
-export_type([lambda/2]).

-export_type([curried/0]).
-export_type([curried/1]).
-export_type([curried/2]).
-export_type([curried/3]).

-export_type([ok/1]).
-export_type([err/1]).

-export_type([result/0]).
-export_type([result/1]).
-export_type([result/2]).

-export_type([safe/0]).
-export_type([safe/1]).
-export_type([safe/2]).
-export_type([safe/3]).

%%====================================================================
%% Types
%%====================================================================

-type func()            :: func(any()).
-type func(T)           :: fun((...) -> T).

-type lambda()          :: lambda(any()).
-type lambda(T)         :: lambda(any(), T).
-type lambda(X, T)      :: fun((X) -> T).

-type curried()         :: curried(any()).
-type curried(T)        :: lambda(any(), curried(T) | T).
-type curried(X, T)     :: lambda(X, curried(T) | T).
-type curried(X, Y, T)  :: lambda(X, curried(Y, T) | T).

-type ok(T)             :: {ok, T}.
-type err(E)            :: {error, E}.

-type result()          :: result(any(), any()).
-type result(T)         :: result(T, any()).
-type result(T, E)      :: ok(T) | err(E).

-type safe()            :: safe(any()).
-type safe(T)           :: lambda(result(T)).
-type safe(X, T)        :: lambda(X, result(T)).
-type safe(X, T, E)     :: lambda(X, result(T, E)).

%%====================================================================
%% API functions
%%====================================================================

%%====================================================================
%% @doc
%% The identity function
%%
%% ```
%% Returns the passed entity.
%%
%% Example:
%% > f:id(a).
%% a
%% '''
%% @end
%%====================================================================
-spec
id(T) -> T when T :: any().
id(T) -> id_(T).

%%====================================================================
%% @doc
%% The currying function
%%
%% ```
%% Turns a multiple-arity function into a sequence of single-arity
%% functions, so that F(X, Y, Z) becomes Fcurried(X)(Y)(Z).
%%
%% Note that Erlang's syntax will require extra parentheses
%% if you wish to pass the arguments in a single statement.
%%
%% Example:
%% > Multiply = f:curry(fun erlang:'*'/2),
%% > Double = Multiply(2),
%% > Double(10).
%% 20
%% > Map = f:curry(fun lists:map/2),
%% > Sequence = f:curry(fun lists:seq/2),
%% > From1To = Sequence(1),
%% > (Map(Double))(From1To(10)),
%% [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
%% '''
%% @end
%%====================================================================
-spec
curry(F) -> curried(T) when F :: func(T).
curry(F) -> curry_(F).

%%====================================================================
%% @doc
%% The is_curried function
%%
%% ```
%% Checks if a function is curried. Single-arity functions are
%% considered curried.
%%
%% Example:
%% > f:is_curried(fun(_) -> hello end).
%% true
%% > f:is_curried(fun lists:foldl/3).
%% false
%% > f:is_curried(f:curry(fun lists:foldl/3)).
%% true
%% '''
%% @end
%%====================================================================
-spec
is_curried(F) -> boolean() when F :: func().
is_curried(F) -> is_curried_(F).

%%====================================================================
%% @doc
%% The flip function
%%
%% ```
%% Flips the next two arguments of a curried function. A non-curried
%% function will be curried before flip is applied.
%% Will return an unchanged function if no more arguments to
%% be applied are left.
%%
%% Example:
%% > Get = (f:curry(fun maps:get/3))(a),
%% > GetWithDefault = (f:flip(Get))(42),
%% > GetWithDefault(#{}).
%% 42
%% '''
%% @end
%%====================================================================
-spec
flip(F :: func(T) | curried(X, Y, T)) -> curried(Y, X, T).
flip(F) -> flip_(F).

%%====================================================================
%% @doc
%% The partial application function
%%
%% ```
%% Partially applies a list of arguments to a function.
%%
%% Example:
%% > Foldl = f:partial(fun lists:foldl/3),
%% > Sum = f:partial(Foldl, [fun erlang:'+'/2, 0]),
%% > Sum(lists:seq(1, 10)).
%% 55
%% '''
%% @end
%%====================================================================
-spec
partial(F, list()) -> curried(T) | T when F :: func(T) | curried(T).
partial(F, Args) -> partial_(F, Args).

%%====================================================================
%% @doc
%% The pipe function
%%
%% ```
%% Pipes data through a list of single-arity functions.
%% Naturally, you may use partially applied functions.
%%
%% Example:
%% > f:pipe(1, [
%% >    (f:curry(fun erlang:'+'/2))(1),
%% >    (fun(X) -> X * 2 end),
%% >    (f:partial(fun lists:seq/3, [1, 50]))
%% > ]).
%% [1,5,9,13,17,21,25,29,33,37,41,45,49]
%% '''
%% @end
%%====================================================================
-spec
pipe(X, Fs) -> T when X :: any(), Fs :: [lambda()], T :: any().
pipe(X, Fs) -> pipe_(X, Fs).

%%====================================================================
%% @doc
%% The ok function
%%
%% ```
%% Wraps data into the ok type.
%%
%% Example:
%% > f:ok(boomer).
%% {ok, boomer}
%% '''
%% @end
%%====================================================================
-spec
ok(T) -> ok(T).
ok(T) -> ok_(T).

%%====================================================================
%% @doc
%% The err function
%%
%% ```
%% Wraps data into the err type.
%%
%% Example:
%% > f:err(zoomer).
%% {error, zoomer}
%% '''
%% @end
%%====================================================================
-spec
err(E) -> err(E).
err(E) -> err_(E).

%%====================================================================
%% @doc
%% The unwrap function
%%
%% ```
%% Unwraps the result type. Returns the actual result or throws
%% the stored error.
%% This can also be used inside a function called from f:do/1 in
%% order to stop the sequence and return an error.
%%
%% Example:
%% > f:unwrap(f:ok(1)).
%% 1
%% > f:unwrap(f:err(go_away)).
%% ** exception throw: go_away
%% '''
%% @end
%%====================================================================
-spec
unwrap(result(T)) -> T | no_return().
unwrap(T) -> unwrap_(T).

%%====================================================================
%% @doc
%% The do function
%%
%% ```
%% Similar to the pipe function, do pipes data through series of
%% functions, however in this case the functions should return the
%% result type: either an {ok, Value} or {error, Reason}.
%% Terminates on an error result, preventing further computation and
%% returning the error result itself.
%% The functions may also use unwrap/1 to stop any kind of computation
%% and return an error result.
%%
%% Example:
%% > f:do(1, [
%% >    (fun(X) -> f:ok(X * 1) end),
%% >    (fun(_) -> f:err(go_away) end),
%% >    (fun(X) -> f:ok(X + 1) end)
%% > ]).
%% {error, go_away}
%% '''
%% @end
%%====================================================================
-spec
do(X, Fs) -> result() when X :: any(), Fs :: [safe()].
do(X, Fs) -> do_(X, Fs).

%%====================================================================
%% Internal functions
%%====================================================================

-spec
id_(T) -> T when T :: any().
id_(T) -> T.

-spec
is_curried_(F) -> boolean() when F :: func().
is_curried_(F) when not is_function(F) -> not_a_function(F);
is_curried_(F) when not is_function(F, 1) -> false;
is_curried_(F) when is_function(F, 1) -> true.

-spec
curry_(F) -> curried(T) when F :: func(T).
curry_(F) when not is_function(F) -> not_a_function(F);
curry_(F) when is_function(F, 0) -> not_curriable(F);
curry_(F) when is_function(F, 1) -> id_(F);
curry_(F) when is_function(F) -> curried(F, function_arity(F), []).

-spec
curried(F, arity(), list()) -> curried(T) when F :: func(T).
curried(F, Arity, Args) when Arity =:= 1 ->
    fun(Arg) -> apply(F, lists:reverse([Arg | Args])) end;
curried(F, Arity, Args) when Arity =/= 1 ->
    fun(Arg) -> curried(F, Arity - 1, [Arg | Args]) end.

-spec
flip_(F :: func(T) | curried(X, Y, T)) -> curried(Y, X, T).
flip_(F) when not is_function(F) -> not_a_function(F);
flip_(F) when not is_function(F, 1) -> flip_(curry_(F));
flip_(F) when is_function(F, 1) ->
    case returns_lambda(F) of
        true -> fun(X) -> (fun(Y) -> (F(Y))(X) end) end;
        false -> id_(F)
    end.

-spec
returns_lambda(F) -> boolean() when F :: lambda() | func().
returns_lambda(F) ->
    try F(x) of G when is_function(G, 1) -> true
    catch _:_ -> false
    end.

-spec
pipe_(X, Fs) -> T when X :: any(), Fs :: [lambda()], T :: any().
pipe_(X, Fs) -> lists:foldl(fun piper/2, X, Fs).

-spec
piper(F, X) -> T when F :: lambda(X, T), X :: any().
piper(F, _) when not is_function(F, 1) -> invalid_lambda(F);
piper(F, X) when is_function(F, 1) -> F(X).

-spec
partial_(F, list()) -> curried(T) | T when F :: func(T) | curried(T).
partial_(F, _Args) when not is_function(F) -> not_a_function(F);
partial_(F, Args) -> lists:foldl(fun applicator/2, curry_(F), Args).

-spec
applicator(X, F) -> F | T when X :: any(), F :: curried(T).
applicator(X, F) when is_function(F, 1) -> F(X);
applicator(_, _) -> too_many_arguments().

-spec
function_arity(F) -> non_neg_integer() when F :: func().
function_arity(F) -> erlang:element(2, erlang:fun_info(F, arity)).

-spec
ok_(T) -> ok(T).
ok_(T) -> {ok, T}.

-spec
err_(E) -> err(E).
err_(E) -> {error, E}.

-spec
unwrap_(result(T)) -> T | no_return().
unwrap_({ok, T}) -> T;
unwrap_({error, E}) -> throw(E);
unwrap_(Invalid) -> not_a_result(Invalid).

-spec
do_(X, Fs) -> T when X :: any(), Fs :: [safe()], T :: result().
do_(X, Fs) ->
    try lists:foldl(fun unwrapper/2, X, Fs) of T -> ok_(T)
    catch Thrown -> err_(Thrown)
    end.

-spec
unwrapper(F, X) -> T when F :: safe(X, T), X :: any(), T :: any().
unwrapper(F, X) -> unwrap_(F(X)).

%%====================================================================
%% Errors
%%====================================================================

-spec
invalid_lambda(_) -> no_return().
invalid_lambda(X) -> error({'Invalid lambda', X}).

-spec
not_a_function(_) -> no_return().
not_a_function(X) -> error({'Not a function', X}).

-spec
not_a_result(_) -> no_return().
not_a_result(X) -> error({'Not a result', X}).

-spec
not_curriable(_) -> no_return().
not_curriable(X) -> error({'Not curriable', X}).

-spec
too_many_arguments() -> no_return().
too_many_arguments() -> error('Too many arguments').
