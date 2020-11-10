The f
=====
[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/aenglisc/the_f/CI?style=for-the-badge)](https://github.com/aenglisc/the_f/runs/801486938)
![Erlang](https://img.shields.io/badge/erlang-any-blue.svg?style=for-the-badge)
[![Hex.pm](https://img.shields.io/hexpm/v/f.svg?style=for-the-badge)](https://hex.pm/packages/the_f)

A collection of functions and types to make your Erlang code more functional, funky and/or fancy.

Examples
-----

#### curry
```erlang
Multiply = f:curry(fun erlang:'*'/2),
Triple = Multiply(3),
Triple(23).
% 69
```

#### flip
```erlang
Get = (f:curry(fun maps:get/3))(a),
GetWithDefault = (f:flip(Get))(42),
GetWithDefault(#{}).
% 42
```

#### partial
```erlang
Sum = f:partial(fun lists:foldl/3, [fun erlang:'+'/2, 0]),
Sum(lists:seq(1, 10)).
% 55
```

#### pipe
```erlang
f:pipe(1, [
    (f:curry(fun erlang:'+'/2))(1),
    (fun(X) -> X * 2 end),
    (f:partial(fun lists:seq/3, [1, 50]))
]).
% [1,5,9,13,17,21,25,29,33,37,41,45,49]
```

#### do
```erlang
f:do(2, [
   (fun(X) -> f:ok(X * 21) end),
   (fun(X) -> f:ok(X * 10) end)
]).
%% {ok, 420}

f:do(1, [
   (fun(X) -> f:ok(X * 1) end),
   (fun(_) -> f:err(go_away) end),
   (fun(X) -> f:ok(X + 1) end)
]).
%% {error, go_away}
```
