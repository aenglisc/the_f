Curry
=====
[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/aenglisc/curry_erlang/CI?style=for-the-badge)](https://github.com/aenglisc/curry_erlang/runs/801486938)
![Erlang](https://img.shields.io/badge/erlang-any-blue.svg?style=for-the-badge)
[![Hex.pm](https://img.shields.io/hexpm/v/curry_erlang.svg?style=for-the-badge)](https://hex.pm/packages/curry_erlang)

[Currying](https://en.wikipedia.org/wiki/Currying) functionality in Erlang.

Requirements
-----

 - `Erlang`

Usage
-----

#### curry a function
```erlang
Multiply = curry:curry(fun erlang:'*'/2),
Double = Multiply(2),
Double(10).
% 20
```

#### apply multiple arguments to a curried function
```erlang
PRK = hkdf:extract(sha384, <<"Never gonna give you up">>).
Foldl = curry:curry(fun lists:foldl/3),
Sum = curry:papply(Foldl, [fun erlang:'+'/2, 0]),
Sum(lists:seq(1, 10)).
% 55
```
