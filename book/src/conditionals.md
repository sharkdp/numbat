# Conditionals

Numbat has `if-then-else` conditional expressions with the following
syntax
``` nbt
if <cond> then <expr1> else <expr2>
```
where `<cond>` is a condition that evaluates to a Boolean value, like
`3 ft < 3 m`. The types of `<expr1>` and `<expr2>` need to match.

## Example: step function

```nbt
fn step(x: Scalar) -> Scalar = if x < 0 then 0 else 1
```

## Example: min function

```nbt
fn min<T>(x: T, y: T) -> T = if x < y then x else y
```

## Example: Fibonacci numbers

Naive recursive Fibonacci implementation

```nbt
fn fib(n: Scalar) -> Scalar =
  if n ≤ 2
    then 1
    else fib(n - 2) + fib(n - 1)
```
