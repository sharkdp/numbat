# Insect

Insect is a statically typed programming language for scientific computations with
first class support for physical units.

## Key features

  * Statically typed: the 
  * Type inference
  * Strict syntax
  * Customizable: the whole system of physical dimensions and units is written in Insect itself and can be modified or replaced
  * (Modular)

## Type system

Insects treats *physical dimensions* like length or time as *types*. A value of `5 meter` is of type `Length`. A value of `2.5 inch`
is also of type `Length`.


### Limitations

Let's look at an exponentiation expression like
```
expr1 ^ expr2
```
where `expr1` and `expr2` are arbitrary expressions. In order for that expression
to properly type check, the *type* of `expr2` must be `Scalar` — something like
`2^meter` does not make any sense. *If* the type of `expr1` is also `Scalar`,
everything is well and the type of the total expression is also `Scalar`. An example
for this trivial case is an expression like `e^(-x²/σ²)`. As long as the type
of `x` is the same as the type of `σ`, this is fine.

A more interesting case arises if `expr1` is dimensionfull, as in `meter^3`. Here,
things become difficult: in order to compute the *type* of the total expression
`expr1 ^ expr2`, we need to know the *value* of `expr2`. For the `meter^3` example,
the answer is `Length^3`. This seems straightforward. However, the syntax of the
language allows arbitrary expressions in the exponent. This is important to support
use cases like the above `e^(-x²/σ²)`. But it poses a problem for the type checker.
In order to compute the type of `expr1 ^ expr2`, we need to fully *evaluate*
`expr2` at compile time. This is not going to work in general. Just think of a
hypothetical expression like `meter^f()` where `f()` could do *anything*. Maybe even
get some input from the user at runtime.

Insects solution to this problem looks like this: If `expr1` is *not* dimensionless, 
we restrict `expr2` to a small subset of allowed operations that can be fully
evaluated at compile time (similar to `constexpr` expressions in C++, `const`
expressions in Rust, etc). Expressions like `meter^(2 * (2 + 1) / 3)` are completely
fine and can be typechecked (`Length^2`), but things like function calls are not
allowed and will lead to a compile time error.

To summarize: Given an exponentiation expression like `expr1 ^ expr2`, the type checker
requires that:

  * `expr2` is of type `Scalar`
  * One of the following:
    * `expr1` is also of type `Scalar`
    * `expr2` can be *evaluated at compile time* and yields a rational number.

#### Remark

We would probably need to enter the world of *dependent types* if we wanted to fully
support exponentiation expressions without the limitations above. For example, consider
the function `f(x, n) = x^n`. The return type of that function *depends on the value*
of the parameter `n`.


## Reasons for rewriting Insect in Rust:

  - Rust is much more popular amongst developers => more possible contributors
  - A redesign from scratch would allow me to focus on areas of improvement:
      - Introducing the concept of physical *dimensions* into the language
      - Experimenting with a static dimension/unit checker
      - Allowing user-defined units => move all of the unit definitions to the Insect language
      - Better parser errors
      - Automated tracking of significant digits
      - Support for rational numbers? Complex numbers? Intervals?
      - Support for binary and hexadecimal numbers, bitwise operators, etc
      - Support for notepad-style computations (Mathematica/Jupyter style)
  - The PureScript implementation is *slow*. A Rust-based parser & interpreter could be much faster. Not just
    on the command-line (startup speed!) but also on the Web (via WASM)
  - It would be a nice playground for a WASM project
