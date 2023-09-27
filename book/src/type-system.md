# Type system

Numbat is a language with a special type system that treats *physical dimensions* as types.
A type checker infers types for every expression in the program and ensures that everything is correct in terms of physical dimensions, which implies correctness in terms of physical *units*.
For example, the expression `2 meter` has a *type* of `Length`.
The expression `3 inch` *also* has a type of `Length`.
The combined expression `2 meter + 3 inch` is therefore well-typed.
On the other hand, `2 meter + 3 second` is ill-typed, as `3 second` is of type `Time`.

The type system is *static* which means that the correctness of a Numbat program is verified before the program starts executing.
Note that certain *runtime* errors (like division-by-zero) can still occur.

## Algebra of types


Types in Numbat can be combined in various ways to produce new types.
In its most general form, a type can be thought of as a product of physical (base) dimensions \\( D_k \\) with exponents \\( \alpha_k \in \mathbb{Q} \\):
\\[ \prod_k D_k^{\alpha_k} \\]
For example, the type *Energy* can be represented as *Mass¹ × Length² × Time⁻²*.

### Multiplication

This naturally allows us to *multiply* types (by combining the factors of both products into a single product).
We can use the `*` operator to construct types for physical dimensions that are products of two or more (base) dimensions. For example:
``` numbat
dimension Time
dimension Current
dimension Charge = Current * Time
```

### Exponentiation

We can also raise units to arbitrary powers \\( n \in \mathbb{Q} \\), by simply multiplying each \\( \alpha_k \\) with \\( n \\). The syntax uses the `^` exponentiation operator:
``` numbat
dimension Length
dimension Volume = Length^3

dimension Time
dimension Frequency = Time^(-1)
```

### Division

Once we have multiplication and exponentiation, we can define the *division* of two types as
``` numbat
TypeA / TypeB ≡ TypeA * TypeB^(-1)
```
This is mostly for convenience. It allows us to write definitions like
``` numbat
dimension Power = Energy / Time
```

> **Note:** When we talk about products of types in this section, we mean actual, literal products.
> Type theory also has the notion of *product types* which denote something else: compound types — like tuples or structs — that are built by combining two or more types. If we think of types in terms of the sets of all possible values that they represent, then product types represent the Cartesian product of those.

## Type inference and type annotations

The type checker can infer the types of (most) expressions without explicitly declaring them. For example,
the following definition does not mention any types:
``` numbat
let E_pot = 80 kg × 9.8 m/s² × 5 m
```
However, it is often helpful to specify the type anyway. This way, we can make sure that no mistakes were made:
``` numbat
let E_pot: Energy = 80 kg × 9.8 m/s² × 5 m
```
The type checker will compare the inferred type with the specified type and raise an error in case of inconsistency.

Function definitions also allow for type annotations, both for the parameters as well as the return type. The following example shows a function that takes a quantity of type `Length` and returns a `Pressure`:
``` numbat
let p0: Pressure = 101325 Pa
let t0: Temperature = 288.15 K

let gradient = 0.65 K / 100 m

fn air_pressure(height: Length) -> Pressure = p0 · (1 - gradient · height / t0)^5.255
```


## Generic types

Numbats type system also supports generic types (type polymorphism).
These can be used for functions that work regardless of the physical dimension of the argument(s).
For example, the type signature of the absolute value function is given by
``` numbat
fn abs<D>(x: D) -> D
```
where the angle brackets after the function name introduce new type parameters (`D`).
This can be read as: `abs` takes an arbitrary physical quantity of dimension `D` and returns a quantity of the *same* physical dimension `D`.

As a more interesting example, we can look at the `sqrt` function. Its type signature can be written as
``` numbat
fn sqrt<D>(x: D^2) -> D
```
Alternatively, it could also be specified as `fn sqrt<D>(x: D) -> D^(1/2)`.

## Limitations

The static type system also has some limitations. Let's look at an exponentiation expression like
``` numbat
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

Numbats solution to this problem looks like this: If `expr1` is *not* dimensionless, 
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
