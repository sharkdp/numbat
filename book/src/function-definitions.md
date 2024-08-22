# Function definitions

Numbat comes with a large number of [predefined functions](./predefined-functions.md), but
it is also possible to add new functions. A function definition is introduced with
the `fn` keyword:

```nbt
fn max_distance(v: Velocity, θ: Angle) -> Length = v² · sin(2 θ) / g0
```

This exemplary function computes the maximum distance of a projectile under the
influence of Earths gravity. It takes two parameters (the initial velocity `v` and
the launch angle `θ`), which are both annotated with their corresponding physical
dimension (their type). The function returns a distance, and so the return type
is specified as `Length`.

## Type inference

Numbat has a powerful type inference system, which is able to infer missing types
when they are not explicitly specified. For example, consider the following function
definition for the braking distance of a car, given its velocity `v`:
```nbt
fn braking_distance(v) = v t_reaction + v² / 2 µ g0
  where t_reaction = 1 s # driver reaction time
    and µ = 0.7          # coefficient of friction
```
If you enter this function into the Numbat REPL, you will see that all types are filled
in automatically:
```nbt
fn braking_distance(v: Velocity) -> Length = v × t_reaction + (v² / (2 µ × g0))
  where t_reaction: Time = 1 second
    and µ: Scalar = 0.7
```
In particular, note that the type of the function argument `v` is correctly inferred as
`Velocity`, and the return type is `Length`.

> **Note**: This is possible because the types of `t_reaction`, `µ`, and `g0` (gravitational acceleration)
> are known. The `+` operator imposes a *constraint* on the types: two quantities can
> only be added if their physical dimension is the same. The type inference algorithm
> records constraints like this, and then tries to find a solution that satisfies all
> of them. In this case, only a single equation needs to be solved:
> ```
> type(v) × type(t_reaction) = type(v)² / (type(µ) × type(g0)      )
> type(v) × Time             = type(v)² / (      1 × Length / Time²)
> ```
> which has the solution `type(v) = Length / Time = Velocity`. Note that this also
> works if there are multiple constraints on the types. In fact, type inference is
> always decidable.

The fact that it is *possible* to omit type annotations does not mean that it is always
a good idea to do so.
Type annotations can help to make the code more readable and can also help to catch
errors earlier.

In some cases, type inference will also lead to function types that are overly generic.
For example, consider the following function to compute the kinetic energy of a massive
object in motion:

```nbt
fn kinetic_energy(mass, speed) = 1/2 * mass * speed^2
```

In the absence of any type annotations, this function has an overly generic type where
`mass` and `speed` can have arbitrary dimensions (but the return type is constrained
accordingly):
```nbt
fn kinetic_energy<A: Dim, B: Dim>(mass: A, speed: B) -> A × B² = …
```
In this example, it would be better to specify the types of `mass` and `speed`
explicitly (`Mass`, `Velocity`). The return type can then be inferred (`Energy`).
It is still valuable to specify it explicitly, in order to ensure there are no
mistakes in the function implementation.

## Generic functions

Sometimes it is useful to write generic functions. For example, consider
`max(a, b)` — a function that returns the larger of the two arguments. We might
want to use that function with *dimensionful* arguments such as `max(1 m, 1 yd)`.
To define such a generic function, you can introduce *type parameters* in angle
brackets:

```nbt
fn max<D: Dim>(a: D, b: D) -> D =
  if a > b then a else b
```

This function signature tells us that `max` takes two arguments of *arbitrary*
dimension type `D` (but they need to match!), and returns a quantity of the same
type `D`. The `D: Dim` syntax is a *type constraint* (or bound) that ensures that
`D` is a dimension type (`Scalar`, `Length`, `Velocity`, etc), and not something
like `Bool` or `DateTime`.

Note that you can perform the usual operations with (dimension) type parameters,
such as multiplying / dividing them with other types, or raising to rational powers.
For example, consider this cube-root function

```nbt
fn cube_root<T>(x: T^3) -> T = x^(1/3)
```

that can be called with a scalar (`cube_root(8) == 2`) or a dimensionful
argument (`cube_root(1 liter) == 10 cm`).

Note: `cube_root` can also be defined as `fn cube_root<T>(x: T) -> T^(1/3)`,
which is equivalent to the definition above.

Functions can also be generic over *all* types, not just dimension types. In this case,
no type constraints are needed. For example:
```nbt
fn second_element<A>(xs: List<A>) -> A =
  head(tail(xs))

second_element([10 cm, 2 m, 3 inch]) # returns 2 m
second_element(["a", "b", "c"])      # returns "b"
```

Note that the type annotations for all examples in this section are optional and
can also be inferred.

## Recursive functions

It is also possible to define recursive functions. For example, a naive
recursive implementation to compute Fibonacci numbers in Numbat looks like
this:

```nbt
fn fib(n) =
  if n ≤ 2
    then 1
    else fib(n - 2) + fib(n - 1)
```
