# Function definitions

Numbat comes with a large number of [predefined functions](./predefined-functions.md), but
it is also possible to add new functions. A function definition is introduced with
the `fn` keyword:

```nbt
fn max_distance(v: Velocity, θ: Angle) -> Length = v² · sin(2 θ) / g0
```

This exemplary function computes the maximum distance of a projectile under the
influence of Earths gravity. It takes two parameters (The initial velocity `v` and
the launch angle `θ`), which are both annotated with their corresponding physical
dimension (their type). The function returns a distance, and so the return type
is specified as `Length`.

## Type inference

The return type annotation may be omitted, but it is often desirable to add it
for better readability of the code and in order to catch potential errors.

The parameter types can also (sometimes) be omitted, in which case Numbat tries
to infer their type. However, this often leads to overly generic function
signatures. For example, consider the following function to compute the kinetic
energy of a massive object in motion:

```nbt
fn kinetic_energy(mass, speed) = 1/2 * mass * speed^2
```

Without any type annotations, this function has an overly generic type where
`mass` and `speed` can have arbitrary dimensions (and the return type is
`type(mass) * type(speed)^2`). So for this case, it is probably better to add
parameter and return types.

## Generic functions

Sometimes however, it *is* useful to write generic functions. For example, consider
`max(a, b)` — a function that returns the larger of the two arguments. We might
want to use that function with *dimensionful* arguments such as `max(1 m, 1 yd)`.
To define such a generic function, you can introduce *type parameters* in angle
brackets:

```nbt
fn max<T>(a: T, b: T) -> T = if a > b then a else b
```

This function signature tells us that `max` takes two arguments of *arbitrary*
type `T` (but they need to match!), and returns a quantity of the same type `T`.

Note that you can perform the usual operations with type parameters, such as
multiplying/dividing them with other types, or raising to rational powers. For
example, consider this cube-root function

```nbt
fn cube_root<T>(x: T^3) -> T = x^(1/3)
```

that can be called with a scalar (`cube_root(8) == 2`) or a dimensionful
argument (`cube_root(1 liter) == 10 cm`).

Note: `cube_root` can also be defined as `fn cube_root<T>(x: T) -> T^(1/3)`,
which is equivalent to the definition above.

## Recursive functions

It is also possible to define recursive functions. In order to do so, you
currently need to specify the return type — as the type signature can not
(yet) be inferred otherwise.

For example, a naive recursive implementation to compute Fibonacci numbers
in Numbat looks like this:

```nbt
fn fib(n: Scalar) -> Scalar =
  if n ≤ 2
    then 1
    else fib(n - 2) + fib(n - 1)
```
