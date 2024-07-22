# Mathematical functions

Jump to: [Basics](#basics) · [Trigonometry](#trigonometry) · [Random numbers](#random-numbers) · [Numerical methods](#numerical-methods) · [Algebra](#algebra)

## Basics

Defined in: `core::functions`

### `id` (Identity function)

```nbt
fn id<A>(x: A) -> A
```

### `abs` (Absolute value)
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.abs).

```nbt
fn abs<T: Dim>(x: T) -> T
```

### `round` (Round)
Round to the nearest integer.
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.round).

```nbt
fn round<T: Dim>(x: T) -> T
```

### `floor` (Floor function)
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.floor).

```nbt
fn floor<T: Dim>(x: T) -> T
```

### `ceil` (Ceil function)
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.ceil).

```nbt
fn ceil<T: Dim>(x: T) -> T
```

### `mod` (Modulo)
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.rem_euclid).

```nbt
fn mod<T: Dim>(a: T, b: T) -> T
```

## Trigonometry

Defined in: `math::functions`, `math::trigonometry_extra`

### `is_nan`

```nbt
fn is_nan<T: Dim>(n: T) -> Bool
```

### `is_infinite`

```nbt
fn is_infinite<T: Dim>(n: T) -> Bool
```

### `sqrt` (Square root)
More information [here](https://en.wikipedia.org/wiki/Square_root).

```nbt
fn sqrt<D: Dim>(x: D^2) -> D
```

### `sqr` (Square function)

```nbt
fn sqr<D: Dim>(x: D) -> D^2
```

### `exp` (Exponential function)
More information [here](https://en.wikipedia.org/wiki/Exponential_function).

```nbt
fn exp(x: Scalar) -> Scalar
```

### `ln` (Natural logarithm)
More information [here](https://en.wikipedia.org/wiki/Natural_logarithm).

```nbt
fn ln(x: Scalar) -> Scalar
```

### `log` (Natural logarithm)
More information [here](https://en.wikipedia.org/wiki/Natural_logarithm).

```nbt
fn log(x: Scalar) -> Scalar
```

### `log10` (Common logarithm)
More information [here](https://en.wikipedia.org/wiki/Common_logarithm).

```nbt
fn log10(x: Scalar) -> Scalar
```

### `log2` (Binary logarithm)
More information [here](https://en.wikipedia.org/wiki/Binary_logarithm).

```nbt
fn log2(x: Scalar) -> Scalar
```

### `sin` (Sine)
More information [here](https://en.wikipedia.org/wiki/Trigonometric_functions).

```nbt
fn sin(x: Scalar) -> Scalar
```

### `cos` (Cosine)
More information [here](https://en.wikipedia.org/wiki/Trigonometric_functions).

```nbt
fn cos(x: Scalar) -> Scalar
```

### `tan` (Tangent)
More information [here](https://en.wikipedia.org/wiki/Trigonometric_functions).

```nbt
fn tan(x: Scalar) -> Scalar
```

### `asin` (Arc sine)
More information [here](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions).

```nbt
fn asin(x: Scalar) -> Scalar
```

### `acos` (Arc cosine)
More information [here](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions).

```nbt
fn acos(x: Scalar) -> Scalar
```

### `atan` (Arc tangent)
More information [here](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions).

```nbt
fn atan(x: Scalar) -> Scalar
```

### `atan2`
More information [here](https://en.wikipedia.org/wiki/Atan2).

```nbt
fn atan2<T: Dim>(y: T, x: T) -> Scalar
```

### `sinh` (Hyperbolic sine)
More information [here](https://en.wikipedia.org/wiki/Hyperbolic_functions).

```nbt
fn sinh(x: Scalar) -> Scalar
```

### `cosh` (Hyperbolic cosine)
More information [here](https://en.wikipedia.org/wiki/Hyperbolic_functions).

```nbt
fn cosh(x: Scalar) -> Scalar
```

### `tanh` (Hyperbolic tangent)
More information [here](https://en.wikipedia.org/wiki/Hyperbolic_functions).

```nbt
fn tanh(x: Scalar) -> Scalar
```

### `asinh` (Area hyperbolic sine)
More information [here](https://en.wikipedia.org/wiki/Hyperbolic_functions).

```nbt
fn asinh(x: Scalar) -> Scalar
```

### `acosh` (Area hyperbolic cosine)
More information [here](https://en.wikipedia.org/wiki/Hyperbolic_functions).

```nbt
fn acosh(x: Scalar) -> Scalar
```

### `atanh` (Area hyperbolic tangent )
More information [here](https://en.wikipedia.org/wiki/Hyperbolic_functions).

```nbt
fn atanh(x: Scalar) -> Scalar
```

### `gamma` (Gamma function)
More information [here](https://en.wikipedia.org/wiki/Gamma_function).

```nbt
fn gamma(x: Scalar) -> Scalar
```

### `maximum` (Maxmimum)
Get the largest element of a list.

```nbt
fn maximum<D: Dim>(xs: List<D>) -> D
```

### `minimum` (Minimum)
Get the smallest element of a list.

```nbt
fn minimum<D: Dim>(xs: List<D>) -> D
```

### `mean` (Arithmetic mean)
More information [here](https://en.wikipedia.org/wiki/Arithmetic_mean).

```nbt
fn mean<D: Dim>(xs: List<D>) -> D
```

### `variance` (Variance)
Calculate the population variance of a list of quantities.
More information [here](https://en.wikipedia.org/wiki/Variance).

```nbt
fn variance<D: Dim>(xs: List<D>) -> D^2
```

### `stdev` (Standard deviation)
Calculate the population standard deviation of a list of quantities.
More information [here](https://en.wikipedia.org/wiki/Standard_deviation).

```nbt
fn stdev<D: Dim>(xs: List<D>) -> D
```

### `median` (Median)
Calculate the median of a list of quantities.
More information [here](https://en.wikipedia.org/wiki/Median).

```nbt
fn median<D: Dim>(xs: List<D>) -> D
```

### `gcd` (Greatest common divisor)
More information [here](https://en.wikipedia.org/wiki/Greatest_common_divisor).

```nbt
fn gcd(a: Scalar, b: Scalar) -> Scalar
```

### `lcm` (Least common multiple)
More information [here](https://en.wikipedia.org/wiki/Least_common_multiple).

```nbt
fn lcm(a: Scalar, b: Scalar) -> Scalar
```

### `hypot2`

```nbt
fn hypot2<T: Dim>(x: T, y: T) -> T
```

### `hypot3`

```nbt
fn hypot3<T: Dim>(x: T, y: T, z: T) -> T
```

### `circle_area`

```nbt
fn circle_area<L: Dim>(radius: L) -> L^2
```

### `circle_circumference`

```nbt
fn circle_circumference<L: Dim>(radius: L) -> L
```

### `sphere_area`

```nbt
fn sphere_area<L: Dim>(radius: L) -> L^2
```

### `sphere_volume`

```nbt
fn sphere_volume<L: Dim>(radius: L) -> L^3
```

### `cot`

```nbt
fn cot(x: Scalar) -> Scalar
```

### `acot`

```nbt
fn acot(x: Scalar) -> Scalar
```

### `coth`

```nbt
fn coth(x: Scalar) -> Scalar
```

### `acoth`

```nbt
fn acoth(x: Scalar) -> Scalar
```

### `secant`

```nbt
fn secant(x: Scalar) -> Scalar
```

### `arcsecant`

```nbt
fn arcsecant(x: Scalar) -> Scalar
```

### `cosecant`

```nbt
fn cosecant(x: Scalar) -> Scalar
```

### `csc`

```nbt
fn csc(x: Scalar) -> Scalar
```

### `acsc`

```nbt
fn acsc(x: Scalar) -> Scalar
```

### `sech`

```nbt
fn sech(x: Scalar) -> Scalar
```

### `asech`

```nbt
fn asech(x: Scalar) -> Scalar
```

### `csch`

```nbt
fn csch(x: Scalar) -> Scalar
```

### `acsch`

```nbt
fn acsch(x: Scalar) -> Scalar
```

## Random numbers

Defined in: `core::random`, `math::statistics`

### `random` (Standard uniform distribution sampling)
Uniformly samples the interval [0,1).

```nbt
fn random() -> Scalar
```

### `rand_uniform` (Continuous uniform distribution sampling)
Uniformly samples the interval [a,b) if a<=b or [b,a) if b<a using inversion sampling.
More information [here](https://en.wikipedia.org/wiki/Continuous_uniform_distribution).

```nbt
fn rand_uniform<T: Dim>(a: T, b: T) -> T
```

### `rand_int` (Discrete uniform distribution sampling)
Uniformly samples the integers in the interval [a, b].
More information [here](https://en.wikipedia.org/wiki/Discrete_uniform_distribution).

```nbt
fn rand_int(a: Scalar, b: Scalar) -> Scalar
```

### `rand_bernoulli` (Bernoulli distribution sampling)
Samples a Bernoulli random variable, that is, 1 with probability p, 0 with probability 1-p.
              Parameter p must be a probability (0 <= p <= 1).
More information [here](https://en.wikipedia.org/wiki/Bernoulli_distribution).

```nbt
fn rand_bernoulli(p: Scalar) -> Scalar
```

### `rand_binom` (Binomial distribution sampling)
Samples a binomial distribution by doing n Bernoulli trials with probability p.
              Parameter n must be a positive integer, parameter p must be a probability (0 <= p <= 1).
More information [here](https://en.wikipedia.org/wiki/Binomial_distribution).

```nbt
fn rand_binom(n: Scalar, p: Scalar) -> Scalar
```

### `rand_norm` (Normal distribution sampling)
Samples a normal distribution with mean μ and standard deviation σ using the Box-Muller transform.
More information [here](https://en.wikipedia.org/wiki/Normal_distribution).

```nbt
fn rand_norm<T: Dim>(μ: T, σ: T) -> T
```

### `rand_geom` (Geometric distribution sampling)
Samples a geometric distribution (the distribution of the number of Bernoulli trials with probability p needed to get one success) by inversion sampling.
              Parameter p must be a probability (0 <= p <= 1).
More information [here](https://en.wikipedia.org/wiki/Geometric_distribution).

```nbt
fn rand_geom(p: Scalar) -> Scalar
```

### `rand_poisson` (Poisson distribution sampling)
Sampling a poisson distribution with rate λ, that is, the distribution of the number of events occurring in a fixed interval if these events occur with mean rate λ.
              The rate parameter λ must not be negative.
More information [here](https://en.wikipedia.org/wiki/Poisson_distribution).

```nbt
fn rand_poisson(λ: Scalar) -> Scalar
```

### `rand_expon` (Exponential distribution sampling)
Sampling an exponential distribution (the distribution of the distance between events in a Poisson process with rate λ) using inversion sampling.
              The rate parameter λ must be positive.
More information [here](https://en.wikipedia.org/wiki/Exponential_distribution).

```nbt
fn rand_expon<T: Dim>(λ: T) -> 1 / T
```

### `rand_lognorm` (Log-normal distribution sampling)
Sampling a log-normal distribution, that is, a distribution whose log is a normal distribution with mean μ and standard deviation σ.
More information [here](https://en.wikipedia.org/wiki/Log-normal_distribution).

```nbt
fn rand_lognorm(μ: Scalar, σ: Scalar) -> Scalar
```

### `rand_pareto` (Pareto distribution sampling)
Sampling a Pareto distribution with minimum value min and shape parameter α using inversion sampling.
              Both parameters α and min must be positive.
More information [here](https://en.wikipedia.org/wiki/Pareto_distribution).

```nbt
fn rand_pareto<T: Dim>(α: Scalar, min: T) -> T
```

## Numerical methods

Defined in: `numerics::diff`, `numerics::solve`

### `diff` (Numerical differentiation)
Compute the numerical derivative of a function at a point using the central difference method.
More information [here](https://en.wikipedia.org/wiki/Numerical_differentiation).

```nbt
fn diff<X: Dim, Y: Dim>(f: Fn[(X) -> Y], x: X) -> Y / X
```

### `root_bisect` (Bisection method)
Find the root of the function f in the interval [x1, x2] using the bisection method. The function f must be continuous and f(x1) × f(x2) < 0.
More information [here](https://en.wikipedia.org/wiki/Bisection_method).

```nbt
fn root_bisect<A: Dim, B: Dim>(f: Fn[(A) -> B], x1: A, x2: A, x_tolerance: A, y_tolerance: B) -> A
```

### `root_newton` (Newton's method)
Find the root of the function f(x) and its derivative f'(x) using Newton's method.
More information [here](https://en.wikipedia.org/wiki/Newton%27s_method).

```nbt
fn root_newton<A: Dim, B: Dim>(f: Fn[(A) -> B], f_prime: Fn[(A) -> B / A], x0: A, y_tolerance: B) -> A
```

## Algebra

Defined in: `extra::algebra`

### `quadratic_equation` (Solve quadratic equations)
Returns the solutions of the equation a x² + b x + c = 0.
More information [here](https://en.wikipedia.org/wiki/Quadratic_equation).

```nbt
fn quadratic_equation<A: Dim, B: Dim>(a: A, b: B, c: B^2 / A) -> List<B / A>
```

