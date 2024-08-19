# Mathematical functions

[Basics](#basics) · [Transcendental functions](#transcendental-functions) · [Trigonometry](#trigonometry) · [Statistics](#statistics) · [Random sampling, distributions](#random-sampling-distributions) · [Number theory](#number-theory) · [Numerical methods](#numerical-methods) · [Geometry](#geometry) · [Algebra](#algebra) · [Trigonometry (extra)](#trigonometry-(extra))

## Basics

Defined in: `core::functions`

### `id` (Identity function)
Return the input value.

```nbt
fn id<A>(x: A) -> A
```

### `abs` (Absolute value)
Return the absolute value \\( |x| \\) of the input. This works for quantities, too: `abs(-5 m) = 5 m`.
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.abs).

```nbt
fn abs<T: Dim>(x: T) -> T
```

### `sqrt` (Square root)
Return the square root \\( \sqrt{x} \\) of the input: `sqrt(121 m^2) = 11 m`.
More information [here](https://en.wikipedia.org/wiki/Square_root).

```nbt
fn sqrt<D: Dim>(x: D^2) -> D
```

### `cbrt` (Cube root)
Return the cube root \\( \sqrt[3]{x} \\) of the input: `cbrt(8 m^3) = 2 m`.
More information [here](https://en.wikipedia.org/wiki/Cube_root).

```nbt
fn cbrt<D: Dim>(x: D^3) -> D
```

### `sqr` (Square function)
Return the square of the input, \\( x^2 \\): `sqr(5 m) = 25 m^2`.

```nbt
fn sqr<D: Dim>(x: D) -> D^2
```

### `round` (Rounding)
Round to the nearest integer. If the value is half-way between two integers, round away from \\( 0 \\). See also: `round_in`.
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.round).

```nbt
fn round(x: Scalar) -> Scalar
```

### `round_in` (Rounding)
Round to the nearest multiple of `base`. For example: `round_in(m, 5.3 m) == 5 m`.

```nbt
fn round_in<D: Dim>(base: D, value: D) -> D
```

### `floor` (Floor function)
Returns the largest integer less than or equal to \\( x \\). See also: `floor_in`.
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.floor).

```nbt
fn floor(x: Scalar) -> Scalar
```

### `floor_in` (Floor function)
Returns the largest integer multiple of `base` less than or equal to `value`. For example: `floor_in(m, 5.7 m) == 5 m`.

```nbt
fn floor_in<D: Dim>(base: D, value: D) -> D
```

### `ceil` (Ceil function)
Returns the smallest integer greater than or equal to \\( x \\). See also: `ceil_in`.
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.ceil).

```nbt
fn ceil(x: Scalar) -> Scalar
```

### `ceil_in` (Ceil function)
Returns the smallest integer multuple of `base` greater than or equal to `value`. For example: `ceil_in(m, 5.3 m) == 6 m`.

```nbt
fn ceil_in<D: Dim>(base: D, value: D) -> D
```

### `trunc` (Truncation)
Returns the integer part of \\( x \\). Non-integer numbers are always truncated towards zero. See also: `trunc_in`.
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.trunc).

```nbt
fn trunc(x: Scalar) -> Scalar
```

### `trunc_in` (Truncation)
Truncates to an integer multiple of `base` (towards zero). For example: `trunc_in(m, -5.7 m) == -5 m`.

```nbt
fn trunc_in<D: Dim>(base: D, value: D) -> D
```

### `mod` (Modulo)
Calculates the least nonnegative remainder of \\( a (\mod b) \\).
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.rem_euclid).

```nbt
fn mod<T: Dim>(a: T, b: T) -> T
```

## Transcendental functions

Defined in: `math::transcendental`

### `exp` (Exponential function)
The exponential function, \\( e^x \\).
More information [here](https://en.wikipedia.org/wiki/Exponential_function).

```nbt
fn exp(x: Scalar) -> Scalar
```

### `ln` (Natural logarithm)
The natural logarithm with base \\( e \\).
More information [here](https://en.wikipedia.org/wiki/Natural_logarithm).

```nbt
fn ln(x: Scalar) -> Scalar
```

### `log` (Natural logarithm)
The natural logarithm with base \\( e \\).
More information [here](https://en.wikipedia.org/wiki/Natural_logarithm).

```nbt
fn log(x: Scalar) -> Scalar
```

### `log10` (Common logarithm)
The common logarithm with base \\( 10 \\).
More information [here](https://en.wikipedia.org/wiki/Common_logarithm).

```nbt
fn log10(x: Scalar) -> Scalar
```

### `log2` (Binary logarithm)
The binary logarithm with base \\( 2 \\).
More information [here](https://en.wikipedia.org/wiki/Binary_logarithm).

```nbt
fn log2(x: Scalar) -> Scalar
```

### `gamma` (Gamma function)
The gamma function, \\( \Gamma(x) \\).
More information [here](https://en.wikipedia.org/wiki/Gamma_function).

```nbt
fn gamma(x: Scalar) -> Scalar
```

## Trigonometry

Defined in: `math::trigonometry`

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

## Statistics

Defined in: `math::statistics`

### `maximum` (Maxmimum)
Get the largest element of a list: `maximum([30 cm, 2 m]) = 2 m`.

```nbt
fn maximum<D: Dim>(xs: List<D>) -> D
```

### `minimum` (Minimum)
Get the smallest element of a list: `minimum([30 cm, 2 m]) = 30 cm`.

```nbt
fn minimum<D: Dim>(xs: List<D>) -> D
```

### `mean` (Arithmetic mean)
Calculate the arithmetic mean of a list of quantities: `mean([1 m, 2 m, 300 cm]) = 2 m`.
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

## Random sampling, distributions

Defined in: `core::random`, `math::distributions`

### `random` (Standard uniform distribution sampling)
Uniformly samples the interval \\( [0,1) \\).

```nbt
fn random() -> Scalar
```

### `rand_uniform` (Continuous uniform distribution sampling)
Uniformly samples the interval \\( [a,b) \\) if \\( a \le b \\) or \\( [b,a) \\) if \\( b<a \\) using inversion sampling.
More information [here](https://en.wikipedia.org/wiki/Continuous_uniform_distribution).

```nbt
fn rand_uniform<T: Dim>(a: T, b: T) -> T
```

### `rand_int` (Discrete uniform distribution sampling)
Uniformly samples integers from the interval \\( [a, b] \\).
More information [here](https://en.wikipedia.org/wiki/Discrete_uniform_distribution).

```nbt
fn rand_int(a: Scalar, b: Scalar) -> Scalar
```

### `rand_bernoulli` (Bernoulli distribution sampling)
Samples a Bernoulli random variable. That is, \\( 1 \\) with probability \\( p \\) and \\( 0 \\) with probability \\( 1-p \\). The parameter \\( p \\) must be a probability (\\( 0 \le p \le 1 \\)).
More information [here](https://en.wikipedia.org/wiki/Bernoulli_distribution).

```nbt
fn rand_bernoulli(p: Scalar) -> Scalar
```

### `rand_binom` (Binomial distribution sampling)
Samples a binomial distribution by doing \\( n \\) Bernoulli trials with probability \\( p \\).
              The parameter \\( n \\) must be a positive integer, the parameter \\( p \\) must be a probability (\\( 0 \le p \le 1 \\)).
More information [here](https://en.wikipedia.org/wiki/Binomial_distribution).

```nbt
fn rand_binom(n: Scalar, p: Scalar) -> Scalar
```

### `rand_norm` (Normal distribution sampling)
Samples a normal distribution with mean \\( \mu \\) and standard deviation \\( \sigma \\) using the Box-Muller transform.
More information [here](https://en.wikipedia.org/wiki/Normal_distribution).

```nbt
fn rand_norm<T: Dim>(μ: T, σ: T) -> T
```

### `rand_geom` (Geometric distribution sampling)
Samples a geometric distribution (the distribution of the number of Bernoulli trials with probability \\( p \\) needed to get one success) by inversion sampling. The parameter \\( p \\) must be a probability (\\( 0 \le p \le 1 \\)).
More information [here](https://en.wikipedia.org/wiki/Geometric_distribution).

```nbt
fn rand_geom(p: Scalar) -> Scalar
```

### `rand_poisson` (Poisson distribution sampling)
Sampling a poisson distribution with rate \\( \lambda \\), that is, the distribution of the number of events occurring in a fixed interval if these events occur with mean rate \\( \lambda \\). The rate parameter \\( \lambda \\) must be non-negative.
More information [here](https://en.wikipedia.org/wiki/Poisson_distribution).

```nbt
fn rand_poisson(λ: Scalar) -> Scalar
```

### `rand_expon` (Exponential distribution sampling)
Sampling an exponential distribution (the distribution of the distance between events in a Poisson process with rate \\( \lambda \\)) using inversion sampling. The rate parameter \\( \lambda \\) must be positive.
More information [here](https://en.wikipedia.org/wiki/Exponential_distribution).

```nbt
fn rand_expon<T: Dim>(λ: T) -> 1 / T
```

### `rand_lognorm` (Log-normal distribution sampling)
Sampling a log-normal distribution, that is, a distribution whose logarithm is a normal distribution with mean \\( \mu \\) and standard deviation \\( \sigma \\).
More information [here](https://en.wikipedia.org/wiki/Log-normal_distribution).

```nbt
fn rand_lognorm(μ: Scalar, σ: Scalar) -> Scalar
```

### `rand_pareto` (Pareto distribution sampling)
Sampling a Pareto distribution with minimum value `min` and shape parameter \\( \alpha \\) using inversion sampling. Both parameters must be positive.
More information [here](https://en.wikipedia.org/wiki/Pareto_distribution).

```nbt
fn rand_pareto<T: Dim>(α: Scalar, min: T) -> T
```

## Number theory

Defined in: `math::number_theory`

### `gcd` (Greatest common divisor)
The largest positive integer that divides each of the integers \\( a \\) and \\( b \\).
More information [here](https://en.wikipedia.org/wiki/Greatest_common_divisor).

```nbt
fn gcd(a: Scalar, b: Scalar) -> Scalar
```

### `lcm` (Least common multiple)
The smallest positive integer that is divisible by both \\( a \\) and \\( b \\).
More information [here](https://en.wikipedia.org/wiki/Least_common_multiple).

```nbt
fn lcm(a: Scalar, b: Scalar) -> Scalar
```

## Numerical methods

Defined in: `numerics::diff`, `numerics::solve`, `numerics::fixed_point`

### `diff` (Numerical differentiation)
Compute the numerical derivative of the function \\( f \\) at point \\( x \\) using the central difference method.
More information [here](https://en.wikipedia.org/wiki/Numerical_differentiation).

```nbt
fn diff<X: Dim, Y: Dim>(f: Fn[(X) -> Y], x: X) -> Y / X
```

### `root_bisect` (Bisection method)
Find the root of the function \\( f \\) in the interval \\( [x_1, x_2] \\) using the bisection method. The function \\( f \\) must be continuous and \\( f(x_1) \cdot f(x_2) < 0 \\).
More information [here](https://en.wikipedia.org/wiki/Bisection_method).

```nbt
fn root_bisect<X: Dim, Y: Dim>(f: Fn[(X) -> Y], x1: X, x2: X, x_tol: X, y_tol: Y) -> X
```

### `root_newton` (Newton's method)
Find the root of the function \\( f(x) \\) and its derivative \\( f'(x) \\) using Newton's method.
More information [here](https://en.wikipedia.org/wiki/Newton%27s_method).

```nbt
fn root_newton<X: Dim, Y: Dim>(f: Fn[(X) -> Y], f_prime: Fn[(X) -> Y / X], x0: X, y_tol: Y) -> X
```

### `fixed_point` (Fixed-point iteration)
Compute the approximate fixed point of a function \\( f: X \rightarrow X \\) starting from \\( x_0 \\), until \\( |f(x) - x| < ε \\).
More information [here](https://en.wikipedia.org/wiki/Fixed-point_iteration).

```nbt
fn fixed_point<X: Dim>(f: Fn[(X) -> X], x0: X, ε: X) -> X
```

## Geometry

Defined in: `math::geometry`

### `hypot2`
The length of the hypotenuse of a right-angled triangle \\( \sqrt{x^2+y^2} \\).

```nbt
fn hypot2<T: Dim>(x: T, y: T) -> T
```

### `hypot3`
The Euclidean norm of a 3D vector \\( \sqrt{x^2+y^2+z^2} \\).

```nbt
fn hypot3<T: Dim>(x: T, y: T, z: T) -> T
```

### `circle_area`
The area of a circle, \\( \pi r^2 \\).

```nbt
fn circle_area<L: Dim>(radius: L) -> L^2
```

### `circle_circumference`
The circumference of a circle, \\( 2\pi r \\).

```nbt
fn circle_circumference<L: Dim>(radius: L) -> L
```

### `sphere_area`
The surface area of a sphere, \\( 4\pi r^2 \\).

```nbt
fn sphere_area<L: Dim>(radius: L) -> L^2
```

### `sphere_volume`
The volume of a sphere, \\( \frac{4}{3}\pi r^3 \\).

```nbt
fn sphere_volume<L: Dim>(radius: L) -> L^3
```

## Algebra

Defined in: `extra::algebra`

### `quadratic_equation` (Solve quadratic equations)
Returns the solutions of the equation a x² + b x + c = 0.
More information [here](https://en.wikipedia.org/wiki/Quadratic_equation).

```nbt
fn quadratic_equation<A: Dim, B: Dim>(a: A, b: B, c: B^2 / A) -> List<B / A>
```

## Trigonometry (extra)

Defined in: `math::trigonometry_extra`

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

