# Mathematical functions

[Basics](#basics) · [Transcendental functions](#transcendental-functions) · [Trigonometry](#trigonometry) · [Statistics](#statistics) · [Random sampling, distributions](#random-sampling-distributions) · [Number theory](#number-theory) · [Numerical methods](#numerical-methods) · [Geometry](#geometry) · [Algebra](#algebra) · [Trigonometry (extra)](#trigonometry-(extra))

## Basics

Defined in: `core::functions`

### `id` (Identity function)
Return the input value.

```nbt
fn id<A>(x: A) -> A
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=id%288kg%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> id(8kg)
    
      id(8 kilogram)
    
        = 8 kg    [Mass]
    
  ```
</details>

### `abs` (Absolute value)
Return the absolute value \\( |x| \\) of the input. This works for quantities, too: `abs(-5 m) = 5 m`.
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.abs).

```nbt
fn abs<T: Dim>(x: T) -> T
```

</details>

### `sqrt` (Square root)
Return the square root \\( \sqrt{x} \\) of the input: `sqrt(121 m^2) = 11 m`.
More information [here](https://en.wikipedia.org/wiki/Square_root).

```nbt
fn sqrt<D: Dim>(x: D^2) -> D
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=sqrt%284are%29%20%2D%3E%20m"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> sqrt(4are) -> m
    
      sqrt(4 are) ➞ metre
    
        = 20 m    [Length]
    
  ```
</details>

### `cbrt` (Cube root)
Return the cube root \\( \sqrt[3]{x} \\) of the input: `cbrt(8 m^3) = 2 m`.
More information [here](https://en.wikipedia.org/wiki/Cube_root).

```nbt
fn cbrt<D: Dim>(x: D^3) -> D
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=cbrt%288l%29%20%2D%3E%20cm"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> cbrt(8l) -> cm
    
      cbrt(8 litre) ➞ centimetre
    
        = 20.0 cm    [Length]
    
  ```
</details>

### `sqr` (Square function)
Return the square of the input, \\( x^2 \\): `sqr(5 m) = 25 m^2`.

```nbt
fn sqr<D: Dim>(x: D) -> D^2
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=sqr%287%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> sqr(7)
    
      sqr(7)
    
        = 49
    
  ```
</details>

### `round` (Rounding)
Round to the nearest integer. If the value is half-way between two integers, round away from \\( 0 \\). See also: `round_in`.
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.round).

```nbt
fn round(x: Scalar) -> Scalar
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=round%285%2E5%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> round(5.5)
    
      round(5.5)
    
        = 6
    
  ```
* <a href="https://numbat.dev/?q=round%28%2D5%2E5%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> round(-5.5)
    
      round(-5.5)
    
        = -6
    
  ```
</details>

### `round_in` (Rounding)
Round to the nearest multiple of `base`.

```nbt
fn round_in<D: Dim>(base: D, value: D) -> D
```

<details>
<summary>Examples</summary>

* Round in meters.

  <a href="https://numbat.dev/?q=round%5Fin%28m%2C%205%2E3%20m%29"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> round_in(m, 5.3 m)
    
      round_in(metre, 5.3 metre)
    
        = 5 m    [Length]
    
  ```
* Round in centimeters.

  <a href="https://numbat.dev/?q=round%5Fin%28cm%2C%205%2E3%20m%29"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> round_in(cm, 5.3 m)
    
      round_in(centimetre, 5.3 metre)
    
        = 530 cm    [Length]
    
  ```
</details>

### `floor` (Floor function)
Returns the largest integer less than or equal to \\( x \\). See also: `floor_in`.
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.floor).

```nbt
fn floor(x: Scalar) -> Scalar
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=floor%285%2E5%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> floor(5.5)
    
      floor(5.5)
    
        = 5
    
  ```
</details>

### `floor_in` (Floor function)
Returns the largest integer multiple of `base` less than or equal to `value`.

```nbt
fn floor_in<D: Dim>(base: D, value: D) -> D
```

<details>
<summary>Examples</summary>

* Floor in meters.

  <a href="https://numbat.dev/?q=floor%5Fin%28m%2C%205%2E7%20m%29"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> floor_in(m, 5.7 m)
    
      floor_in(metre, 5.7 metre)
    
        = 5 m    [Length]
    
  ```
* Floor in centimeters.

  <a href="https://numbat.dev/?q=floor%5Fin%28cm%2C%205%2E7%20m%29"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> floor_in(cm, 5.7 m)
    
      floor_in(centimetre, 5.7 metre)
    
        = 570 cm    [Length]
    
  ```
</details>

### `ceil` (Ceil function)
Returns the smallest integer greater than or equal to \\( x \\). See also: `ceil_in`.
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.ceil).

```nbt
fn ceil(x: Scalar) -> Scalar
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=ceil%285%2E5%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> ceil(5.5)
    
      ceil(5.5)
    
        = 6
    
  ```
</details>

### `ceil_in` (Ceil function)
Returns the smallest integer multuple of `base` greater than or equal to `value`.

```nbt
fn ceil_in<D: Dim>(base: D, value: D) -> D
```

<details>
<summary>Examples</summary>

* Ceil in meters.

  <a href="https://numbat.dev/?q=ceil%5Fin%28m%2C%205%2E3%20m%29"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> ceil_in(m, 5.3 m)
    
      ceil_in(metre, 5.3 metre)
    
        = 6 m    [Length]
    
  ```
* Ceil in centimeters.

  <a href="https://numbat.dev/?q=ceil%5Fin%28cm%2C%205%2E3%20m%29"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> ceil_in(cm, 5.3 m)
    
      ceil_in(centimetre, 5.3 metre)
    
        = 530 cm    [Length]
    
  ```
</details>

### `trunc` (Truncation)
Returns the integer part of \\( x \\). Non-integer numbers are always truncated towards zero. See also: `trunc_in`.
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.trunc).

```nbt
fn trunc(x: Scalar) -> Scalar
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=trunc%285%2E5%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> trunc(5.5)
    
      trunc(5.5)
    
        = 5
    
  ```
* <a href="https://numbat.dev/?q=trunc%28%2D5%2E5%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> trunc(-5.5)
    
      trunc(-5.5)
    
        = -5
    
  ```
</details>

### `trunc_in` (Truncation)
Truncates to an integer multiple of `base` (towards zero).

```nbt
fn trunc_in<D: Dim>(base: D, value: D) -> D
```

<details>
<summary>Examples</summary>

* Truncate in meters.

  <a href="https://numbat.dev/?q=trunc%5Fin%28m%2C%205%2E7%20m%29"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> trunc_in(m, 5.7 m)
    
      trunc_in(metre, 5.7 metre)
    
        = 5 m    [Length]
    
  ```
* Truncate in centimeters.

  <a href="https://numbat.dev/?q=trunc%5Fin%28cm%2C%205%2E7%20m%29"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> trunc_in(cm, 5.7 m)
    
      trunc_in(centimetre, 5.7 metre)
    
        = 570 cm    [Length]
    
  ```
</details>

### `mod` (Modulo)
Calculates the least nonnegative remainder of \\( a (\mod b) \\).
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.rem_euclid).

```nbt
fn mod<T: Dim>(a: T, b: T) -> T
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=mod%2827%2C%205%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> mod(27, 5)
    
      mod(27, 5)
    
        = 2
    
  ```
</details>

## Transcendental functions

Defined in: `math::transcendental`

### `exp` (Exponential function)
The exponential function, \\( e^x \\).
More information [here](https://en.wikipedia.org/wiki/Exponential_function).

```nbt
fn exp(x: Scalar) -> Scalar
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=exp%284%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> exp(4)
    
      exp(4)
    
        = 54.5982
    
  ```
</details>

### `ln` (Natural logarithm)
The natural logarithm with base \\( e \\).
More information [here](https://en.wikipedia.org/wiki/Natural_logarithm).

```nbt
fn ln(x: Scalar) -> Scalar
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=ln%2820%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> ln(20)
    
      ln(20)
    
        = 2.99573
    
  ```
</details>

### `log` (Natural logarithm)
The natural logarithm with base \\( e \\).
More information [here](https://en.wikipedia.org/wiki/Natural_logarithm).

```nbt
fn log(x: Scalar) -> Scalar
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=log%2820%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> log(20)
    
      log(20)
    
        = 2.99573
    
  ```
</details>

### `log10` (Common logarithm)
The common logarithm with base \\( 10 \\).
More information [here](https://en.wikipedia.org/wiki/Common_logarithm).

```nbt
fn log10(x: Scalar) -> Scalar
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=log10%28100%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> log10(100)
    
      log10(100)
    
        = 2
    
  ```
</details>

### `log2` (Binary logarithm)
The binary logarithm with base \\( 2 \\).
More information [here](https://en.wikipedia.org/wiki/Binary_logarithm).

```nbt
fn log2(x: Scalar) -> Scalar
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=log2%28256%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> log2(256)
    
      log2(256)
    
        = 8
    
  ```
</details>

### `gamma` (Gamma function)
The gamma function, \\( \Gamma(x) \\).
More information [here](https://en.wikipedia.org/wiki/Gamma_function).

```nbt
fn gamma(x: Scalar) -> Scalar
```

</details>

## Trigonometry

Defined in: `math::trigonometry`

### `sin` (Sine)
More information [here](https://en.wikipedia.org/wiki/Trigonometric_functions).

```nbt
fn sin(x: Scalar) -> Scalar
```

</details>

### `cos` (Cosine)
More information [here](https://en.wikipedia.org/wiki/Trigonometric_functions).

```nbt
fn cos(x: Scalar) -> Scalar
```

</details>

### `tan` (Tangent)
More information [here](https://en.wikipedia.org/wiki/Trigonometric_functions).

```nbt
fn tan(x: Scalar) -> Scalar
```

</details>

### `asin` (Arc sine)
More information [here](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions).

```nbt
fn asin(x: Scalar) -> Scalar
```

</details>

### `acos` (Arc cosine)
More information [here](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions).

```nbt
fn acos(x: Scalar) -> Scalar
```

</details>

### `atan` (Arc tangent)
More information [here](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions).

```nbt
fn atan(x: Scalar) -> Scalar
```

</details>

### `atan2`
More information [here](https://en.wikipedia.org/wiki/Atan2).

```nbt
fn atan2<T: Dim>(y: T, x: T) -> Scalar
```

</details>

### `sinh` (Hyperbolic sine)
More information [here](https://en.wikipedia.org/wiki/Hyperbolic_functions).

```nbt
fn sinh(x: Scalar) -> Scalar
```

</details>

### `cosh` (Hyperbolic cosine)
More information [here](https://en.wikipedia.org/wiki/Hyperbolic_functions).

```nbt
fn cosh(x: Scalar) -> Scalar
```

</details>

### `tanh` (Hyperbolic tangent)
More information [here](https://en.wikipedia.org/wiki/Hyperbolic_functions).

```nbt
fn tanh(x: Scalar) -> Scalar
```

</details>

### `asinh` (Area hyperbolic sine)
More information [here](https://en.wikipedia.org/wiki/Hyperbolic_functions).

```nbt
fn asinh(x: Scalar) -> Scalar
```

</details>

### `acosh` (Area hyperbolic cosine)
More information [here](https://en.wikipedia.org/wiki/Hyperbolic_functions).

```nbt
fn acosh(x: Scalar) -> Scalar
```

</details>

### `atanh` (Area hyperbolic tangent )
More information [here](https://en.wikipedia.org/wiki/Hyperbolic_functions).

```nbt
fn atanh(x: Scalar) -> Scalar
```

</details>

## Statistics

Defined in: `math::statistics`

### `maximum` (Maxmimum)
Get the largest element of a list.

```nbt
fn maximum<D: Dim>(xs: List<D>) -> D
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=maximum%28%5B30%20cm%2C%202%20m%5D%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> maximum([30 cm, 2 m])
    
      maximum([30 centimetre, 2 metre])
    
        = 2 m    [Length]
    
  ```
</details>

### `minimum` (Minimum)
Get the smallest element of a list.

```nbt
fn minimum<D: Dim>(xs: List<D>) -> D
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=minimum%28%5B30%20cm%2C%202%20m%5D%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> minimum([30 cm, 2 m])
    
      minimum([30 centimetre, 2 metre])
    
        = 30 cm    [Length]
    
  ```
</details>

### `mean` (Arithmetic mean)
Calculate the arithmetic mean of a list of quantities.
More information [here](https://en.wikipedia.org/wiki/Arithmetic_mean).

```nbt
fn mean<D: Dim>(xs: List<D>) -> D
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=mean%28%5B1%20m%2C%202%20m%2C%20300%20cm%5D%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> mean([1 m, 2 m, 300 cm])
    
      mean([1 metre, 2 metre, 300 centimetre])
    
        = 2 m    [Length]
    
  ```
</details>

### `variance` (Variance)
Calculate the population variance of a list of quantities.
More information [here](https://en.wikipedia.org/wiki/Variance).

```nbt
fn variance<D: Dim>(xs: List<D>) -> D^2
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=variance%28%5B1%20m%2C%202%20m%2C%20300%20cm%5D%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> variance([1 m, 2 m, 300 cm])
    
      variance([1 metre, 2 metre, 300 centimetre])
    
        = 0.666667 m²    [Area]
    
  ```
</details>

### `stdev` (Standard deviation)
Calculate the population standard deviation of a list of quantities.
More information [here](https://en.wikipedia.org/wiki/Standard_deviation).

```nbt
fn stdev<D: Dim>(xs: List<D>) -> D
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=stdev%28%5B1%20m%2C%202%20m%2C%20300%20cm%5D%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> stdev([1 m, 2 m, 300 cm])
    
      stdev([1 metre, 2 metre, 300 centimetre])
    
        = 0.816497 m    [Length]
    
  ```
</details>

### `median` (Median)
Calculate the median of a list of quantities.
More information [here](https://en.wikipedia.org/wiki/Median).

```nbt
fn median<D: Dim>(xs: List<D>) -> D
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=median%28%5B1%20m%2C%202%20m%2C%20400%20cm%5D%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> median([1 m, 2 m, 400 cm])
    
      median([1 metre, 2 metre, 400 centimetre])
    
        = 2 m    [Length]
    
  ```
</details>

## Random sampling, distributions

Defined in: `core::random`, `math::distributions`

### `random` (Standard uniform distribution sampling)
Uniformly samples the interval \\( [0,1) \\).

```nbt
fn random() -> Scalar
```

</details>

### `rand_uniform` (Continuous uniform distribution sampling)
Uniformly samples the interval \\( [a,b) \\) if \\( a \le b \\) or \\( [b,a) \\) if \\( b<a \\) using inversion sampling.
More information [here](https://en.wikipedia.org/wiki/Continuous_uniform_distribution).

```nbt
fn rand_uniform<T: Dim>(a: T, b: T) -> T
```

</details>

### `rand_int` (Discrete uniform distribution sampling)
Uniformly samples integers from the interval \\( [a, b] \\).
More information [here](https://en.wikipedia.org/wiki/Discrete_uniform_distribution).

```nbt
fn rand_int(a: Scalar, b: Scalar) -> Scalar
```

</details>

### `rand_bernoulli` (Bernoulli distribution sampling)
Samples a Bernoulli random variable. That is, \\( 1 \\) with probability \\( p \\) and \\( 0 \\) with probability \\( 1-p \\). The parameter \\( p \\) must be a probability (\\( 0 \le p \le 1 \\)).
More information [here](https://en.wikipedia.org/wiki/Bernoulli_distribution).

```nbt
fn rand_bernoulli(p: Scalar) -> Scalar
```

</details>

### `rand_binom` (Binomial distribution sampling)
Samples a binomial distribution by doing \\( n \\) Bernoulli trials with probability \\( p \\).
              The parameter \\( n \\) must be a positive integer, the parameter \\( p \\) must be a probability (\\( 0 \le p \le 1 \\)).
More information [here](https://en.wikipedia.org/wiki/Binomial_distribution).

```nbt
fn rand_binom(n: Scalar, p: Scalar) -> Scalar
```

</details>

### `rand_norm` (Normal distribution sampling)
Samples a normal distribution with mean \\( \mu \\) and standard deviation \\( \sigma \\) using the Box-Muller transform.
More information [here](https://en.wikipedia.org/wiki/Normal_distribution).

```nbt
fn rand_norm<T: Dim>(μ: T, σ: T) -> T
```

</details>

### `rand_geom` (Geometric distribution sampling)
Samples a geometric distribution (the distribution of the number of Bernoulli trials with probability \\( p \\) needed to get one success) by inversion sampling. The parameter \\( p \\) must be a probability (\\( 0 \le p \le 1 \\)).
More information [here](https://en.wikipedia.org/wiki/Geometric_distribution).

```nbt
fn rand_geom(p: Scalar) -> Scalar
```

</details>

### `rand_poisson` (Poisson distribution sampling)
Sampling a poisson distribution with rate \\( \lambda \\), that is, the distribution of the number of events occurring in a fixed interval if these events occur with mean rate \\( \lambda \\). The rate parameter \\( \lambda \\) must be non-negative.
More information [here](https://en.wikipedia.org/wiki/Poisson_distribution).

```nbt
fn rand_poisson(λ: Scalar) -> Scalar
```

</details>

### `rand_expon` (Exponential distribution sampling)
Sampling an exponential distribution (the distribution of the distance between events in a Poisson process with rate \\( \lambda \\)) using inversion sampling. The rate parameter \\( \lambda \\) must be positive.
More information [here](https://en.wikipedia.org/wiki/Exponential_distribution).

```nbt
fn rand_expon<T: Dim>(λ: T) -> 1 / T
```

</details>

### `rand_lognorm` (Log-normal distribution sampling)
Sampling a log-normal distribution, that is, a distribution whose logarithm is a normal distribution with mean \\( \mu \\) and standard deviation \\( \sigma \\).
More information [here](https://en.wikipedia.org/wiki/Log-normal_distribution).

```nbt
fn rand_lognorm(μ: Scalar, σ: Scalar) -> Scalar
```

</details>

### `rand_pareto` (Pareto distribution sampling)
Sampling a Pareto distribution with minimum value `min` and shape parameter \\( \alpha \\) using inversion sampling. Both parameters must be positive.
More information [here](https://en.wikipedia.org/wiki/Pareto_distribution).

```nbt
fn rand_pareto<T: Dim>(α: Scalar, min: T) -> T
```

</details>

## Number theory

Defined in: `math::number_theory`

### `gcd` (Greatest common divisor)
The largest positive integer that divides each of the integers \\( a \\) and \\( b \\).
More information [here](https://en.wikipedia.org/wiki/Greatest_common_divisor).

```nbt
fn gcd(a: Scalar, b: Scalar) -> Scalar
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=gcd%2860%2C42%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> gcd(60,42)
    
      gcd(60, 42)
    
        = 6
    
  ```
</details>

### `lcm` (Least common multiple)
The smallest positive integer that is divisible by both \\( a \\) and \\( b \\).
More information [here](https://en.wikipedia.org/wiki/Least_common_multiple).

```nbt
fn lcm(a: Scalar, b: Scalar) -> Scalar
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=lcm%2814%2C%204%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> lcm(14, 4)
    
      lcm(14, 4)
    
        = 28
    
  ```
</details>

## Numerical methods

Defined in: `numerics::diff`, `numerics::solve`, `numerics::fixed_point`

### `diff` (Numerical differentiation)
Compute the numerical derivative of the function \\( f \\) at point \\( x \\) using the central difference method.
More information [here](https://en.wikipedia.org/wiki/Numerical_differentiation).

```nbt
fn diff<X: Dim, Y: Dim>(f: Fn[(X) -> Y], x: X) -> Y / X
```

<details>
<summary>Examples</summary>

* Compute the drivative of \\( f(x) = x² -x -1 \\) at \\( x=1 \\).

  <a href="https://numbat.dev/?q=use%20numerics%3A%3Adiff%0Afn%20polynomial%28x%29%20%3D%20x%C2%B2%20%2Dx%20%2D1%0Adiff%28polynomial%2C%201%29"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> fn polynomial(x) = x² -x -1
    diff(polynomial, 1)
    
      fn polynomial(x: Scalar) -> Scalar = (x² - x) - 1
    
      diff(polynomial, 1)
    
        = 1.0
    
  ```
</details>

### `root_bisect` (Bisection method)
Find the root of the function \\( f \\) in the interval \\( [x_1, x_2] \\) using the bisection method. The function \\( f \\) must be continuous and \\( f(x_1) \cdot f(x_2) < 0 \\).
More information [here](https://en.wikipedia.org/wiki/Bisection_method).

```nbt
fn root_bisect<X: Dim, Y: Dim>(f: Fn[(X) -> Y], x1: X, x2: X, x_tol: X, y_tol: Y) -> X
```

<details>
<summary>Examples</summary>

* Find the root of \\( f(x) = x² +x -2 \\) in the interval \\( [0, 100] \\).

  <a href="https://numbat.dev/?q=use%20numerics%3A%3Asolve%0Afn%20f%28x%29%20%3D%20x%C2%B2%20%2Bx%20%2D2%0Aroot%5Fbisect%28f%2C%200%2C%20100%2C%200%2E01%2C%200%2E01%29"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> fn f(x) = x² +x -2
    root_bisect(f, 0, 100, 0.01, 0.01)
    
      fn f(x: Scalar) -> Scalar = (x² + x) - 2
    
      root_bisect(f, 0, 100, 0.01, 0.01)
    
        = 1.00098
    
  ```
</details>

### `root_newton` (Newton's method)
Find the root of the function \\( f(x) \\) and its derivative \\( f'(x) \\) using Newton's method.
More information [here](https://en.wikipedia.org/wiki/Newton%27s_method).

```nbt
fn root_newton<X: Dim, Y: Dim>(f: Fn[(X) -> Y], f_prime: Fn[(X) -> Y / X], x0: X, y_tol: Y) -> X
```

<details>
<summary>Examples</summary>

* Find a root of \\( f(x) = x² -3x +2 \\) using Newton's method.

  <a href="https://numbat.dev/?q=use%20numerics%3A%3Asolve%0Afn%20f%28x%29%20%3D%20x%C2%B2%20%2D3x%20%2B2%0Afn%20f%5Fprime%28x%29%20%3D%202x%20%2D3%0Aroot%5Fnewton%28f%2C%20f%5Fprime%2C%200%20%2C%200%2E01%29"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> fn f(x) = x² -3x +2
    fn f_prime(x) = 2x -3
    root_newton(f, f_prime, 0 , 0.01)
    
      fn f(x: Scalar) -> Scalar = (x² - 3 x) + 2
    
      fn f_prime(x: Scalar) -> Scalar = 2 x - 3
    
      root_newton(f, f_prime, 0, 0.01)
    
        = 0.996078
    
  ```
</details>

### `fixed_point` (Fixed-point iteration)
Compute the approximate fixed point of a function \\( f: X \rightarrow X \\) starting from \\( x_0 \\), until \\( |f(x) - x| < ε \\).
More information [here](https://en.wikipedia.org/wiki/Fixed-point_iteration).

```nbt
fn fixed_point<X: Dim>(f: Fn[(X) -> X], x0: X, ε: X) -> X
```

<details>
<summary>Examples</summary>

* Compute the fixed poin of \\( f(x) = x/2 -1 \\).

  <a href="https://numbat.dev/?q=use%20numerics%3A%3Afixed%5Fpoint%0Afn%20function%28x%29%20%3D%20x%2F2%20%2D%201%0Afixed%5Fpoint%28function%2C%200%2C%200%2E01%29"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> fn function(x) = x/2 - 1
    fixed_point(function, 0, 0.01)
    
      fn function(x: Scalar) -> Scalar = (x / 2) - 1
    
      fixed_point(function, 0, 0.01)
    
        = -1.99219
    
  ```
</details>

## Geometry

Defined in: `math::geometry`

### `hypot2`
The length of the hypotenuse of a right-angled triangle \\( \sqrt{x^2+y^2} \\).

```nbt
fn hypot2<T: Dim>(x: T, y: T) -> T
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=hypot2%283%2C%204%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> hypot2(3, 4)
    
      hypot2(3, 4)
    
        = 5
    
  ```
</details>

### `hypot3`
The Euclidean norm of a 3D vector \\( \sqrt{x^2+y^2+z^2} \\).

```nbt
fn hypot3<T: Dim>(x: T, y: T, z: T) -> T
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=hypot3%284%2C%201%2C%204%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> hypot3(4, 1, 4)
    
      hypot3(4, 1, 4)
    
        = 5.74456
    
  ```
</details>

### `circle_area`
The area of a circle, \\( \pi r^2 \\).

```nbt
fn circle_area<L: Dim>(radius: L) -> L^2
```

</details>

### `circle_circumference`
The circumference of a circle, \\( 2\pi r \\).

```nbt
fn circle_circumference<L: Dim>(radius: L) -> L
```

</details>

### `sphere_area`
The surface area of a sphere, \\( 4\pi r^2 \\).

```nbt
fn sphere_area<L: Dim>(radius: L) -> L^2
```

</details>

### `sphere_volume`
The volume of a sphere, \\( \frac{4}{3}\pi r^3 \\).

```nbt
fn sphere_volume<L: Dim>(radius: L) -> L^3
```

</details>

## Algebra

Defined in: `extra::algebra`

### `quadratic_equation` (Solve quadratic equations)
Returns the solutions of the equation a x² + b x + c = 0.
More information [here](https://en.wikipedia.org/wiki/Quadratic_equation).

```nbt
fn quadratic_equation<A: Dim, B: Dim>(a: A, b: B, c: B^2 / A) -> List<B / A>
```

<details>
<summary>Examples</summary>

* Solve the equation \\( 2x² -x -1 = 0 \\)

  <a href="https://numbat.dev/?q=use%20extra%3A%3Aalgebra%0Aquadratic%5Fequation%282%2C%20%2D1%2C%20%2D1%29"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> quadratic_equation(2, -1, -1)
    
      quadratic_equation(2, -1, -1)
    
        = [1, -0.5]    [List<Scalar>]
    
  ```
</details>

## Trigonometry (extra)

Defined in: `math::trigonometry_extra`

### `cot`

```nbt
fn cot(x: Scalar) -> Scalar
```

</details>

### `acot`

```nbt
fn acot(x: Scalar) -> Scalar
```

</details>

### `coth`

```nbt
fn coth(x: Scalar) -> Scalar
```

</details>

### `acoth`

```nbt
fn acoth(x: Scalar) -> Scalar
```

</details>

### `secant`

```nbt
fn secant(x: Scalar) -> Scalar
```

</details>

### `arcsecant`

```nbt
fn arcsecant(x: Scalar) -> Scalar
```

</details>

### `cosecant`

```nbt
fn cosecant(x: Scalar) -> Scalar
```

</details>

### `csc`

```nbt
fn csc(x: Scalar) -> Scalar
```

</details>

### `acsc`

```nbt
fn acsc(x: Scalar) -> Scalar
```

</details>

### `sech`

```nbt
fn sech(x: Scalar) -> Scalar
```

</details>

### `asech`

```nbt
fn asech(x: Scalar) -> Scalar
```

</details>

### `csch`

```nbt
fn csch(x: Scalar) -> Scalar
```

</details>

### `acsch`

```nbt
fn acsch(x: Scalar) -> Scalar
```

</details>

