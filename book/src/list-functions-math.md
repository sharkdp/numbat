# Mathematical functions

[Basics](#basics) · [Transcendental functions](#transcendental-functions) · [Trigonometry](#trigonometry) · [Statistics](#statistics) · [Random sampling, distributions](#random-sampling-distributions) · [Number theory](#number-theory) · [Numerical methods](#numerical-methods) · [Percentage calculations](#percentage-calculations) · [Geometry](#geometry) · [Algebra](#algebra) · [Trigonometry (extra)](#trigonometry-(extra))

## Basics

Defined in: `core::functions`

### `id` (Identity function)
Return the input value.

```nbt
fn id<A>(x: A) -> A
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=id%288%20kg%29')""></button></div><code class="language-nbt hljs numbat">id(8 kg)

    = 8 kg    [Mass]
</code></pre>

</details>

### `abs` (Absolute value)
Return the absolute value \\( |x| \\) of the input. This works for quantities, too: `abs(-5 m) = 5 m`.
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.abs).

```nbt
fn abs<T: Dim>(x: T) -> T
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=abs%28%2D22%2E2%20m%29')""></button></div><code class="language-nbt hljs numbat">abs(-22.2 m)

    = 22.2 m    [Length]
</code></pre>

</details>

### `sqrt` (Square root)
Return the square root \\( \sqrt{x} \\) of the input: `sqrt(121 m^2) = 11 m`.
More information [here](https://en.wikipedia.org/wiki/Square_root).

```nbt
fn sqrt<D: Dim>(x: D^2) -> D
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=sqrt%284%20are%29%20%2D%3E%20m')""></button></div><code class="language-nbt hljs numbat">sqrt(4 are) -> m

    = 20 m    [Length]
</code></pre>

</details>

### `cbrt` (Cube root)
Return the cube root \\( \sqrt[3]{x} \\) of the input: `cbrt(8 m^3) = 2 m`.
More information [here](https://en.wikipedia.org/wiki/Cube_root).

```nbt
fn cbrt<D: Dim>(x: D^3) -> D
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=cbrt%288%20L%29%20%2D%3E%20cm')""></button></div><code class="language-nbt hljs numbat">cbrt(8 L) -> cm

    = 20.0 cm    [Length]
</code></pre>

</details>

### `sqr` (Square function)
Return the square of the input, \\( x^2 \\): `sqr(5 m) = 25 m^2`.

```nbt
fn sqr<D: Dim>(x: D) -> D^2
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=sqr%287%29')""></button></div><code class="language-nbt hljs numbat">sqr(7)

    = 49
</code></pre>

</details>

### `round` (Rounding)
Round to the nearest integer. If the value is half-way between two integers, round away from \\( 0 \\). See also: `round_in`.
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.round).

```nbt
fn round(x: Scalar) -> Scalar
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=round%285%2E5%29')""></button></div><code class="language-nbt hljs numbat">round(5.5)

    = 6
</code></pre>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=round%28%2D5%2E5%29')""></button></div><code class="language-nbt hljs numbat">round(-5.5)

    = -6
</code></pre>

</details>

### `round_in` (Rounding)
Round to the nearest multiple of `base`.

```nbt
fn round_in<D: Dim>(base: D, value: D) -> D
```

<details>
<summary>Examples</summary>

Round in meters.
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=round%5Fin%28m%2C%205%2E3%20m%29')""></button></div><code class="language-nbt hljs numbat">round_in(m, 5.3 m)

    = 5 m    [Length]
</code></pre>

Round in centimeters.
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=round%5Fin%28cm%2C%205%2E3%20m%29')""></button></div><code class="language-nbt hljs numbat">round_in(cm, 5.3 m)

    = 530 cm    [Length]
</code></pre>

</details>

### `floor` (Floor function)
Returns the largest integer less than or equal to \\( x \\). See also: `floor_in`.
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.floor).

```nbt
fn floor(x: Scalar) -> Scalar
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=floor%285%2E5%29')""></button></div><code class="language-nbt hljs numbat">floor(5.5)

    = 5
</code></pre>

</details>

### `floor_in` (Floor function)
Returns the largest integer multiple of `base` less than or equal to `value`.

```nbt
fn floor_in<D: Dim>(base: D, value: D) -> D
```

<details>
<summary>Examples</summary>

Floor in meters.
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=floor%5Fin%28m%2C%205%2E7%20m%29')""></button></div><code class="language-nbt hljs numbat">floor_in(m, 5.7 m)

    = 5 m    [Length]
</code></pre>

Floor in centimeters.
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=floor%5Fin%28cm%2C%205%2E7%20m%29')""></button></div><code class="language-nbt hljs numbat">floor_in(cm, 5.7 m)

    = 570 cm    [Length]
</code></pre>

</details>

### `ceil` (Ceil function)
Returns the smallest integer greater than or equal to \\( x \\). See also: `ceil_in`.
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.ceil).

```nbt
fn ceil(x: Scalar) -> Scalar
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=ceil%285%2E5%29')""></button></div><code class="language-nbt hljs numbat">ceil(5.5)

    = 6
</code></pre>

</details>

### `ceil_in` (Ceil function)
Returns the smallest integer multiple of `base` greater than or equal to `value`.

```nbt
fn ceil_in<D: Dim>(base: D, value: D) -> D
```

<details>
<summary>Examples</summary>

Ceil in meters.
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=ceil%5Fin%28m%2C%205%2E3%20m%29')""></button></div><code class="language-nbt hljs numbat">ceil_in(m, 5.3 m)

    = 6 m    [Length]
</code></pre>

Ceil in centimeters.
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=ceil%5Fin%28cm%2C%205%2E3%20m%29')""></button></div><code class="language-nbt hljs numbat">ceil_in(cm, 5.3 m)

    = 530 cm    [Length]
</code></pre>

</details>

### `trunc` (Truncation)
Returns the integer part of \\( x \\). Non-integer numbers are always truncated towards zero. See also: `trunc_in`.
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.trunc).

```nbt
fn trunc(x: Scalar) -> Scalar
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=trunc%285%2E5%29')""></button></div><code class="language-nbt hljs numbat">trunc(5.5)

    = 5
</code></pre>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=trunc%28%2D5%2E5%29')""></button></div><code class="language-nbt hljs numbat">trunc(-5.5)

    = -5
</code></pre>

</details>

### `trunc_in` (Truncation)
Truncates to an integer multiple of `base` (towards zero).

```nbt
fn trunc_in<D: Dim>(base: D, value: D) -> D
```

<details>
<summary>Examples</summary>

Truncate in meters.
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=trunc%5Fin%28m%2C%205%2E7%20m%29')""></button></div><code class="language-nbt hljs numbat">trunc_in(m, 5.7 m)

    = 5 m    [Length]
</code></pre>

Truncate in centimeters.
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=trunc%5Fin%28cm%2C%205%2E7%20m%29')""></button></div><code class="language-nbt hljs numbat">trunc_in(cm, 5.7 m)

    = 570 cm    [Length]
</code></pre>

</details>

### `mod` (Modulo)
Calculates the least nonnegative remainder of \\( a (\mod b) \\).
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.rem_euclid).

```nbt
fn mod<T: Dim>(a: T, b: T) -> T
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=mod%2827%2C%205%29')""></button></div><code class="language-nbt hljs numbat">mod(27, 5)

    = 2
</code></pre>

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

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=exp%284%29')""></button></div><code class="language-nbt hljs numbat">exp(4)

    = 54.5982
</code></pre>

</details>

### `ln` (Natural logarithm)
The natural logarithm with base \\( e \\).
More information [here](https://en.wikipedia.org/wiki/Natural_logarithm).

```nbt
fn ln(x: Scalar) -> Scalar
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=ln%2820%29')""></button></div><code class="language-nbt hljs numbat">ln(20)

    = 2.99573
</code></pre>

</details>

### `log` (Natural logarithm)
The natural logarithm with base \\( e \\).
More information [here](https://en.wikipedia.org/wiki/Natural_logarithm).

```nbt
fn log(x: Scalar) -> Scalar
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=log%2820%29')""></button></div><code class="language-nbt hljs numbat">log(20)

    = 2.99573
</code></pre>

</details>

### `log10` (Common logarithm)
The common logarithm with base \\( 10 \\).
More information [here](https://en.wikipedia.org/wiki/Common_logarithm).

```nbt
fn log10(x: Scalar) -> Scalar
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=log10%28100%29')""></button></div><code class="language-nbt hljs numbat">log10(100)

    = 2
</code></pre>

</details>

### `log2` (Binary logarithm)
The binary logarithm with base \\( 2 \\).
More information [here](https://en.wikipedia.org/wiki/Binary_logarithm).

```nbt
fn log2(x: Scalar) -> Scalar
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=log2%28256%29')""></button></div><code class="language-nbt hljs numbat">log2(256)

    = 8
</code></pre>

</details>

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
Get the largest element of a list.

```nbt
fn maximum<D: Dim>(xs: List<D>) -> D
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=maximum%28%5B30%20cm%2C%202%20m%5D%29')""></button></div><code class="language-nbt hljs numbat">maximum([30 cm, 2 m])

    = 2 m    [Length]
</code></pre>

</details>

### `minimum` (Minimum)
Get the smallest element of a list.

```nbt
fn minimum<D: Dim>(xs: List<D>) -> D
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=minimum%28%5B30%20cm%2C%202%20m%5D%29')""></button></div><code class="language-nbt hljs numbat">minimum([30 cm, 2 m])

    = 30 cm    [Length]
</code></pre>

</details>

### `mean` (Arithmetic mean)
Calculate the arithmetic mean of a list of quantities.
More information [here](https://en.wikipedia.org/wiki/Arithmetic_mean).

```nbt
fn mean<D: Dim>(xs: List<D>) -> D
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=mean%28%5B1%20m%2C%202%20m%2C%20300%20cm%5D%29')""></button></div><code class="language-nbt hljs numbat">mean([1 m, 2 m, 300 cm])

    = 2 m    [Length]
</code></pre>

</details>

### `variance` (Variance)
Calculate the population variance of a list of quantities.
More information [here](https://en.wikipedia.org/wiki/Variance).

```nbt
fn variance<D: Dim>(xs: List<D>) -> D^2
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=variance%28%5B1%20m%2C%202%20m%2C%20300%20cm%5D%29')""></button></div><code class="language-nbt hljs numbat">variance([1 m, 2 m, 300 cm])

    = 0.666667 m²    [Area]
</code></pre>

</details>

### `stdev` (Standard deviation)
Calculate the population standard deviation of a list of quantities.
More information [here](https://en.wikipedia.org/wiki/Standard_deviation).

```nbt
fn stdev<D: Dim>(xs: List<D>) -> D
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=stdev%28%5B1%20m%2C%202%20m%2C%20300%20cm%5D%29')""></button></div><code class="language-nbt hljs numbat">stdev([1 m, 2 m, 300 cm])

    = 0.816497 m    [Length]
</code></pre>

</details>

### `median` (Median)
Calculate the median of a list of quantities.
More information [here](https://en.wikipedia.org/wiki/Median).

```nbt
fn median<D: Dim>(xs: List<D>) -> D
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=median%28%5B1%20m%2C%202%20m%2C%20400%20cm%5D%29')""></button></div><code class="language-nbt hljs numbat">median([1 m, 2 m, 400 cm])

    = 2 m    [Length]
</code></pre>

</details>

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

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=gcd%2860%2C%2042%29')""></button></div><code class="language-nbt hljs numbat">gcd(60, 42)

    = 6
</code></pre>

</details>

### `lcm` (Least common multiple)
The smallest positive integer that is divisible by both \\( a \\) and \\( b \\).
More information [here](https://en.wikipedia.org/wiki/Least_common_multiple).

```nbt
fn lcm(a: Scalar, b: Scalar) -> Scalar
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=lcm%2814%2C%204%29')""></button></div><code class="language-nbt hljs numbat">lcm(14, 4)

    = 28
</code></pre>

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

Compute the derivative of \\( f(x) = x² -x -1 \\) at \\( x=1 \\).
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=use%20numerics%3A%3Adiff%0Afn%20polynomial%28x%29%20%3D%20x%C2%B2%20%2D%20x%20%2D%201%0Adiff%28polynomial%2C%201%29')""></button></div><code class="language-nbt hljs numbat">use numerics::diff
fn polynomial(x) = x² - x - 1
diff(polynomial, 1)

    = 1.0
</code></pre>

Compute the free fall velocity after \\( t=2 s \\).
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=use%20numerics%3A%3Adiff%0Afn%20distance%28t%29%20%3D%200%2E5%20g0%20t%C2%B2%0Afn%20velocity%28t%29%20%3D%20diff%28distance%2C%20t%29%0Avelocity%282%20s%29')""></button></div><code class="language-nbt hljs numbat">use numerics::diff
fn distance(t) = 0.5 g0 t²
fn velocity(t) = diff(distance, t)
velocity(2 s)

    = 19.6133 m/s    [Velocity]
</code></pre>

</details>

### `root_bisect` (Bisection method)
Find the root of the function \\( f \\) in the interval \\( [x_1, x_2] \\) using the bisection method. The function \\( f \\) must be continuous and \\( f(x_1) \cdot f(x_2) < 0 \\).
More information [here](https://en.wikipedia.org/wiki/Bisection_method).

```nbt
fn root_bisect<X: Dim, Y: Dim>(f: Fn[(X) -> Y], x1: X, x2: X, x_tol: X, y_tol: Y) -> X
```

<details>
<summary>Examples</summary>

Find the root of \\( f(x) = x² +x -2 \\) in the interval \\( [0, 100] \\).
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=use%20numerics%3A%3Asolve%0Afn%20f%28x%29%20%3D%20x%C2%B2%20%2Bx%20%2D2%0Aroot%5Fbisect%28f%2C%200%2C%20100%2C%200%2E01%2C%200%2E01%29')""></button></div><code class="language-nbt hljs numbat">use numerics::solve
fn f(x) = x² +x -2
root_bisect(f, 0, 100, 0.01, 0.01)

    = 1.00098
</code></pre>

</details>

### `root_newton` (Newton's method)
Find the root of the function \\( f(x) \\) and its derivative \\( f'(x) \\) using Newton's method.
More information [here](https://en.wikipedia.org/wiki/Newton%27s_method).

```nbt
fn root_newton<X: Dim, Y: Dim>(f: Fn[(X) -> Y], f_prime: Fn[(X) -> Y / X], x0: X, y_tol: Y) -> X
```

<details>
<summary>Examples</summary>

Find a root of \\( f(x) = x² -3x +2 \\) using Newton's method.
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=use%20numerics%3A%3Asolve%0Afn%20f%28x%29%20%3D%20x%C2%B2%20%2D3x%20%2B2%0Afn%20f%5Fprime%28x%29%20%3D%202x%20%2D3%0Aroot%5Fnewton%28f%2C%20f%5Fprime%2C%200%20%2C%200%2E01%29')""></button></div><code class="language-nbt hljs numbat">use numerics::solve
fn f(x) = x² -3x +2
fn f_prime(x) = 2x -3
root_newton(f, f_prime, 0 , 0.01)

    = 0.996078
</code></pre>

</details>

### `fixed_point` (Fixed-point iteration)
Compute the approximate fixed point of a function \\( f: X \rightarrow X \\) starting from \\( x_0 \\), until \\( |f(x) - x| < ε \\).
More information [here](https://en.wikipedia.org/wiki/Fixed-point_iteration).

```nbt
fn fixed_point<X: Dim>(f: Fn[(X) -> X], x0: X, ε: X) -> X
```

<details>
<summary>Examples</summary>

Compute the fixed poin of \\( f(x) = x/2 -1 \\).
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=use%20numerics%3A%3Afixed%5Fpoint%0Afn%20function%28x%29%20%3D%20x%2F2%20%2D%201%0Afixed%5Fpoint%28function%2C%200%2C%200%2E01%29')""></button></div><code class="language-nbt hljs numbat">use numerics::fixed_point
fn function(x) = x/2 - 1
fixed_point(function, 0, 0.01)

    = -1.99219
</code></pre>

</details>

## Percentage calculations

Defined in: `math::percentage_calculations`

### `increase_by`
Increase a quantity by the given percentage.
More information [here](https://en.wikipedia.org/wiki/Percentage#Percentage_increase_and_decrease).

```nbt
fn increase_by<D: Dim>(percentage: Scalar, quantity: D) -> D
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=72%20%E2%82%AC%20%7C%3E%20increase%5Fby%2815%25%29')""></button></div><code class="language-nbt hljs numbat">72 € |> increase_by(15%)

    = 82.8 €    [Money]
</code></pre>

</details>

### `decrease_by`
Decrease a quantity by the given percentage.
More information [here](https://en.wikipedia.org/wiki/Percentage#Percentage_increase_and_decrease).

```nbt
fn decrease_by<D: Dim>(percentage: Scalar, quantity: D) -> D
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=210%20cm%20%7C%3E%20decrease%5Fby%2810%25%29')""></button></div><code class="language-nbt hljs numbat">210 cm |> decrease_by(10%)

    = 189 cm    [Length]
</code></pre>

</details>

### `percentage_change`
By how many percent has a given quantity increased or decreased?.
More information [here](https://en.wikipedia.org/wiki/Percentage).

```nbt
fn percentage_change<D: Dim>(old: D, new: D) -> Scalar
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=percentage%5Fchange%2835%20kg%2C%2042%20kg%29')""></button></div><code class="language-nbt hljs numbat">percentage_change(35 kg, 42 kg)

    = 20 %
</code></pre>

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

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=hypot2%283%20m%2C%204%20m%29')""></button></div><code class="language-nbt hljs numbat">hypot2(3 m, 4 m)

    = 5 m    [Length]
</code></pre>

</details>

### `hypot3`
The Euclidean norm of a 3D vector \\( \sqrt{x^2+y^2+z^2} \\).

```nbt
fn hypot3<T: Dim>(x: T, y: T, z: T) -> T
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=hypot3%288%2C%209%2C%2012%29')""></button></div><code class="language-nbt hljs numbat">hypot3(8, 9, 12)

    = 17
</code></pre>

</details>

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

<details>
<summary>Examples</summary>

Solve the equation \\( 2x² -x -1 = 0 \\)
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=use%20extra%3A%3Aalgebra%0Aquadratic%5Fequation%282%2C%20%2D1%2C%20%2D1%29')""></button></div><code class="language-nbt hljs numbat">use extra::algebra
quadratic_equation(2, -1, -1)

    = [1, -0.5]    [List<Scalar>]
</code></pre>

</details>

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

