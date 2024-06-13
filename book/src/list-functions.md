<!-- NOTE! This file is auto-generated -->
# List of functions


## `unit_of`
```nbt
fn unit_of<T: Dim>(x: T) -> T
```
## `value_of`
```nbt
fn value_of<T: Dim>(x: T) -> Scalar
```
## `is_nan`
```nbt
fn is_nan<T: Dim>(n: T) -> Bool
```
## `is_infinite`
```nbt
fn is_infinite<T: Dim>(n: T) -> Bool
```
## Identity function (`id`)
```nbt
fn id<A>(x: A) -> A
```
## Absolute value (`abs`)
```nbt
fn abs<T: Dim>(x: T) -> T
```
[Further information](https://doc.rust-lang.org/std/primitive.f64.html#method.abs)
## Round (`round`)
Round to the nearest integer.

```nbt
fn round<T: Dim>(x: T) -> T
```
[Further information](https://doc.rust-lang.org/std/primitive.f64.html#method.round)
## Floor function (`floor`)
```nbt
fn floor<T: Dim>(x: T) -> T
```
[Further information](https://doc.rust-lang.org/std/primitive.f64.html#method.floor)
## Ceil function (`ceil`)
```nbt
fn ceil<T: Dim>(x: T) -> T
```
[Further information](https://doc.rust-lang.org/std/primitive.f64.html#method.ceil)
## Modulo (`mod`)
```nbt
fn mod<T: Dim>(a: T, b: T) -> T
```
[Further information](https://doc.rust-lang.org/std/primitive.f64.html#method.rem_euclid)
## `error`
Throw a user-defined error.

```nbt
fn error<T>(message: String) -> T
```
## `str_length`
```nbt
fn str_length(s: String) -> Scalar
```
## `str_slice`
```nbt
fn str_slice(s: String, start: Scalar, end: Scalar) -> String
```
## `chr`
```nbt
fn chr(n: Scalar) -> String
```
## `lowercase`
```nbt
fn lowercase(s: String) -> String
```
## `uppercase`
```nbt
fn uppercase(s: String) -> String
```
## `str_append`
```nbt
fn str_append(a: String, b: String) -> String
```
## `str_find`
```nbt
fn str_find(haystack: String, needle: String) -> Scalar
```
## `str_contains`
```nbt
fn str_contains(haystack: String, needle: String) -> Bool
```
## `str_replace`
```nbt
fn str_replace(s: String, pattern: String, replacement: String) -> String
```
## `str_repeat`
```nbt
fn str_repeat(a: String, n: Scalar) -> String
```
## `bin`
```nbt
fn bin(x: Scalar) -> String
```
## `oct`
```nbt
fn oct(x: Scalar) -> String
```
## `dec`
```nbt
fn dec(x: Scalar) -> String
```
## `hex`
```nbt
fn hex(x: Scalar) -> String
```
## `base`
```nbt
fn base(b: Scalar) -> Fn[(Scalar) -> String]
```
## `len`
Get the length of a list

```nbt
fn len<A>(xs: List<A>) -> Scalar
```
## `head`
Get the first element of a list. Yields a runtime error if the list is empty.

```nbt
fn head<A>(xs: List<A>) -> A
```
## `tail`
Get everything but the first element of a list. Yields a runtime error if the list is empty.

```nbt
fn tail<A>(xs: List<A>) -> List<A>
```
## `cons`
Prepend an element to a list

```nbt
fn cons<A>(x: A, xs: List<A>) -> List<A>
```
## `is_empty`
Check if a list is empty

```nbt
fn is_empty<A>(xs: List<A>) -> Bool
```
## `concat`
Concatenate two lists

```nbt
fn concat<A>(xs1: List<A>, xs2: List<A>) -> List<A>
```
## `take`
Get the first `n` elements of a list

```nbt
fn take<A>(n: Scalar, xs: List<A>) -> List<A>
```
## `drop`
Get everything but the first `n` elements of a list

```nbt
fn drop<A>(n: Scalar, xs: List<A>) -> List<A>
```
## `element_at`
Get the element at index `i` in a list

```nbt
fn element_at<A>(i: Scalar, xs: List<A>) -> A
```
## `range`
Generate a range of integer numbers from `start` to `end` (inclusive)

```nbt
fn range(start: Scalar, end: Scalar) -> List<Scalar>
```
## `cons_end`
Append an element to the end of a list

```nbt
fn cons_end<A>(xs: List<A>, x: A) -> List<A>
```
## `reverse`
Reverse the order of a list

```nbt
fn reverse<A>(xs: List<A>) -> List<A>
```
## `map`
Generate a new list by applying a function to each element of the input list

```nbt
fn map<A, B>(f: Fn[(A) -> B], xs: List<A>) -> List<B>
```
## `filter`
Filter a list by a predicate

```nbt
fn filter<A>(p: Fn[(A) -> Bool], xs: List<A>) -> List<A>
```
## `foldl`
Fold a function over a list

```nbt
fn foldl<A, B>(f: Fn[(A, B) -> A], acc: A, xs: List<B>) -> A
```
## `sort_by_key`
Sort a list of elements, using the given key function that maps the element to a quantity

```nbt
fn sort_by_key<A, D: Dim>(key: Fn[(A) -> D], xs: List<A>) -> List<A>
```
## `sort`
Sort a list of quantities

```nbt
fn sort<D: Dim>(xs: List<D>) -> List<D>
```
## `intersperse`
Add an element between each pair of elements in a list

```nbt
fn intersperse<A>(sep: A, xs: List<A>) -> List<A>
```
## `sum`
Sum all elements of a list

```nbt
fn sum<D: Dim>(xs: List<D>) -> D
```
## `linspace`
Generate a list of `n_steps` evenly spaced numbers from `start` to `end` (inclusive)

```nbt
fn linspace<D: Dim>(start: D, end: D, n_steps: Scalar) -> List<D>
```
## `join`
Convert a list of strings into a single string by concatenating them with a separator

```nbt
fn join(xs: List<String>, sep: String) -> String
```
## `split`
Split a string into a list of strings using a separator

```nbt
fn split(input: String, separator: String) -> List<String>
```
## Standard uniform distribution sampling (`random`)
Uniformly samples the interval [0,1).

```nbt
fn random() -> Scalar
```
## Square root (`sqrt`)
```nbt
fn sqrt<D: Dim>(x: D^2) -> D
```
[Further information](https://en.wikipedia.org/wiki/Square_root)
## Square function (`sqr`)
```nbt
fn sqr<D: Dim>(x: D) -> D^2
```
## Exponential function (`exp`)
```nbt
fn exp(x: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Exponential_function)
## Natural logarithm (`ln`)
```nbt
fn ln(x: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Natural_logarithm)
## Natural logarithm (`log`)
```nbt
fn log(x: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Natural_logarithm)
## Common logarithm (`log10`)
```nbt
fn log10(x: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Common_logarithm)
## Binary logarithm (`log2`)
```nbt
fn log2(x: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Binary_logarithm)
## Sine (`sin`)
```nbt
fn sin(x: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Trigonometric_functions)
## Cosine (`cos`)
```nbt
fn cos(x: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Trigonometric_functions)
## Tangent (`tan`)
```nbt
fn tan(x: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Trigonometric_functions)
## Arc sine (`asin`)
```nbt
fn asin(x: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)
## Arc cosine (`acos`)
```nbt
fn acos(x: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)
## Arc tangent (`atan`)
```nbt
fn atan(x: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)
## `atan2`
```nbt
fn atan2<T: Dim>(y: T, x: T) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Atan2)
## Hyperbolic sine (`sinh`)
```nbt
fn sinh(x: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Hyperbolic_functions)
## Hyperbolic cosine (`cosh`)
```nbt
fn cosh(x: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Hyperbolic_functions)
## Hyperbolic tangent (`tanh`)
```nbt
fn tanh(x: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Hyperbolic_functions)
## Area hyperbolic sine (`asinh`)
```nbt
fn asinh(x: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Hyperbolic_functions)
## Area hyperbolic cosine (`acosh`)
```nbt
fn acosh(x: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Hyperbolic_functions)
## Area hyperbolic tangent  (`atanh`)
```nbt
fn atanh(x: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Hyperbolic_functions)
## Gamma function (`gamma`)
```nbt
fn gamma(x: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Gamma_function)
## Maxmimum (`maximum`)
Get the largest element of a list

```nbt
fn maximum<D: Dim>(xs: List<D>) -> D
```
## Minimum (`minimum`)
Get the smallest element of a list

```nbt
fn minimum<D: Dim>(xs: List<D>) -> D
```
## Arithmetic mean (`mean`)
```nbt
fn mean<D: Dim>(xs: List<D>) -> D
```
[Further information](https://en.wikipedia.org/wiki/Arithmetic_mean)
## Variance (`variance`)
Calculate the population variance of a list of quantities

```nbt
fn variance<D: Dim>(xs: List<D>) -> D^2
```
[Further information](https://en.wikipedia.org/wiki/Variance)
## Standard deviation (`stdev`)
Calculate the population standard deviation of a list of quantities

```nbt
fn stdev<D: Dim>(xs: List<D>) -> D
```
[Further information](https://en.wikipedia.org/wiki/Standard_deviation)
## Median (`median`)
Calculate the median of a list of quantities

```nbt
fn median<D: Dim>(xs: List<D>) -> D
```
[Further information](https://en.wikipedia.org/wiki/Median)
## Greatest common divisor (`gcd`)
```nbt
fn gcd(a: Scalar, b: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Greatest_common_divisor)
## Least common multiple (`lcm`)
```nbt
fn lcm(a: Scalar, b: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Least_common_multiple)
## `hypot2`
```nbt
fn hypot2<T: Dim>(x: T, y: T) -> T
```
## `hypot3`
```nbt
fn hypot3<T: Dim>(x: T, y: T, z: T) -> T
```
## `circle_area`
```nbt
fn circle_area<L: Dim>(radius: L) -> L^2
```
## `circle_circumference`
```nbt
fn circle_circumference<L: Dim>(radius: L) -> L
```
## `sphere_area`
```nbt
fn sphere_area<L: Dim>(radius: L) -> L^2
```
## `sphere_volume`
```nbt
fn sphere_volume<L: Dim>(radius: L) -> L^3
```
## `cot`
```nbt
fn cot(x: Scalar) -> Scalar
```
## `acot`
```nbt
fn acot(x: Scalar) -> Scalar
```
## `coth`
```nbt
fn coth(x: Scalar) -> Scalar
```
## `acoth`
```nbt
fn acoth(x: Scalar) -> Scalar
```
## `secant`
```nbt
fn secant(x: Scalar) -> Scalar
```
## `arcsecant`
```nbt
fn arcsecant(x: Scalar) -> Scalar
```
## `cosecant`
```nbt
fn cosecant(x: Scalar) -> Scalar
```
## `csc`
```nbt
fn csc(x: Scalar) -> Scalar
```
## `acsc`
```nbt
fn acsc(x: Scalar) -> Scalar
```
## `sech`
```nbt
fn sech(x: Scalar) -> Scalar
```
## `asech`
```nbt
fn asech(x: Scalar) -> Scalar
```
## `csch`
```nbt
fn csch(x: Scalar) -> Scalar
```
## `acsch`
```nbt
fn acsch(x: Scalar) -> Scalar
```
## Continuous uniform distribution sampling (`rand_uniform`)
Uniformly samples the interval [a,b) if a<=b or [b,a) if b<a using inversion sampling.

```nbt
fn rand_uniform<T: Dim>(a: T, b: T) -> T
```
[Further information](https://en.wikipedia.org/wiki/Continuous_uniform_distribution)
## Discrete uniform distribution sampling (`rand_int`)
Uniformly samples the integers in the interval [a, b].

```nbt
fn rand_int(a: Scalar, b: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Discrete_uniform_distribution)
## Bernoulli distribution sampling (`rand_bernoulli`)
Samples a Bernoulli random variable, that is, 1 with probability p, 0 with probability 1-p.
              Parameter p must be a probability (0 <= p <= 1).

```nbt
fn rand_bernoulli(p: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Bernoulli_distribution)
## Binomial distribution sampling (`rand_binom`)
Samples a binomial distribution by doing n Bernoulli trials with probability p.
              Parameter n must be a positive integer, parameter p must be a probability (0 <= p <= 1).

```nbt
fn rand_binom(n: Scalar, p: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Binomial_distribution)
## Normal distribution sampling (`rand_norm`)
Samples a normal distribution with mean μ and standard deviation σ using the Box-Muller transform.

```nbt
fn rand_norm<T: Dim>(μ: T, σ: T) -> T
```
[Further information](https://en.wikipedia.org/wiki/Normal_distribution)
## Geometric distribution sampling (`rand_geom`)
Samples a geometric distribution (the distribution of the number of Bernoulli trials with probability p needed to get one success) by inversion sampling.
              Parameter p must be a probability (0 <= p <= 1).

```nbt
fn rand_geom(p: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Geometric_distribution)
## Poisson distribution sampling (`rand_poisson`)
Sampling a poisson distribution with rate λ, that is, the distribution of the number of events occurring in a fixed interval if these events occur with mean rate λ.
              The rate parameter λ must not be negative.

```nbt
fn rand_poisson(λ: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Poisson_distribution)
## Exponential distribution sampling (`rand_expon`)
Sampling an exponential distribution (the distribution of the distance between events in a Poisson process with rate λ) using inversion sampling.
              The rate parameter λ must be positive.

```nbt
fn rand_expon<T: Dim>(λ: T) -> 1 / T
```
[Further information](https://en.wikipedia.org/wiki/Exponential_distribution)
## Log-normal distribution sampling (`rand_lognorm`)
Sampling a log-normal distribution, that is, a distribution whose log is a normal distribution with mean μ and standard deviation σ.

```nbt
fn rand_lognorm(μ: Scalar, σ: Scalar) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Log-normal_distribution)
## Pareto distribution sampling (`rand_pareto`)
Sampling a Pareto distribution with minimum value min and shape parameter α using inversion sampling.
              Both parameters α and min must be positive.

```nbt
fn rand_pareto<T: Dim>(α: Scalar, min: T) -> T
```
[Further information](https://en.wikipedia.org/wiki/Pareto_distribution)
## `from_celsius`
Converts from degree Celsius to Kelvin.

```nbt
fn from_celsius(t_celsius: Scalar) -> Temperature
```
[Further information](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature)
## `celsius`
Converts from Kelvin to degree Celsius.

```nbt
fn celsius(t_kelvin: Temperature) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature)
## `from_fahrenheit`
Converts from degree Fahrenheit to Kelvin.

```nbt
fn from_fahrenheit(t_fahrenheit: Scalar) -> Temperature
```
[Further information](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature)
## `fahrenheit`
Converts from Kelvin to degree Fahrenheit.

```nbt
fn fahrenheit(t_kelvin: Temperature) -> Scalar
```
[Further information](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature)
## Chemical element (`element`)
Get properties of a chemical element by its symbol or name (case-insensitive).

```nbt
fn element(pattern: String) -> ChemicalElement
```
## `now`
Returns the current date and time.

```nbt
fn now() -> DateTime
```
[Further information](https://numbat.dev/doc/date-and-time.html)
## `datetime`
Parses a string (date and time) into a DateTime object. See https://numbat.dev/doc/date-and-time.html#date-time-formats for an overview of the supported formats.

```nbt
fn datetime(input: String) -> DateTime
```
[Further information](https://numbat.dev/doc/date-and-time.html)
## `format_datetime`
Formats a DateTime object as a string.

```nbt
fn format_datetime(format: String, input: DateTime) -> String
```
[Further information](https://numbat.dev/doc/date-and-time.html)
## `get_local_timezone`
Returns the users local timezone.

```nbt
fn get_local_timezone() -> String
```
[Further information](https://numbat.dev/doc/date-and-time.html)
## `tz`
Returns a timezone conversion function, typically used with the conversion operator.

```nbt
fn tz(tz: String) -> Fn[(DateTime) -> DateTime]
```
[Further information](https://numbat.dev/doc/date-and-time.html)
## `unixtime`
Converts a DateTime to a UNIX timestamp.

```nbt
fn unixtime(input: DateTime) -> Scalar
```
[Further information](https://numbat.dev/doc/date-and-time.html)
## `from_unixtime`
Converts a UNIX timestamp to a DateTime object.

```nbt
fn from_unixtime(input: Scalar) -> DateTime
```
[Further information](https://numbat.dev/doc/date-and-time.html)
## `today`
Returns the current date at midnight (in the local time).

```nbt
fn today() -> DateTime
```
[Further information](https://numbat.dev/doc/date-and-time.html)
## `date`
Parses a string (only date) into a DateTime object.

```nbt
fn date(input: String) -> DateTime
```
[Further information](https://numbat.dev/doc/date-and-time.html)
## `time`
Parses a string (only time) into a DateTime object.

```nbt
fn time(input: String) -> DateTime
```
[Further information](https://numbat.dev/doc/date-and-time.html)
## Human-readable time duration (`human`)
Converts a time duration to a human-readable string in days, hours, minutes and seconds.

```nbt
fn human(time: Time) -> String
```
[Further information](https://numbat.dev/doc/date-and-time.html)
## `exchange_rate`
```nbt
fn exchange_rate(currency: String) -> Scalar
```
## Solve quadratic equations (`quadratic_equation`)
Returns the solutions of the equation a x² + b x + c = 0

```nbt
fn quadratic_equation<A: Dim, B: Dim>(a: A, b: B, c: B^2 / A) -> List<B / A>
```
[Further information](https://en.wikipedia.org/wiki/Quadratic_equation)
## Numerical differentiation (`diff`)
Compute the numerical derivative of a function at a point using the central difference method

```nbt
fn diff<X: Dim, Y: Dim>(f: Fn[(X) -> Y], x: X) -> Y / X
```
[Further information](https://en.wikipedia.org/wiki/Numerical_differentiation)
## Bisection method (`root_bisect`)
Find the root of the function f in the interval [x1, x2] using the bisection method. The function f must be continuous and f(x1) × f(x2) < 0.

```nbt
fn root_bisect<A: Dim, B: Dim>(f: Fn[(A) -> B], x1: A, x2: A, x_tolerance: A, y_tolerance: B) -> A
```
[Further information](https://en.wikipedia.org/wiki/Bisection_method)
## Newton's method (`root_newton`)
Find the root of the function f(x) and its derivative f'(x) using Newton's method.

```nbt
fn root_newton<A: Dim, B: Dim>(f: Fn[(A) -> B], f_prime: Fn[(A) -> B / A], x0: A, y_tolerance: B) -> A
```
[Further information](https://en.wikipedia.org/wiki/Newton%27s_method)
