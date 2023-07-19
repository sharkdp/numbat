# Predefined functions

## Math

### Basics

* `fn abs<T>(x: T) -> T`
* `fn round<T>(x: T) -> T`
* `fn floor<T>(x: T) -> T`
* `fn ceil<T>(x: T) -> T`
* `fn mod<T>(a: T, b: T) -> T`
* `fn sqrt<D>(x: D^2) -> D`
* `fn sqr<D>(x: D) -> D^2`

### Exponential and logarithm

* `fn exp(x: Scalar) -> Scalar`
* `fn ln(x: Scalar) -> Scalar`
* `fn log(x: Scalar) -> Scalar`
* `fn log10(x: Scalar) -> Scalar`
* `fn log2(x: Scalar) -> Scalar`

### Trigonometry

* `fn cos(x: Scalar) -> Scalar`
* `fn sin(x: Scalar) -> Scalar`
* `fn tan(x: Scalar) -> Scalar`
* `fn asin(x: Scalar) -> Scalar`
* `fn acos(x: Scalar) -> Scalar`
* `fn atan(x: Scalar) -> Scalar`
* `fn atan2<T>(y: T, x: T) -> Scalar`
* `fn sinh(x: Scalar) -> Scalar`
* `fn cosh(x: Scalar) -> Scalar`
* `fn tanh(x: Scalar) -> Scalar`
* `fn asinh(x: Scalar) -> Scalar`
* `fn acosh(x: Scalar) -> Scalar`
* `fn atanh(x: Scalar) -> Scalar`

### Statistics

* `fn mean<D>(xs: D…) -> D`
* `fn maximum<D>(xs: D…) -> D`
* `fn minimum<D>(xs: D…) -> D`

## Physics

### Temperature conversion

* `fn from_celsius(t_celsius: Scalar) -> Temperature`
* `fn to_celsius(t_kelvin: Temperature) -> Scalar`
* `fn from_fahrenheit(t_fahrenheit: Scalar) -> Temperature`
* `fn to_fahrenheit(t_kelvin: Temperature) -> Scalar`
