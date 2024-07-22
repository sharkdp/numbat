# Other functions

[Error handling](#error-handling) · [Floating point](#floating-point) · [Quantities](#quantities) · [Chemical elements](#chemical-elements) · [Temperature conversion](#temperature-conversion)

## Error handling

Defined in: `core::error`

### `error`
Throw an error with the specified message. Stops the execution of the program.

```nbt
fn error<T>(message: String) -> T
```

## Floating point

Defined in: `core::numbers`

### `is_nan`
Returns true if the input is `NaN`.
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.is_nan).

```nbt
fn is_nan<T: Dim>(n: T) -> Bool
```

### `is_infinite`
Returns true if the input is positive infinity or negative infinity.
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.is_infinite).

```nbt
fn is_infinite<T: Dim>(n: T) -> Bool
```

## Quantities

Defined in: `core::quantities`

### `unit_of`
Extract the unit of a quantity (the `km/h` in `20 km/h`). This can be useful in generic code, but should generally be avoided otherwise.

```nbt
fn unit_of<T: Dim>(x: T) -> T
```

### `value_of`
Extract the plain value of a quantity (the `20` in `20 km/h`). This can be useful in generic code, but should generally be avoided otherwise.

```nbt
fn value_of<T: Dim>(x: T) -> Scalar
```

## Chemical elements

Defined in: `chemistry::elements`

### `element` (Chemical element)
Get properties of a chemical element by its symbol or name (case-insensitive). For example: `element("H")` or `element("hydrogen")`.

```nbt
fn element(pattern: String) -> ChemicalElement
```

## Temperature conversion

Defined in: `physics::temperature_conversion`

### `from_celsius`
Converts from degree Celsius (°C) to Kelvin.
More information [here](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature).

```nbt
fn from_celsius(t_celsius: Scalar) -> Temperature
```

### `celsius`
Converts from Kelvin to degree Celcius (°C). This can be used on the right hand side of a conversion operator: `200 K -> celsius`.
More information [here](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature).

```nbt
fn celsius(t_kelvin: Temperature) -> Scalar
```

### `from_fahrenheit`
Converts from degree Fahrenheit (°F) to Kelvin.
More information [here](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature).

```nbt
fn from_fahrenheit(t_fahrenheit: Scalar) -> Temperature
```

### `fahrenheit`
Converts from Kelvin to degree Fahrenheit (°F). This can be used on the right hand side of a conversion operator: `200 K -> fahrenheit`.
More information [here](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature).

```nbt
fn fahrenheit(t_kelvin: Temperature) -> Scalar
```

