# Other functions

[Error handling](#error-handling) · [Floating point](#floating-point) · [Quantities](#quantities) · [Chemical elements](#chemical-elements) · [Mixed unit conversion](#mixed-unit-conversion) · [Temperature conversion](#temperature-conversion) · [Color format conversion](#color-format-conversion)

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

### `is_finite`
Returns true if the input is neither infinite nor `NaN`.

```nbt
fn is_finite<T: Dim>(n: T) -> Bool
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

## Mixed unit conversion

Defined in: `units::mixed`

### `DMS` (Degrees, minutes, seconds)
Convert an angle to a mixed degrees, (arc)minutes, and (arc)seconds representation. Also called sexagesimal degree notation.
More information [here](https://en.wikipedia.org/wiki/Sexagesimal_degree).

```nbt
fn DMS(alpha: Angle) -> String
```

### `DM` (Degrees, decimal minutes)
Convert an angle to a mixed degrees and decimal minutes representation.
More information [here](https://en.wikipedia.org/wiki/Decimal_degrees).

```nbt
fn DM(alpha: Angle) -> String
```

### `feet_and_inches` (Feet and inches)
Convert a length to a mixed feet and inches representation.
More information [here](https://en.wikipedia.org/wiki/Foot_(unit)).

```nbt
fn feet_and_inches(length: Length) -> String
```

### `pounds_and_ounces` (Pounds and ounces)
Convert a mass to a mixed pounds and ounces representation.
More information [here](https://en.wikipedia.org/wiki/Pound_(mass)).

```nbt
fn pounds_and_ounces(mass: Mass) -> String
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

## Color format conversion

Defined in: `extra::color`

### `rgb`
Create a `Color` from RGB (red, green, blue) values in the range \\( [0, 256) \\).

```nbt
fn rgb(red: Scalar, green: Scalar, blue: Scalar) -> Color
```

### `color`
Create a `Color` from a (hexadecimal) value, e.g. `color(0xff7700)`.

```nbt
fn color(rgb_hex: Scalar) -> Color
```

### `color_rgb`
Convert a color to its RGB representation, e.g. `cyan -> color_rgb`.

```nbt
fn color_rgb(color: Color) -> String
```

### `color_rgb_float`
Convert a color to its RGB floating point representation, e.g. `cyan -> color_rgb_float`.

```nbt
fn color_rgb_float(color: Color) -> String
```

### `color_hex`
Convert a color to its hexadecimal representation, e.g. `rgb(225, 36, 143) -> color_hex`.

```nbt
fn color_hex(color: Color) -> String
```

