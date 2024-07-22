<!-- NOTE! This file is auto-generated -->

# Other

### `unit_of`

```nbt
fn unit_of<T: Dim>(x: T) -> T
```
(defined in *core::quantities*)

### `value_of`

```nbt
fn value_of<T: Dim>(x: T) -> Scalar
```
(defined in *core::quantities*)

### `error`
Throw a user-defined error.

```nbt
fn error<T>(message: String) -> T
```
(defined in *core::error*)

### `from_celsius`
Converts from degree Celsius to Kelvin.
More information [here](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature).

```nbt
fn from_celsius(t_celsius: Scalar) -> Temperature
```
(defined in *physics::temperature_conversion*)

### `celsius`
Converts from Kelvin to degree Celsius.
More information [here](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature).

```nbt
fn celsius(t_kelvin: Temperature) -> Scalar
```
(defined in *physics::temperature_conversion*)

### `from_fahrenheit`
Converts from degree Fahrenheit to Kelvin.
More information [here](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature).

```nbt
fn from_fahrenheit(t_fahrenheit: Scalar) -> Temperature
```
(defined in *physics::temperature_conversion*)

### `fahrenheit`
Converts from Kelvin to degree Fahrenheit.
More information [here](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature).

```nbt
fn fahrenheit(t_kelvin: Temperature) -> Scalar
```
(defined in *physics::temperature_conversion*)

### `element` (Chemical element)
Get properties of a chemical element by its symbol or name (case-insensitive).

```nbt
fn element(pattern: String) -> ChemicalElement
```
(defined in *chemistry::elements*)

