---
icon: lucide/ellipsis
---

# Other functions

[Error handling](#error-handling) · [Debugging](#debugging) · [Floating point](#floating-point) · [Quantities](#quantities) · [Chemical elements](#chemical-elements) · [Mixed unit conversion](#mixed-unit-conversion) · [Temperature conversion](#temperature-conversion) · [Speed of sound](#speed-of-sound) · [Color format conversion](#color-format-conversion) · [Celestial calculations](#celestial-calculations)

## Error handling

Defined in: `core::error`

### `error`
Throw an error with the specified message. Stops the execution of the program.

```nbt
fn error<T>(message: String) -> T
```

## Debugging

Defined in: `core::debug`

### `inspect`
Print the value (and type) of the argument and return it. Useful for debugging.

```nbt
fn inspect<T>(x: T) -> T
```

!!! example "Example"
    ```nbt
    inspect(36 km / 1.5 hours) * 1 day

        = 576 km    [Length]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=inspect%2836%20km%20%2F%201%2E5%20hours%29%20%2A%201%20day){ .md-button }

!!! example "Example"
    ```nbt
    range(1, 3) |> map(sqr) |> map(inspect) |> sum

        = 14
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=range%281%2C%203%29%20%7C%3E%20map%28sqr%29%20%7C%3E%20map%28inspect%29%20%7C%3E%20sum){ .md-button }

## Floating point

Defined in: `core::numbers`

### `is_nan`
Returns true if the input is `NaN`.
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.is_nan).

```nbt
fn is_nan<T: Dim>(n: T) -> Bool
```

!!! example "Example"
    ```nbt
    is_nan(37)

        = false    [Bool]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=is%5Fnan%2837%29){ .md-button }

!!! example "Example"
    ```nbt
    is_nan(NaN)

        = true    [Bool]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=is%5Fnan%28NaN%29){ .md-button }

### `is_infinite`
Returns true if the input is positive infinity or negative infinity.
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.is_infinite).

```nbt
fn is_infinite<T: Dim>(n: T) -> Bool
```

!!! example "Example"
    ```nbt
    is_infinite(37)

        = false    [Bool]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=is%5Finfinite%2837%29){ .md-button }

!!! example "Example"
    ```nbt
    is_infinite(-inf)

        = true    [Bool]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=is%5Finfinite%28%2Dinf%29){ .md-button }

### `is_finite`
Returns true if the input is neither infinite nor `NaN`.

```nbt
fn is_finite<T: Dim>(n: T) -> Bool
```

!!! example "Example"
    ```nbt
    is_finite(37)

        = true    [Bool]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=is%5Ffinite%2837%29){ .md-button }

!!! example "Example"
    ```nbt
    is_finite(-inf)

        = false    [Bool]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=is%5Ffinite%28%2Dinf%29){ .md-button }

### `is_zero`
Returns true if the input is 0 (zero).

```nbt
fn is_zero<D: Dim>(value: D) -> Bool
```

!!! example "Example"
    ```nbt
    is_zero(37)

        = false    [Bool]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=is%5Fzero%2837%29){ .md-button }

!!! example "Example"
    ```nbt
    is_zero(0)

        = true    [Bool]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=is%5Fzero%280%29){ .md-button }

### `is_nonzero`
Returns true unless the input is 0 (zero).

```nbt
fn is_nonzero<D: Dim>(value: D) -> Bool
```

!!! example "Example"
    ```nbt
    is_nonzero(37)

        = true    [Bool]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=is%5Fnonzero%2837%29){ .md-button }

!!! example "Example"
    ```nbt
    is_nonzero(0)

        = false    [Bool]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=is%5Fnonzero%280%29){ .md-button }

### `is_integer`
Returns true if the input is an integer.

```nbt
fn is_integer(x: Scalar) -> Bool
```

!!! example "Example"
    ```nbt
    is_integer(3)

        = true    [Bool]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=is%5Finteger%283%29){ .md-button }

!!! example "Example"
    ```nbt
    is_integer(pi)

        = false    [Bool]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=is%5Finteger%28pi%29){ .md-button }

## Quantities

Defined in: `core::quantities`

### `value_of`
Extract the plain value of a quantity (the `20` in `20 km/h`). This can be useful in generic code, but should generally be avoided otherwise.

```nbt
fn value_of<T: Dim>(x: T) -> Scalar
```

!!! example "Example"
    ```nbt
    value_of(20 km/h)

        = 20
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=value%5Fof%2820%20km%2Fh%29){ .md-button }

### `unit_of`
Extract the unit of a quantity (the `km/h` in `20 km/h`). This can be useful in generic code, but should generally be avoided otherwise. Returns an error if the quantity is zero.

```nbt
fn unit_of<T: Dim>(x: T) -> T
```

!!! example "Example"
    ```nbt
    unit_of(20 km/h)

        = 1 km/h    [Velocity]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=unit%5Fof%2820%20km%2Fh%29){ .md-button }

### `has_unit`
Returns true if `quantity` has the same unit as `unit_query`, or if `quantity` evaluates to zero.

```nbt
fn has_unit<T: Dim>(quantity: T, unit_query: T) -> Bool
```

!!! example "Example"
    ```nbt
    has_unit(20 km/h, km/h)

        = true    [Bool]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=has%5Funit%2820%20km%2Fh%2C%20km%2Fh%29){ .md-button }

!!! example "Example"
    ```nbt
    has_unit(20 km/h, m/s)

        = false    [Bool]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=has%5Funit%2820%20km%2Fh%2C%20m%2Fs%29){ .md-button }

### `is_dimensionless`
Returns true if `quantity` is dimensionless, or if `quantity` is zero.

```nbt
fn is_dimensionless<T: Dim>(quantity: T) -> Bool
```

!!! example "Example"
    ```nbt
    is_dimensionless(10)

        = true    [Bool]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=is%5Fdimensionless%2810%29){ .md-button }

!!! example "Example"
    ```nbt
    is_dimensionless(10 km/h)

        = false    [Bool]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=is%5Fdimensionless%2810%20km%2Fh%29){ .md-button }

### `unit_name`
Returns a string representation of the unit of `quantity`. Returns an empty string if `quantity` is dimensionless.

```nbt
fn unit_name<T: Dim>(quantity: T) -> String
```

!!! example "Example"
    ```nbt
    unit_name(20)

        = ""    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=unit%5Fname%2820%29){ .md-button }

!!! example "Example"
    ```nbt
    unit_name(20 m^2)

        = "m²"    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=unit%5Fname%2820%20m%5E2%29){ .md-button }

!!! example "Example"
    ```nbt
    unit_name(20 km/h)

        = "km/h"    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=unit%5Fname%2820%20km%2Fh%29){ .md-button }

### `quantity_cast`
Unsafe function that returns the quantity `from` unmodified with the target dimension `To`. This can be useful in generic code, but should generally be avoided otherwise.

```nbt
fn quantity_cast<From: Dim, To: Dim>(f: From, t: To) -> To
```

!!! example "Example"
    ```nbt
    quantity_cast(1 nm, m)

        = 1 nm    [Length]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=quantity%5Fcast%281%20nm%2C%20m%29){ .md-button }

## Chemical elements

Defined in: `chemistry::elements`

### `element` (Chemical element)
Get properties of a chemical element by its symbol or name (case-insensitive).

```nbt
fn element(pattern: String) -> ChemicalElement
```

!!! example "Get the entire element struct for hydrogen."
    ```nbt
    element("H")

        = ChemicalElement { symbol: "H", name: "Hydrogen", atomic_number: 1, group: 1, group_name: "Alkali metals", period: 1, melting_point: 13.99 K, boiling_point: 20.271 K, density: 0.00008988 g/cm³, electron_affinity: 0.754 eV, ionization_energy: 13.598 eV, vaporization_heat: 0.904 kJ/mol }    [ChemicalElement]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=element%28%22H%22%29){ .md-button }

!!! example "Get the ionization energy of hydrogen."
    ```nbt
    element("hydrogen").ionization_energy

        = 13.598 eV    [Energy or Torque]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=element%28%22hydrogen%22%29%2Eionization%5Fenergy){ .md-button }

## Mixed unit conversion

Defined in: `units::mixed`

### `unit_list` (Unit list)
Convert a value to a mixed representation using the provided units.

```nbt
fn unit_list<D: Dim>(units: List<D>, value: D) -> List<D>
```

!!! example "Example"
    ```nbt
    5500 m |> unit_list([miles, yards, feet, inches])

        = [3 mi, 734 yd, 2 ft, 7.43307 in]    [List<Length>]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=5500%20m%20%7C%3E%20unit%5Flist%28%5Bmiles%2C%20yards%2C%20feet%2C%20inches%5D%29){ .md-button }

### `DMS` (Degrees, minutes, seconds)
Convert an angle to a mixed degrees, (arc)minutes, and (arc)seconds representation. Also called sexagesimal degree notation.
More information [here](https://en.wikipedia.org/wiki/Sexagesimal_degree).

```nbt
fn DMS(alpha: Angle) -> List<Angle>
```

!!! example "Example"
    ```nbt
    46.5858° -> DMS

        = [46°, 35′, 8.88″]    [List<Scalar>]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=46%2E5858%C2%B0%20%2D%3E%20DMS){ .md-button }

### `DM` (Degrees, decimal minutes)
Convert an angle to a mixed degrees and decimal minutes representation.
More information [here](https://en.wikipedia.org/wiki/Decimal_degrees).

```nbt
fn DM(alpha: Angle) -> List<Angle>
```

!!! example "Example"
    ```nbt
    46.5858° -> DM

        = [46°, 35.148′]    [List<Scalar>]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=46%2E5858%C2%B0%20%2D%3E%20DM){ .md-button }

### `feet_and_inches` (Feet and inches)
Convert a length to a mixed feet and inches representation.
More information [here](https://en.wikipedia.org/wiki/Foot_(unit)).

```nbt
fn feet_and_inches(length: Length) -> List<Length>
```

!!! example "Example"
    ```nbt
    180 cm -> feet_and_inches

        = [5 ft, 10.8661 in]    [List<Length>]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=180%20cm%20%2D%3E%20feet%5Fand%5Finches){ .md-button }

### `pounds_and_ounces` (Pounds and ounces)
Convert a mass to a mixed pounds and ounces representation.
More information [here](https://en.wikipedia.org/wiki/Pound_(mass)).

```nbt
fn pounds_and_ounces(mass: Mass) -> List<Mass>
```

!!! example "Example"
    ```nbt
    1 kg -> pounds_and_ounces

        = [2 lb, 3.27396 oz]    [List<Mass>]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=1%20kg%20%2D%3E%20pounds%5Fand%5Founces){ .md-button }

## Temperature conversion

Defined in: `physics::temperature_conversion`

### `from_celsius`
Converts from degree Celsius (°C) to Kelvin.
More information [here](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature).

```nbt
fn from_celsius(t_celsius: Scalar) -> Temperature
```

!!! example "300 °C in Kelvin."
    ```nbt
    from_celsius(300)

        = 573.15 K    [Temperature]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=from%5Fcelsius%28300%29){ .md-button }

### `celsius`
Converts from Kelvin to degree Celcius (°C). This can be used on the right hand side of a conversion operator: `200 K -> celsius`.
More information [here](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature).

```nbt
fn celsius(t_kelvin: Temperature) -> Scalar
```

!!! example "300 K in degree Celsius."
    ```nbt
    300K -> celsius

        = 26.85
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=300K%20%2D%3E%20celsius){ .md-button }

### `from_fahrenheit`
Converts from degree Fahrenheit (°F) to Kelvin.
More information [here](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature).

```nbt
fn from_fahrenheit(t_fahrenheit: Scalar) -> Temperature
```

!!! example "300 °F in Kelvin."
    ```nbt
    from_fahrenheit(300)

        = 422.039 K    [Temperature]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=from%5Ffahrenheit%28300%29){ .md-button }

### `fahrenheit`
Converts from Kelvin to degree Fahrenheit (°F). This can be used on the right hand side of a conversion operator: `200 K -> fahrenheit`.
More information [here](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature).

```nbt
fn fahrenheit(t_kelvin: Temperature) -> Scalar
```

!!! example "300 K in degree Fahrenheit."
    ```nbt
    300K -> fahrenheit

        = 80.33
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=300K%20%2D%3E%20fahrenheit){ .md-button }

## Speed of sound

Defined in: `physics::speed_of_sound`

### `speed_of_sound` (Speed of sound in dry air)
Calculate the speed of sound in dry air as a function of air temperature.
More information [here](https://en.wikipedia.org/wiki/Speed_of_sound#Speed_of_sound_in_ideal_gases_and_air).

```nbt
fn speed_of_sound(T_air: Temperature) -> Velocity
```

!!! example "Example"
    ```nbt
    speed_of_sound(from_celsius(20))

        = 343.263 m/s    [Velocity]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=speed%5Fof%5Fsound%28from%5Fcelsius%2820%29%29){ .md-button }

## Color format conversion

Defined in: `extra::color`

### `rgb`
Create a `Color` from RGB (red, green, blue) values in the range \( [0, 256) \).

```nbt
fn rgb(red: Scalar, green: Scalar, blue: Scalar) -> Color
```

!!! example "Example"
    ```nbt
    use extra::color
    rgb(125, 128, 218)

        = Color { red: 125, green: 128, blue: 218 }    [Color]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=use%20extra%3A%3Acolor%0Argb%28125%2C%20128%2C%20218%29){ .md-button }

### `color`
Create a `Color` from a (hexadecimal) value.

```nbt
fn color(rgb_hex: Scalar) -> Color
```

!!! example "Example"
    ```nbt
    use extra::color
    color(0xff7700)

        = Color { red: 255, green: 119, blue: 0 }    [Color]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=use%20extra%3A%3Acolor%0Acolor%280xff7700%29){ .md-button }

### `color_rgb`
Convert a color to its RGB representation.

```nbt
fn color_rgb(color: Color) -> String
```

!!! example "Example"
    ```nbt
    use extra::color
    cyan -> color_rgb

        = "rgb(0, 255, 255)"    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=use%20extra%3A%3Acolor%0Acyan%20%2D%3E%20color%5Frgb){ .md-button }

### `color_rgb_float`
Convert a color to its RGB floating point representation.

```nbt
fn color_rgb_float(color: Color) -> String
```

!!! example "Example"
    ```nbt
    use extra::color
    cyan -> color_rgb_float

        = "rgb(0.000, 1.000, 1.000)"    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=use%20extra%3A%3Acolor%0Acyan%20%2D%3E%20color%5Frgb%5Ffloat){ .md-button }

### `color_hex`
Convert a color to its hexadecimal representation.

```nbt
fn color_hex(color: Color) -> String
```

!!! example "Example"
    ```nbt
    use extra::color
    rgb(225, 36, 143) -> color_hex

        = "#e1248f"    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=use%20extra%3A%3Acolor%0Argb%28225%2C%2036%2C%20143%29%20%2D%3E%20color%5Fhex){ .md-button }

## Celestial calculations

Defined in: `extra::celestial`

### `sunrise_sunset` (Sunrise and sunset)
Compute sunrise, solar noon (transit), and sunset times for a given location and date.

```nbt
fn sunrise_sunset(position: Position, dt: DateTime) -> SunTimes
```

!!! example "Example"
    ```nbt
    use extra::celestial
    sunrise_sunset(Position { lat: 40.713°, lon: -74.006° }, datetime("2023-03-21 12:00:00 America/New_York"))

        = SunTimes { sunrise: 2023-03-21 10:57:38 UTC, transit: 2023-03-21 17:03:09 UTC, sunset: 2023-03-21 23:08:40 UTC }    [SunTimes]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=use%20extra%3A%3Acelestial%0Asunrise%5Fsunset%28Position%20%7B%20lat%3A%2040%2E713%C2%B0%2C%20lon%3A%20%2D74%2E006%C2%B0%20%7D%2C%20datetime%28%222023%2D03%2D21%2012%3A00%3A00%20America%2FNew%5FYork%22%29%29){ .md-button }

