# Other functions

[Error handling](#error-handling) · [Floating point](#floating-point) · [Quantities](#quantities) · [Chemical elements](#chemical-elements) · [Mixed unit conversion](#mixed-unit-conversion) · [Temperature conversion](#temperature-conversion) · [Color format conversion](#color-format-conversion)

## Error handling

Defined in: `core::error`

### `error`
Throw an error with the specified message. Stops the execution of the program.

```nbt
fn error<T>(message: String) -> T
```

</details>

## Floating point

Defined in: `core::numbers`

### `is_nan`
Returns true if the input is `NaN`.
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.is_nan).

```nbt
fn is_nan<T: Dim>(n: T) -> Bool
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=is%5Fnan%2837%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> is_nan(37)
    
      is_nan(37)
    
        = false    [Bool]
    
  ```
* <a href="https://numbat.dev/?q=is%5Fnan%28NaN%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> is_nan(NaN)
    
      is_nan(NaN)
    
        = true    [Bool]
    
  ```
</details>

### `is_infinite`
Returns true if the input is positive infinity or negative infinity.
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.is_infinite).

```nbt
fn is_infinite<T: Dim>(n: T) -> Bool
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=is%5Finfinite%2837%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> is_infinite(37)
    
      is_infinite(37)
    
        = false    [Bool]
    
  ```
* <a href="https://numbat.dev/?q=is%5Finfinite%28%2Dinf%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> is_infinite(-inf)
    
      is_infinite(-inf)
    
        = true    [Bool]
    
  ```
</details>

### `is_finite`
Returns true if the input is neither infinite nor `NaN`.

```nbt
fn is_finite<T: Dim>(n: T) -> Bool
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=is%5Ffinite%2837%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> is_finite(37)
    
      is_finite(37)
    
        = true    [Bool]
    
  ```
* <a href="https://numbat.dev/?q=is%5Ffinite%28%2Dinf%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> is_finite(-inf)
    
      is_finite(-inf)
    
        = false    [Bool]
    
  ```
</details>

## Quantities

Defined in: `core::quantities`

### `unit_of`
Extract the unit of a quantity (the `km/h` in `20 km/h`). This can be useful in generic code, but should generally be avoided otherwise.

```nbt
fn unit_of<T: Dim>(x: T) -> T
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=unit%5Fof%2820%20km%2Fh%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> unit_of(20 km/h)
    
      unit_of(20 kilometre / hour)
    
        = 1 km/h    [Velocity]
    
  ```
</details>

### `value_of`
Extract the plain value of a quantity (the `20` in `20 km/h`). This can be useful in generic code, but should generally be avoided otherwise.

```nbt
fn value_of<T: Dim>(x: T) -> Scalar
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=value%5Fof%2820%20km%2Fh%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> value_of(20 km/h)
    
      value_of(20 kilometre / hour)
    
        = 20
    
  ```
</details>

## Chemical elements

Defined in: `chemistry::elements`

### `element` (Chemical element)
Get properties of a chemical element by its symbol or name (case-insensitive).

```nbt
fn element(pattern: String) -> ChemicalElement
```

<details>
<summary>Examples</summary>

* Get the entire element struct for hydrogen.

  <a href="https://numbat.dev/?q=element%28%22H%22%29"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> element("H")
    
      element("H")
    
        = ChemicalElement { symbol: "H", name: "Hydrogen", atomic_number: 1, group: 1, group_name: "Alkali metals", period: 1, melting_point: 13.99 K, boiling_point: 20.271 K, density: 0.00008988 g/cm³, electron_affinity: 0.754 eV, ionization_energy: 13.598 eV, vaporization_heat: 0.904 kJ/mol }    [ChemicalElement]
    
  ```
* Get the ionization energy of hydrogen.

  <a href="https://numbat.dev/?q=element%28%22hydrogen%22%29%2Eionization%5Fenergy"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> element("hydrogen").ionization_energy
    
      element("hydrogen").ionization_energy
    
        = 13.598 eV    [Energy or Torque]
    
  ```
</details>

## Mixed unit conversion

Defined in: `units::mixed`

### `DMS` (Degrees, minutes, seconds)
Convert an angle to a mixed degrees, (arc)minutes, and (arc)seconds representation. Also called sexagesimal degree notation.
More information [here](https://en.wikipedia.org/wiki/Sexagesimal_degree).

```nbt
fn DMS(alpha: Angle) -> String
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=DMS%2846%2E5858%C2%B0%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> DMS(46.5858°)
    
      DMS(46.5858 degree)
    
        = "46° 35′ 9″"    [String]
    
  ```
</details>

### `DM` (Degrees, decimal minutes)
Convert an angle to a mixed degrees and decimal minutes representation.
More information [here](https://en.wikipedia.org/wiki/Decimal_degrees).

```nbt
fn DM(alpha: Angle) -> String
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=DM%2846%2E5858%C2%B0%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> DM(46.5858°)
    
      DM(46.5858 degree)
    
        = "46° 35.148′"    [String]
    
  ```
</details>

### `feet_and_inches` (Feet and inches)
Convert a length to a mixed feet and inches representation.
More information [here](https://en.wikipedia.org/wiki/Foot_(unit)).

```nbt
fn feet_and_inches(length: Length) -> String
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=feet%5Fand%5Finches%28180cm%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> feet_and_inches(180cm)
    
      feet_and_inches(180 centimetre)
    
        = "5 ft 10.8661 in"    [String]
    
  ```
</details>

### `pounds_and_ounces` (Pounds and ounces)
Convert a mass to a mixed pounds and ounces representation.
More information [here](https://en.wikipedia.org/wiki/Pound_(mass)).

```nbt
fn pounds_and_ounces(mass: Mass) -> String
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=pounds%5Fand%5Founces%281kg%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> pounds_and_ounces(1kg)
    
      pounds_and_ounces(1 kilogram)
    
        = "2 lb 3.27396 oz"    [String]
    
  ```
</details>

## Temperature conversion

Defined in: `physics::temperature_conversion`

### `from_celsius`
Converts from degree Celsius (°C) to Kelvin.
More information [here](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature).

```nbt
fn from_celsius(t_celsius: Scalar) -> Temperature
```

<details>
<summary>Examples</summary>

* \\( 300 °C \\) in Kelvin.

  <a href="https://numbat.dev/?q=from%5Fcelsius%28300%29"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> from_celsius(300)
    
      from_celsius(300)
    
        = 573.15 K    [Temperature]
    
  ```
</details>

### `celsius`
Converts from Kelvin to degree Celcius (°C). This can be used on the right hand side of a conversion operator: `200 K -> celsius`.
More information [here](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature).

```nbt
fn celsius(t_kelvin: Temperature) -> Scalar
```

<details>
<summary>Examples</summary>

* \\( 300K \\) in degree Celsius.

  <a href="https://numbat.dev/?q=300K%20%2D%3E%20celsius"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> 300K -> celsius
    
      celsius(300 kelvin)
    
        = 26.85
    
  ```
</details>

### `from_fahrenheit`
Converts from degree Fahrenheit (°F) to Kelvin.
More information [here](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature).

```nbt
fn from_fahrenheit(t_fahrenheit: Scalar) -> Temperature
```

<details>
<summary>Examples</summary>

* \\( 300 °F \\) in Kelvin.

  <a href="https://numbat.dev/?q=from%5Ffahrenheit%28300%29"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> from_fahrenheit(300)
    
      from_fahrenheit(300)
    
        = 422.039 K    [Temperature]
    
  ```
</details>

### `fahrenheit`
Converts from Kelvin to degree Fahrenheit (°F). This can be used on the right hand side of a conversion operator: `200 K -> fahrenheit`.
More information [here](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature).

```nbt
fn fahrenheit(t_kelvin: Temperature) -> Scalar
```

<details>
<summary>Examples</summary>

* \\( 300K \\) in degree Fahrenheit.

  <a href="https://numbat.dev/?q=300K%20%2D%3E%20fahrenheit"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> 300K -> fahrenheit
    
      fahrenheit(300 kelvin)
    
        = 80.33
    
  ```
</details>

## Color format conversion

Defined in: `extra::color`

### `rgb`
Create a `Color` from RGB (red, green, blue) values in the range \\( [0, 256) \\).

```nbt
fn rgb(red: Scalar, green: Scalar, blue: Scalar) -> Color
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=use%20extra%3A%3Acolor%0Argb%28125%2C%20128%2C%20218%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> rgb(125, 128, 218)
    
      rgb(125, 128, 218)
    
        = Color { red: 125, green: 128, blue: 218 }    [Color]
    
  ```
</details>

### `color`
Create a `Color` from a (hexadecimal) value.

```nbt
fn color(rgb_hex: Scalar) -> Color
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=use%20extra%3A%3Acolor%0Acolor%280xff7700%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> color(0xff7700)
    
      color(16_742_144)
    
        = Color { red: 255, green: 119, blue: 0 }    [Color]
    
  ```
</details>

### `color_rgb`
Convert a color to its RGB representation.

```nbt
fn color_rgb(color: Color) -> String
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=use%20extra%3A%3Acolor%0Acyan%20%2D%3E%20color%5Frgb"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> cyan -> color_rgb
    
      color_rgb(cyan)
    
        = "rgb(0, 255, 255)"    [String]
    
  ```
</details>

### `color_rgb_float`
Convert a color to its RGB floating point representation.

```nbt
fn color_rgb_float(color: Color) -> String
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=use%20extra%3A%3Acolor%0Acyan%20%2D%3E%20color%5Frgb%5Ffloat"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> cyan -> color_rgb_float
    
      color_rgb_float(cyan)
    
        = "rgb(0.000, 1.000, 1.000)"    [String]
    
  ```
</details>

### `color_hex`
Convert a color to its hexadecimal representation.

```nbt
fn color_hex(color: Color) -> String
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=use%20extra%3A%3Acolor%0Argb%28225%2C%2036%2C%20143%29%20%2D%3E%20color%5Fhex"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> rgb(225, 36, 143) -> color_hex
    
      color_hex(rgb(225, 36, 143))
    
        = "#e1248f"    [String]
    
  ```
</details>

