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

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=is%5Fnan%2837%29')""></button></div><code class="language-nbt hljs numbat">is_nan(37)

    = false    [Bool]
</code></pre>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=is%5Fnan%28NaN%29')""></button></div><code class="language-nbt hljs numbat">is_nan(NaN)

    = true    [Bool]
</code></pre>

</details>

### `is_infinite`
Returns true if the input is positive infinity or negative infinity.
More information [here](https://doc.rust-lang.org/std/primitive.f64.html#method.is_infinite).

```nbt
fn is_infinite<T: Dim>(n: T) -> Bool
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=is%5Finfinite%2837%29')""></button></div><code class="language-nbt hljs numbat">is_infinite(37)

    = false    [Bool]
</code></pre>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=is%5Finfinite%28%2Dinf%29')""></button></div><code class="language-nbt hljs numbat">is_infinite(-inf)

    = true    [Bool]
</code></pre>

</details>

### `is_finite`
Returns true if the input is neither infinite nor `NaN`.

```nbt
fn is_finite<T: Dim>(n: T) -> Bool
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=is%5Ffinite%2837%29')""></button></div><code class="language-nbt hljs numbat">is_finite(37)

    = true    [Bool]
</code></pre>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=is%5Ffinite%28%2Dinf%29')""></button></div><code class="language-nbt hljs numbat">is_finite(-inf)

    = false    [Bool]
</code></pre>

</details>

### `is_zero`
Returns true if the input is 0 (zero).

```nbt
fn is_zero<D: Dim>(value: D) -> Bool
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=is%5Fzero%2837%29')""></button></div><code class="language-nbt hljs numbat">is_zero(37)

    = false    [Bool]
</code></pre>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=is%5Fzero%280%29')""></button></div><code class="language-nbt hljs numbat">is_zero(0)

    = true    [Bool]
</code></pre>

</details>

### `is_nonzero`
Returns true unless the input is 0 (zero).

```nbt
fn is_nonzero<D: Dim>(value: D) -> Bool
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=is%5Fnonzero%2837%29')""></button></div><code class="language-nbt hljs numbat">is_nonzero(37)

    = true    [Bool]
</code></pre>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=is%5Fnonzero%280%29')""></button></div><code class="language-nbt hljs numbat">is_nonzero(0)

    = false    [Bool]
</code></pre>

</details>

## Quantities

Defined in: `core::quantities`

### `value_of`
Extract the plain value of a quantity (the `20` in `20 km/h`). This can be useful in generic code, but should generally be avoided otherwise.

```nbt
fn value_of<T: Dim>(x: T) -> Scalar
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=value%5Fof%2820%20km%2Fh%29')""></button></div><code class="language-nbt hljs numbat">value_of(20 km/h)

    = 20
</code></pre>

</details>

### `unit_of`
Extract the unit of a quantity (the `km/h` in `20 km/h`). This can be useful in generic code, but should generally be avoided otherwise. Returns an error if the quantity is zero.

```nbt
fn unit_of<T: Dim>(x: T) -> T
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=unit%5Fof%2820%20km%2Fh%29')""></button></div><code class="language-nbt hljs numbat">unit_of(20 km/h)

    = 1 km/h    [Velocity]
</code></pre>

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

Get the entire element struct for hydrogen.
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=element%28%22H%22%29')""></button></div><code class="language-nbt hljs numbat">element("H")

    = ChemicalElement { symbol: "H", name: "Hydrogen", atomic_number: 1, group: 1, group_name: "Alkali metals", period: 1, melting_point: 13.99 K, boiling_point: 20.271 K, density: 0.00008988 g/cm³, electron_affinity: 0.754 eV, ionization_energy: 13.598 eV, vaporization_heat: 0.904 kJ/mol }    [ChemicalElement]
</code></pre>

Get the ionization energy of hydrogen.
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=element%28%22hydrogen%22%29%2Eionization%5Fenergy')""></button></div><code class="language-nbt hljs numbat">element("hydrogen").ionization_energy

    = 13.598 eV    [Energy or Torque]
</code></pre>

</details>

## Mixed unit conversion

Defined in: `units::mixed`

### `unit_list` (Unit list)
Convert a value to a mixed representation using the provided units.

```nbt
fn unit_list<D: Dim>(units: List<D>, value: D) -> List<D>
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=5500%20m%20%7C%3E%20unit%5Flist%28%5Bmiles%2C%20yards%2C%20feet%2C%20inches%5D%29')""></button></div><code class="language-nbt hljs numbat">5500 m |> unit_list([miles, yards, feet, inches])

    = [3 mi, 734 yd, 2 ft, 7.43307 in]    [List<Length>]
</code></pre>

</details>

### `DMS` (Degrees, minutes, seconds)
Convert an angle to a mixed degrees, (arc)minutes, and (arc)seconds representation. Also called sexagesimal degree notation.
More information [here](https://en.wikipedia.org/wiki/Sexagesimal_degree).

```nbt
fn DMS(alpha: Angle) -> List<Angle>
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=46%2E5858%C2%B0%20%2D%3E%20DMS')""></button></div><code class="language-nbt hljs numbat">46.5858° -> DMS

    = [46°, 35′, 8.88″]    [List<Scalar>]
</code></pre>

</details>

### `DM` (Degrees, decimal minutes)
Convert an angle to a mixed degrees and decimal minutes representation.
More information [here](https://en.wikipedia.org/wiki/Decimal_degrees).

```nbt
fn DM(alpha: Angle) -> List<Angle>
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=46%2E5858%C2%B0%20%2D%3E%20DM')""></button></div><code class="language-nbt hljs numbat">46.5858° -> DM

    = [46°, 35.148′]    [List<Scalar>]
</code></pre>

</details>

### `feet_and_inches` (Feet and inches)
Convert a length to a mixed feet and inches representation.
More information [here](https://en.wikipedia.org/wiki/Foot_(unit)).

```nbt
fn feet_and_inches(length: Length) -> List<Length>
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=180%20cm%20%2D%3E%20feet%5Fand%5Finches')""></button></div><code class="language-nbt hljs numbat">180 cm -> feet_and_inches

    = [5 ft, 10.8661 in]    [List<Length>]
</code></pre>

</details>

### `pounds_and_ounces` (Pounds and ounces)
Convert a mass to a mixed pounds and ounces representation.
More information [here](https://en.wikipedia.org/wiki/Pound_(mass)).

```nbt
fn pounds_and_ounces(mass: Mass) -> List<Mass>
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=1%20kg%20%2D%3E%20pounds%5Fand%5Founces')""></button></div><code class="language-nbt hljs numbat">1 kg -> pounds_and_ounces

    = [2 lb, 3.27396 oz]    [List<Mass>]
</code></pre>

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

300 °C in Kelvin.
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=from%5Fcelsius%28300%29')""></button></div><code class="language-nbt hljs numbat">from_celsius(300)

    = 573.15 K    [Temperature]
</code></pre>

</details>

### `celsius`
Converts from Kelvin to degree Celcius (°C). This can be used on the right hand side of a conversion operator: `200 K -> celsius`.
More information [here](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature).

```nbt
fn celsius(t_kelvin: Temperature) -> Scalar
```

<details>
<summary>Examples</summary>

300 K in degree Celsius.
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=300K%20%2D%3E%20celsius')""></button></div><code class="language-nbt hljs numbat">300K -> celsius

    = 26.85
</code></pre>

</details>

### `from_fahrenheit`
Converts from degree Fahrenheit (°F) to Kelvin.
More information [here](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature).

```nbt
fn from_fahrenheit(t_fahrenheit: Scalar) -> Temperature
```

<details>
<summary>Examples</summary>

300 °F in Kelvin.
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=from%5Ffahrenheit%28300%29')""></button></div><code class="language-nbt hljs numbat">from_fahrenheit(300)

    = 422.039 K    [Temperature]
</code></pre>

</details>

### `fahrenheit`
Converts from Kelvin to degree Fahrenheit (°F). This can be used on the right hand side of a conversion operator: `200 K -> fahrenheit`.
More information [here](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature).

```nbt
fn fahrenheit(t_kelvin: Temperature) -> Scalar
```

<details>
<summary>Examples</summary>

300 K in degree Fahrenheit.
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=300K%20%2D%3E%20fahrenheit')""></button></div><code class="language-nbt hljs numbat">300K -> fahrenheit

    = 80.33
</code></pre>

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

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=use%20extra%3A%3Acolor%0Argb%28125%2C%20128%2C%20218%29')""></button></div><code class="language-nbt hljs numbat">use extra::color
rgb(125, 128, 218)

    = Color { red: 125, green: 128, blue: 218 }    [Color]
</code></pre>

</details>

### `color`
Create a `Color` from a (hexadecimal) value.

```nbt
fn color(rgb_hex: Scalar) -> Color
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=use%20extra%3A%3Acolor%0Acolor%280xff7700%29')""></button></div><code class="language-nbt hljs numbat">use extra::color
color(0xff7700)

    = Color { red: 255, green: 119, blue: 0 }    [Color]
</code></pre>

</details>

### `color_rgb`
Convert a color to its RGB representation.

```nbt
fn color_rgb(color: Color) -> String
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=use%20extra%3A%3Acolor%0Acyan%20%2D%3E%20color%5Frgb')""></button></div><code class="language-nbt hljs numbat">use extra::color
cyan -> color_rgb

    = "rgb(0, 255, 255)"    [String]
</code></pre>

</details>

### `color_rgb_float`
Convert a color to its RGB floating point representation.

```nbt
fn color_rgb_float(color: Color) -> String
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=use%20extra%3A%3Acolor%0Acyan%20%2D%3E%20color%5Frgb%5Ffloat')""></button></div><code class="language-nbt hljs numbat">use extra::color
cyan -> color_rgb_float

    = "rgb(0.000, 1.000, 1.000)"    [String]
</code></pre>

</details>

### `color_hex`
Convert a color to its hexadecimal representation.

```nbt
fn color_hex(color: Color) -> String
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=use%20extra%3A%3Acolor%0Argb%28225%2C%2036%2C%20143%29%20%2D%3E%20color%5Fhex')""></button></div><code class="language-nbt hljs numbat">use extra::color
rgb(225, 36, 143) -> color_hex

    = "#e1248f"    [String]
</code></pre>

</details>

