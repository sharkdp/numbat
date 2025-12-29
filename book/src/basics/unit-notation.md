---
icon: lucide/ruler
---

# Unit notation

Most units can be entered in the same way that they would appear in textbook calculations. They
usually have a long form (`meter`, `degrees`, `byte`, …), a plural form (`meters`, `degrees`, `bytes`),
and a short alias (`m`, `°`, `B`). For a full list of supported units, see
[this page](../prelude/list-units.md).

All SI-accepted units support [metric prefixes](https://en.wikipedia.org/wiki/Metric_prefix) (`mm`, `cm`, `km`, ... or `millimeter`, `centimeter`, `kilometer`, ...)
and — where sensible — units allow for [binary prefixes](https://en.wikipedia.org/wiki/Binary_prefix) (`MiB`, `GiB`, ... or `mebibyte`, `gibibyte`, ...). Note
that the short-form prefixes can only be used with the short version of the unit, and vice versa (that is: `kmeter` and `kilom` are *not* allowed, only `km` and `kilometer`).

Units can be combined using [mathematical operations](./operations.md) such as multiplication, division and exponentiation: `kg * m/s^2`, `km/h`, `m²`, `meter per second`.

The following snippet shows various styles of entering units:
```nbt
2 min + 1 s
150 cm
sin(30°)
50 mph
6 MiB

2 minutes + 1 second
150 centimeters
sin(30 degrees)
50 miles per hour
6 mebibyte
```

Note that Numbat also allows you to [define new units](../advanced/unit-definitions.md).

!!! quote

    In metric, one milliliter of water occupies one cubic centimeter, weighs one gram, and requires one calorie of energy to heat up by one degree centigrade — which is 1 percent of the difference between its freezing point and its boiling point. An amount of hydrogen weighing the same amount has exactly one mole of atoms in it. Whereas in the American system, the answer to *‘How much energy does it take to boil a room-temperature gallon of water?’* is *‘Go fuck yourself,’* because you can’t directly relate any of those quantities.

    Josh Bazell, Wild Thing

!!! example "How much energy does it take to boil a room-temperature gallon of water?"

    The amount of heat needed to uniformly raise the temperature of a substance by \( \Delta T \)
    is given by \( Q = m \cdot c \cdot \Delta T \), where \( m \) is the mass and \( c \) is the specific heat
    capacity of the substance.

    ```nbt
    let density_water = 1 kg / L  # at sea level
    let mass_water = 1 gallon × density_water

    let c_water = 1 cal / g K

    let ΔT = from_fahrenheit(212) - from_fahrenheit(70)

    let heat = mass_water × c_water × ΔT

    print("Energy to boil 1 gallon of room-temperature water:")
    print("  {heat -> kJ}")
    print("  {heat -> BTU}")
    print("  {heat -> kWh}")
    ```

    [:material-play-circle: Run this example](https://numbat.dev/?q=let+density_water+%3D+1+kg+%2F+L++%23+at+sea+level%0Alet+mass_water+%3D+1+gallon+%C3%97+density_water%0A%0Alet+c_water+%3D+1+cal+%2F+g+K%0A%0Alet+%CE%94T+%3D+from_fahrenheit%28212%29+-+from_fahrenheit%2870%29%0A%0Alet+heat+%3D+mass_water+%C3%97+c_water+%C3%97+%CE%94T%0A%0Aprint%28%22Energy+to+boil+1+gallon+of+room-temperature+water%3A%22%29%0Aprint%28%22++%7Bheat+-%3E+kJ%7D%22%29%0Aprint%28%22++%7Bheat+-%3E+BTU%7D%22%29%0Aprint%28%22++%7Bheat+-%3E+kWh%7D%22%29%E2%8F%8E){ .md-button .md-button--primary }
