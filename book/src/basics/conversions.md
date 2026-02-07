---
icon: lucide/arrow-right-left
---

# Conversions

## Unit conversions

### Basic conversions

The conversion operator `->` can be used to convert the physical quantity on its left hand side
to a different unit. For example:

``` numbat
> 120 km/h -> mph

  = 74.5645 mph    [Velocity]

> 1600 kcal / day -> W

    = 77.4815 W    [Power]

> 4 million ฿ -> €

    = 109066 €    [Money]
```

The right hand side can also be a more complex expression:

``` numbat
> 120 m^3 -> km * m^2

  = 0.12 km·m²    [Volume]
```



!!! example "Grandpa Simpson"

    !!! quote

        *"The metric system is the tool of the devil! My car gets 40 rods to the hogshead and that's the way I likes it."* — Grandpa Simpson, A Star Is Burns (S06E18)


    To convert the mileage of Grandpa's car to miles per gallon, you can write:

    ```nbt
    >>> 40 rods / hogshead -> mpg

        = 0.00198413 mpg    [Length⁻²]
    ```

    Seeing this, we might actually want to conver to feet per gallon instead:

    ```nbt
    >>> 40 rods / hogshead -> ft / gallon

        = 10.4762 ft/gal    [Length⁻²]
    ```

    Supporters of [Marge's side](https://www.youtube.com/watch?v=Y9XcWqBl8Xs) may instead want to convert to liters per 100 km:

    ```nbt
    >>> 1 / (40 rods / hogshead) -> L / (100 km)

        = 118548 × 0.01 l/km    [Area]
    ```

    By the way, if you are curious why fuel economy is inferred as having a physical dimension of an (inverse) area,
    you might enjoy reading (the latter half of) this ["what if?" article](https://what-if.xkcd.com/11/). The first
    half of that article is also available as a
    [Numbat program](https://github.com/sharkdp/numbat/blob/main/examples/what_if_11.nbt).

### Advanced conversions

When the right hand side expression has a magnitude other than 1, the result shows how many times
the right hand side fits into the left hand side. For example, to find out how many 45-minute slots
fit into 6 hours, you can write:

``` numbat
> 6 hours -> 45 min

  = 8 × 45 min
```

!!! example "Concorde's Mach number"

    Concorde flew at a speed of 2180 km/h and had a cruising altitude of 17,000 m where the air temperature is around -60 °C.
    We can convert Concorde's velocity to the speed of sound at that temperature to find out that it cruised at
    Mach 2:

    ``` numbat
    > let concorde_speed = 2180 km/h
    > concorde_speed -> speed_of_sound(-60 °C)

      = 2.06885 × 292.701 m/s    [Velocity]
    ```

If you want to convert a quantity to the *same unit as another quantity*, you can use the `unit_of(…)` function:

``` numbat
> let v1 = 50 km / h
> let v2 = 3 m/s -> unit_of(v1)
> v2

  10.8 km/h    [Velocity]
```

### Temperature conversions

Temperature units like °C (degree Celsius) and °F (degree Fahrenheit) are special because they are not just scaled versions of the base unit (Kelvin), but also have an offset.
In Numbat, only the base unit Kelvin (`K`, `kelvin`) is an actual unit of type `Temperature`. The other temperature units can only be used to *enter* temperature values, and as
the target of a *conversion*. When you enter an expression like `25 °C`, it is immediately converted to `298.15 K`, and when `°C` and `°F` are used on the right hand side of a
unit conversion, you will only get the plain number as a result (without the unit). You can still do useful computations and conversions with these units. For example:

``` numbat
> 25 °C

    = 298.15 K    [Temperature]

> 25 °C -> °F

    = 77

>>> element("Fe").melting_point -> °C

    = 1538
```

If your keyboard layout does contain the `°` symbol, you can also use `celsius`/`fahrenheit` or `degree_celsius`/`degree_fahrenheit` instead of `°C`/`°F`.
Alternatively, you can also use Numbat's Unicode input feature and type `\degree<tab>` to get the `°` symbol.

!!! warning "Computations with °C and °F"

    You need to be extra careful when doing computations with °C and °F. Numbat does currently not prevent you from
    doing something like `10 °C + 1 °C`, even if the result is probably *not* what you expect. Since each of these
    values will be converted to Kelvin before the addition, the result will be `557.3 K` (or `284.15 °C`), and not
    `11 °C`. If you really want to add two temperatures, one of them should be a *temperature difference* expressed
    in `K` (e.g. `10 °C + 1 K`). *Subtracting* two temperatures is always fine, since any offsets will cancel out.
    For example, `20 °C - 10 °C` will correctly give a result of `10 K`.

## Conversion functions

The conversion operator `->` (or `to`) can not just be used for unit conversions, but also for other types of conversions.
The way this is set up in Numbat is that you can call `x -> f` for any function `f` that takes a single argument of the same type as `x`.

The following functions are available for this purpose:

```nbt
# Convert a date and time to a Unix timestamp
now() -> unixtime_s

# Convert a date and time to a different timezone
now() -> tz("Asia/Kathmandu")

# Convert a duration to years, months, days, hours, minutes, seconds
10 million seconds -> human

# Convert an angle to degrees, minutes, seconds (48° 46′ 32″)
48.7756° -> DMS

# Convert an angle to degrees, decimal minutes (48° 46.536′)
48.7756° -> DM

# Convert a length to feet and inches
1.75 m -> feet_and_inches

# Convert a mass to pounds and ounces
70 kg -> pounds_and_ounces

# Convert a number to its binary representation
42 -> bin

# Convert a number to its octal representation
42 -> oct

# Convert a number to its hexadecimal representation
2^31-1 -> hex

# Convert a code point number to a character
0x2764 -> chr

# Convert a character to a code point number
"❤" -> ord

# Convert a string to upper/lower case
"numbat is awesome" -> uppercase
"vier bis elf weiße Querbänder" -> lowercase
```

Note that the `tz(…)` call above *returns a function*, i.e. the right hand side of
the conversion operator is still a function.

## Alternative syntax

You can also use the `to` keyword instead of the `->` operator for conversions. Instead of the
ASCII arrow, you can also use Unicode arrows (`x → y` or `x ➞ y`).
