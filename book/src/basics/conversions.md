---
icon: lucide/arrow-right-left
---

# Conversions

## Unit conversions

The conversion operator `->` attempts to convert the physical quantity on its left hand side to
the *unit of the expression* on its right hand side. This means that you can write an arbitrary
expression on the right hand side — but only the unit part will be extracted. For example:

``` numbat
# simple unit conversion:
> 120 km/h -> mph

  = 74.5645 mi/h

# expression on the right hand side:
> 120 m^3 -> km * m^2

  = 0.12 m²·km

# convert x1 to the same unit as x2:
> let x1 = 50 km / h
> let x2 = 3 m/s -> x1

  x2 = 10.8 km/h
```


!!! quote

    *"The metric system is the tool of the devil! My car gets 40 rods to the hogshead and that's the way I likes it."* — Grandpa Simpson, A Star Is Burns (S06E18)

!!! example

    To convert the mileage of Grandpa's car to miles per gallon, you can write:

    ```nbt
    >>> 40 rods / hogshead -> mpg

        = 0.00198413 mi/gal    [Length⁻²]
    ```

    And for supporters of [Marge's side](https://www.youtube.com/watch?v=Y9XcWqBl8Xs) could use something like:

    ```nbt
    100 km / (40 rods / hogshead) -> l

        = 118548 l    [Volume]
    ```

## Conversion functions

The conversion operator `->` (or `to`) can not just be used for unit conversions, but also for other types of conversions.
The way this is set up in Numbat is that you can call `x -> f` for any function `f` that takes a single argument of the same type as `x`.

The following functions are available for this purpose:

```nbt
# Convert a date and time to a Unix timestamp
now() -> unixtime

# Convert a date and time to a different timezone
now() -> tz("Asia/Kathmandu")

# Convert a duration to years, months, days, hours, minutes, seconds
10 million seconds -> human

# Convert an angle to degrees, minutes, seconds (48° 46′ 32″)
48.7756° -> DMS

# Convert an angle to degrees, decimal minutes (48° 46.536′)
48.7756° -> DM

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
