# Conversion functions

The conversion operator `->` (or `to`) can not just be used for [unit conversions](./unit-conversions.md), but also for other types of conversions.
The way this is set up in Numbat is that you can call `x -> f` for any function `f` that takes a single argument of the same type as `x`.

The following functions are available for this purpose:

```nbt
# Convert a date and time to a Unix timestamp
now() -> unixtime

# Convert a date and time to a different timezone
now() -> tz("Asia/Kathmandu")

# Convert a duration to days, hours, minutes, seconds
10 million seconds -> human

# Convert a number to its binary representation
42 -> bin

# Convert a number to its octal representation
42 -> oct

# Convert a number to its hexadecimal representation
2^31-1 -> hex

# Convert a number to a custom base
42 -> base(16)

# Convert a code point number to a character
0x2764 -> chr

# Convert a string to upper/lower case
"numbat is awesome" -> uppercase
"vier bis elf weiße Querbänder" -> lowercase
```

Note that the `tz(…)` and `base(…)` calls above return *functions*, i.e. the right hand side of
the conversion operator is still a function.
