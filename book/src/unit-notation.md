# Unit notation

Most units can be entered in the same way that they would appear in textbook calculations. They
usually have a long form (`meter`, `degrees`, `byte`, …), a plural form (`meters`, `degrees`, `bytes`),
and a short alias (`m`, `°`, `B`). For a full list of supported units, see
[this page](./list-units.md).

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

Note that Numbat also allows you to [define new units](./unit-definitions.md).
