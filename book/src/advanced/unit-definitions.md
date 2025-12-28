---
icon: lucide/pencil-ruler
---

# Unit definitions

New units of measurement can be introduced with the `unit` keyword. There are two types of units: base units and derived units.

A new **base unit** can be defined by specifying the *physical dimension* it represents. For example, in the
 [International System of Units (SI)](https://en.wikipedia.org/wiki/International_System_of_Units), the *second* is the base unit for measuring times:
``` numbat
unit second: Time
```
Here, `Time` denotes the physical dimension. To learn more, you can read the [corresponding chapter](dimension-definitions.md). But for now, we can just assume that they are already given.

**Derived units** are also introduced with the `unit` keyword. But unlike base units, they are defined through their relation to
other units. For example, a *minute* can be defined as
``` numbat
unit minute: Time = 60 second
```
Here, the `: Time` annotation is optional. If a dimension is specified, it will be used to verify that the right hand side expression (`60 second`) is indeed of physical dimension `Time`. This is apparent in this simple example, but can be useful for more complicated unit definitions like
``` numbat
unit farad: Capacitance = ampere^2 second^4 / (kilogram meter^2)
```


## Prefixes

If a unit may be used with metric prefixes such as `milli`/`m`, `kilo`/`k` or `mega`/`M`, we can prepend the unit definition with the `@metric_prefixes` decorator:
``` numbat
@metric_prefixes
unit second: Time
```
This allows identifiers such as `millisecond` to be used in calculations. See the section below how prefixes interact with aliases.

Similarly, if a unit should be prependable with *binary* (IEC) prefixes such as `kibi`/`Ki`, `mebi`/`Mi` or `gibi`/`Gi`, you can
add the `@binary_prefixes` decorator. A unit might also allow for both metric and binary prefixes, for example:
``` numbat
@binary_prefixes
@metric_prefixes
unit byte = 8 bit
```
This allows the usage of both `mebibyte` (1024² byte) as well as `megabyte` (1000² byte).

## Aliases

It is often useful to define alternative names for a unit. For example, we might want to use the plural form `seconds` or the commonly
used short version `s`. We can use the `@aliases` decorator to specify them:
``` numbat
@metric_prefixes
@aliases(meters, metre, metres, m: short)
unit meter: Length
```
In addition to the name, we can also specify how aliases interact with prefixes using `: long` (the default), `: short`, `: both` or
`: none`. The actual unit name (`meter`) and all `long` aliases will accept the long version of prefixes (..., `milli`, `kilo`, `mega`, `giga`, ...).
All `short` aliases (`m` in the example above) will only accept the respective short versions of the prefixes (..., `m`, `k`, `M`, `G`, ...).
Aliases annotated with `: both` or `: none` accept either both long *and* short prefixes, or none of them.
The unit definition above allows all of following expressions:
``` numbat
millimeter
kilometer

millimeters
kilometers

millimetre
kilometre

millimetres
kilometres

mm
km
...
```

## Ad-hoc units

It is often useful to introduce 'fictional' physical units (and dimensions).
This comes up frequently when you want to count things. For example:
``` numbat
unit book

@aliases(pages)
unit page

@aliases(words)
unit word

let words_per_book = 500 words/page × 300 pages/book
```
Note that those base unit definitions will implicitly create new [dimensions](./dimension-definitions.md) which are capitalized
versions of the unit names (`Book`, `Page`, `Word`). A definition like `unit book` is a shorthand
for `dimension Book; unit book: Book`.
Those units now allow us to count books, pages
and words independently without any risk of mixing them. The `words_per_book` constant in this
examples has a type of `Word / Book`.

Another example shows how we introduce a `dot` unit to do calculations with
screen resolutions:
``` numbat
@aliases(dots)
unit dot

unit dpi = dots / inch

# Note: a `Dot` dimension was implicitly created for us
fn inter_dot_spacing(resolution: Dot / Length) -> Length = 1 dot / resolution

inter_dot_spacing(72 dpi) -> µm  # 353 µm
```
