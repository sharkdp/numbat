# Introduction

> *"You see, Vergon 6 was once filled with the super-dense substance known as dark matter, each pound of which weighs over 10,000 pounds."* — Futurama, S1E4

Numbat is a statically typed programming language for scientific computations
with first class support for physical dimensions and units.

You can use it for simple mathematical computations:
``` numbat
>>> 1920/16*9

    = 1080

>>> 2^32

    = 4294967296

>>> sqrt(1.4^2 + 1.5^2) * cos(pi/3)^2

    = 0.512957
```

The real strength of Numbat, however, is to perform calculations with physical units:

``` numbat
>>> 8 km / (1 h + 25 min)

  8 kilometer / (1 hour + 25 minute)

    = 5.64706 km/h    [Velocity]

>>> 140 € -> GBP

  140 euro ➞ british_pound

    = 120.768 £    [Money]

>>> atan2(30 cm, 1 m) -> deg

  atan2(30 centimeter, 1 meter) ➞ degree

    = 16.6992°

>>> let ω = 2π c / 660 nm

  let ω: Frequency = 2 π × c / 660 nanometer

>>> ℏ ω -> eV

  ℏ × ω ➞ electronvolt

    = 1.87855 eV    [Energy]
```

Read the [tutorial](./tutorial.md) to learn more about the language or look at some [example programs](./examples.md).
You can also jump directly to the [syntax reference](./example-numbat_syntax.md).
