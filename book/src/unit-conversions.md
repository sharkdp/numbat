# Unit conversions

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
> x1 = 50 km / h
> x2 = 3 m/s -> x1

  x2 = 10.8 km/h
```
