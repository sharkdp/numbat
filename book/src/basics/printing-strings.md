---
icon: lucide/quote
---

# Printing and strings

Numbat has a builtin `print` procedure that can be used to display the value of an expression:

```nbt
print(2 km/h)
print(3 ft < 1 m)
```

You can also print out simple messages as strings. This is particularly useful when combined
with string interpolation to display the results of a computation:

```nbt
let radius: Length = sqrt(footballfield / 4 pi) -> meter
print("A football field would fit on a sphere of radius {radius}")
```

You can use almost every expression inside a string interpolation field. For example:

```nbt
print("3² + 4² = {hypot2(3, 4)}²")

let speed = 25 km/h
print("Speed of the bicycle: {speed} ({speed -> mph})")
```

You can use format specifiers inside the interpolation fields to control how the value is printed:

```nbt
let falling_time = 2 s
let falling_speed = g0 × falling_time -> km/h
print("After {falling_time} of free fall, the speed is {falling_speed:.1}")
```

For more information on supported format specifiers, please see
[this page](https://doc.rust-lang.org/std/fmt/#formatting-parameters).
