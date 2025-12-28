# Printing, testing, debugging

## Printing

Numbat has a builtin `print` procedure that can be used to print the value of an expression:

```nbt
print(2 km/h)
print(3 ft < 1 m)
```

You can also print out simple messages as strings. This is particularly useful when combined
with string interpolation to print results of a computation:

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

Format specifiers are also supported in interpolations. For instance:

```nbt
print("{pi:0.2f}")  # Prints "3.14"
```

For more information on supported format specifiers, please see
[this page](https://doc.rust-lang.org/std/fmt/#formatting-parameters).

## Testing

The `assert_eq` procedure can be used to test for (approximate) equality of two quantities.
This is often useful to make sure that (intermediate) results in longer calculations have
a certain value, e.g. when restructuring the code. The general syntax is

```nbt
assert_eq(q1, q2)
assert_eq(q1, q2, ε)
```

where the first version tests for exact equality while the second version tests for approximate
equality \\( |q_1-q_2| <= \epsilon \\) with a specified accuracy of \\( \epsilon \\).
Note that the input quantities are converted to the units of \\( \epsilon \\) before comparison.
For example:

```nbt
assert_eq(2 + 3, 5)
assert_eq(1 ft × 77 in², 4 gal)

assert_eq(alpha, 1 / 137, 1e-4)
assert_eq(3.3 ft, 1 m, 1 cm)
```

There is also a plain `assert` procedure that can test any boolean condition. For example:

```nbt
assert(1 yard < 1 meter)
assert(str_contains("bar", "foobar"))
```

A runtime error is thrown if an assertion fails. Otherwise, nothing happens.

## Debugging

You can use the builtin `type` procedure to see the type (or physical dimension) of a quantity:

```nbt
>>> type(g0)

  Length / Time²

>>> type(2 < 3)

  Bool
```
