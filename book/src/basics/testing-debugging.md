---
icon: lucide/bug-play
---

# Testing and debugging

## Testing

The `assert_eq` procedure can be used to test for (approximate) equality of two quantities.
This is often useful to make sure that (intermediate) results in longer calculations have
a certain value, e.g. when restructuring the code. The general syntax is

```nbt
assert_eq(q1, q2)
assert_eq(q1, q2, ε)
```

where the first version tests for exact equality while the second version tests for approximate
equality \( |q_1-q_2| <= \epsilon \) with a specified accuracy of \( \epsilon \).
Note that the input quantities are converted to the units of \( \epsilon \) before comparison.
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

>>> type(sqrt)

  forall A: Dim. Fn[(A²) -> A]
```

The `inspect` function can be used to print the value of an expression while
returning it unchanged. This is useful for debugging intermediate values in a
longer computation without restructuring the code:

```nbt
>>> inspect(1 + 1) * 3
inspect: 2
6

>>> [1, 2, 3] |> map(inspect) |> sum
inspect: 1
inspect: 2
inspect: 3
6
```
