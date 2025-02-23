# Operations and precedence

Numbat operators and other language constructs, ordered by precedence form *high* to *low*:

| Operation / operator      | Syntax                               |
| ------------------------- | ------------------------------------ |
| square, cube, ...         | `x²`, `x³`, `x⁻¹`, ...               |
| factorials                | `x!`, `x!!`, `x!!!`, ...             |
| exponentiation            | `x^y`, `x**y`                        |
| multiplication (implicit) | `x y` (*whitespace*)                 |
| unary negation            | `-x`                                 |
| division                  | `x per y`                            |
| division                  | `x / y`, `x ÷ y`                     |
| multiplication (explicit) | `x * y`, `x · y`, `x × y`            |
| subtraction               | `x - y`                              |
| addition                  | `x + y`                              |
| comparisons               | `x < y`, `x <= y`, `x ≤ y`, … `x == y`, `x != y` |
| logical negation          | `!x`                                 |
| logical 'and'             | `x && y`                             |
| logical 'or'              | <code>x &#124;&#124; y</code>        |
| unit conversion           | `x -> y`, `x → y`, `x ➞ y`, `x to y` |
| conditionals              | `if x then y else z`                 |
| reverse function call     | `x \|> f`                            |

Note that *implicit* multiplication has a higher precedence than division, i.e. `50 cm / 2 m` will be parsed as `50 cm / (2 m)`.

Also, note that `per`-division has a higher precedence than `/`-division. This means `1 / meter  per second` will be parsed as `1 / (meter per second)`.

If in doubt, you can always look at the pretty-printing output (second line in the snippet below)
to make sure that your input was parsed correctly:
``` numbat
>>> 1 / meter per second

  1 / (meter / second)

    = 1 s/m
```
