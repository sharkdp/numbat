# Operators and their precedence

  Operators (ordered by precedence: high to low)

| Operator                  | Syntax               |
| ------------------------- | -------------------- |
| square, cube, ...         | `²`, `³`, `⁻¹`, ...  |
| exponentiation            | `^`, `**`            |
| multiplication (implicit) | *whitespace*         |
| modulo                    | `%`                  |
| division                  | `per`                |
| division                  | `/`, `÷`             |
| multiplication (explicit) | `*`, `·`, `×`        |
| subtraction               | `-`                  |
| addition                  | `+`                  |
| unit conversion           | `->`, `→`, `➞`, `to` |

Note that *implicit* multiplication has a higher precedence than division, i.e. `50 cm / 2 m` will be parsed as `50 cm / (2 m)`.

Also, note that `per`-division has a higher precedence than `/`-division. This means `1 / meter  per second` will be parsed as `1 / (meter per second)`.

If in doubt, you can always look at the pretty-printing output (second line in the snippet below)
to make sure that your input was parsed correctly:
``` numbat
>>> 1 / meter per second

  1 / (meter / second)

    = 1 s/m
```
