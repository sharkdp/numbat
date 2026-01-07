---
icon: lucide/help-circle
---

# Typed holes

You can use a question mark (`?`) anywhere an expression would be expected,
and the type checker will tell you what the type of the missing expression
needs to be. For example, if you type-check the following code:

```nbt
let mass = 1 kg
let f: Force = mass * ?
```

[:material-play-circle: Run this example](https://numbat.dev/?q=let+mass+%3D+1+kg%0Alet+f%3A+Force+%3D+mass+*+%3F)

The type checker will produce an error message indicating that there is
a typed hole, and it will tell you that an expression of type `Acceleration`
is expected at that position (Newton's second law):

```txt
error: Found typed hole
  ┌─ <input:2>:3:23
  │
3 │ let f: Force = mass * ?
  │                       ^ Acceleration
  │
  = Found a hole of type 'Acceleration' in the statement:
  =   let f: Force = mass × ?
```

## Dimensional analysis with typed holes

As we saw above, this can be useful if you write mathematical expressions and need to figure
out what part you are missing.

As another example, say you want to compute the potential energy of a charge `q`
at a distance `r` from another charge `Q`. You might remember that Coulomb's
law involves the product of the two charges divided by the distance, but
you might have forgotten the proportionality constant. You can write:

```nbt
let q = electron_charge
let Q = 2 electron_charge
let r = 1 a0  # Bohr radius

q Q / (? r) -> eV
```

[:material-play-circle: Run this example](https://numbat.dev/?q=let+q+%3D+electron_charge%0Alet+Q+%3D+2+electron_charge%0Alet+r+%3D+1+a0++%23+Bohr+radius%0A%0Aq+Q+%2F+(%3F+r)+-%3E+eV)

If you run the type checker on this code, it will tell you what type
is expected at the position of the question mark. If possible, it will also suggest available
bindings that match the expected type. In this case, we can see that an
electric permittivity is expected, and the type checker suggests the globally
available constant `ε0` (and its aliases) as a possible match:

```txt
error: Found typed hole
  ┌─ <input:8>:5:8
  │
5 │ q Q / (? r) -> eV
  │        ^ ElectricPermittivity
  │
  = Found a hole of type 'ElectricPermittivity' in the statement:
  =   q × Q / (? × r) ➞ electronvolt
  = Relevant matches for this hole include:
  =   eps0, ε0, electric_constant
```

!!! info

    The actual form of Coulomb's law $E = \frac{q Q}{4 \pi \epsilon_0 \, r}$
    also involves a factor of $4 \pi$, which is dimensionless, and so cannot
    be inferred using dimensional analysis.

## Arbitrary types

Using typed holes is not limited to physical quantities. You can use them to
infer arbitrary types in any expression:

```nbt
fn seq(n) = if n == 1 then [1] else cons_end(n, ?)
```

[:material-play-circle: Run this example](https://numbat.dev/?q=fn+seq(n)+%3D+if+n+%3D%3D+1+then+%5B1%5D+else+cons_end(n,+%3F))

```txt
error: Found typed hole
  ┌─ <input:2>:1:49
  │
1 │ fn seq(n) = if n == 1 then [1] else cons_end(n, ?)
  │                                                 ^ List<Scalar>
  │
  = Found a hole of type 'List<Scalar>' in the statement:
  =   fn seq(n: Scalar) -> List<Scalar> = if (n == 1) then [1] else cons_end(n, ?)
```

You can even use them to infer function types:

```nbt
fn transform(xs: List<Scalar>) -> List<String> = map(?, xs)
```

[:material-play-circle: Run this example](https://numbat.dev/?q=fn+transform(xs:+List%3CScalar%3E)+-%3E+List%3CString%3E+%3D+map(%3F,+xs))

```txt
error: Found typed hole
  ┌─ <input:2>:1:54
  │
1 │ fn transform(xs: List<Scalar>) -> List<String> = map(?, xs)
  │                                                      ^ Fn[(Scalar) -> String]
  │
  = Found a hole of type 'Fn[(Scalar) -> String]' in the statement:
  =   fn transform(xs: List<Scalar>) -> List<String> = map(?, xs)
```
