---
icon: lucide/braces
---

# Structs

Numbat has compound data structures in the form of structs. A struct can be defined using the `struct` keyword, followed by the list of fields and their types. For example:

```nbt
struct Element {
    name: String,
    atomic_number: Scalar,
    density: MassDensity,
}
```

Structs can be instantiated by providing values for each field:

```nbt
let tungsten = Element {
    name: "Tungsten",
    atomic_number: 74,
    density: 19.25 g/cm³,
}
```

Fields can be accessed using dot notation:

```nbt
let mass = 1 kg
let side_length = cbrt(mass / tungsten.density) -> cm

print("A tungsten cube with a mass of {mass} has a side length of {side_length:.2}.")
```

## Methods

You can define methods on structs using `impl` blocks. Methods take `self` as their first parameter and are called using dot notation:

```nbt
impl Element {
    fn cube_side_length(self, mass: Mass) -> Length =
        cbrt(mass / self.density)
}

tungsten.cube_side_length(1 kg) -> cm   # 3.72 cm
```

## Generic structs

Structs can be generic over type parameters. Type parameters are declared in angle brackets after the struct name:

```nbt
struct Tuple<A, B> {
    first: A,
    second: B,
}

let t = Tuple { first: "hello", second: 42 }
```

If you want to constrain a type parameter to be a dimension type, use the `Dim` bound:

```nbt
struct Vec<D: Dim> {
    x: D,
    y: D,
}

let position = Vec { x: 1 m, y: 2 m }
let velocity: Vec<Velocity> = Vec { x: 1 m/s, y: 2 m/s }
```

For generic structs, the type parameters are declared on the `impl` block. Methods can also be generic over additional type parameters:

```nbt
impl<D: Dim> Vec<D> {
    fn norm(self) -> D² = self.x² + self.y²

    fn multiply<S: Dim>(self, factor: S) -> Vec<S × D> =
      Vec {
          x: factor × self.x,
          y: factor × self.y,
      }
}

let p = Vec { x: 3 m, y: 4 m }
p.norm()          # 25 m²
p.multiply(10 N)  # Vec { x: 30 N·m, y: 40 N·m }
```
