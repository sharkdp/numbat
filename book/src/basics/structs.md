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
struct Vec<X: Dim> {
    x: X,
    y: X,
}

let position = Vec { x: 1 m, y: 2 m }
let velocity: Vec<Velocity> = Vec { x: 1 m/s, y: 2 m/s }
```
