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

print("A {tungsten.name} cube with a mass of {mass} has a side length of {side_length:.2}.")
```
