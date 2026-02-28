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

Field access is often enough for simple data, but structs can also define behavior with methods:

```nbt
struct Element {
    name: String,
    atomic_number: Scalar,
    density: MassDensity,

    fn tungsten() -> Self = Element {
        name: "Tungsten",
        atomic_number: 74,
        density: 19.25 g/cm³,
    }

    fn cube_side_length(self, mass: Mass) -> Length =
        cbrt(mass / self.density)
}

let tungsten = Element::tungsten()
let side_length = tungsten.cube_side_length(1 kg) -> cm
print("A 1 kg tungsten cube has side length {side_length:.2}.")
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

Structs with generic type parameters can also have methods that use those type parameters, and methods can introduce additional type parameters of their own:

```nbt
struct Vec<X: Dim> {
    x: X,
    y: X,

    fn scale(self, factor: Scalar) -> Self =
        Vec { x: self.x * factor, y: self.y * factor }

    fn dot_product<Y: Dim>(self, other: Vec<Y>) -> X * Y =
        self.x * other.x + self.y * other.y
}

let v1 = Vec { x: 1 m, y: 2 m }
let v2 = Vec { x: 3 m, y: 4 m }
let v2_cm = Vec { x: 300 cm, y: 400 cm }

let v3 = v1.scale(2)  # Vec { x: 2 m, y: 4 m }
let dp_m = v1.dot_product(v2)     # 11 m²
let dp_cm = v1.dot_product(v2_cm) # 110_000 cm²
```
