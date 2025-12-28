---
icon: lucide/braces
---

# Structs

Numbat has compound data structures in the form of structs:

```nbt
struct Vector {
  x: Length,
  y: Length,
}

let origin   = Vector { x: 0 m, y: 0 m }
let position = Vector { x: 6 m, y: 8 m }

# A function with a struct as a parameter
fn euclidean_distance(a: Vector, b: Vector) =
  sqrt((a.x - b.x)² + (a.y - b.y)²)

assert_eq(euclidean_distance(origin, position), 10 m)

# Struct fields can be accessed using `.field` notation
let x = position.x
```
