# Struct types

Numbat supports nominally typed records:

```nbt
# Define a struct
struct Vector {
  x: Length,
  y: Length,
}

let v = Vector {x: 6m, y: 8m}

# A function with a struct as a parameter
fn euclidian_distance(a: Vector, b: Vector) =
  sqrt(sqr(a.x - b.x) + sqr(a.y - b.y))
  
assert_eq(
  euclidian_distance(Vector {x: 0m, y: 0m}, v),
  10m)
  
# Struct fields can be accessed using `.field` notation
let x = v.x
```
