# Struct types

Numbat supports structurally typed records:

```nbt
# A value level struct
${x: Length, y: Length}

# A function with a struct as a parameter
fn euclidian_distance(a: ${x: Length, y: Length}, b: ${x: Length, y: Length}) =
  sqrt(sqr(a.x - b.x) + sqr(a.y - b.y))
  
assert_eq(
  euclidian_distance(${x: 0m, y: 0m}, ${x: 6m, y: 8m}),
  10m)
  
let r = ${foo: 1}

# Struct fields can be accessed using `.field` notation
let x = r.foo
```
