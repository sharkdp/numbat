---
icon: lucide/brackets
---

# Lists

Numbat has a built-in data type for lists. The elements can be of any type, including other lists.
Lists can be created using the `[…]` syntax. For example:

```nbt
[30 cm, 110 cm, 2 m]
["a", "b", "c"]
[[1, 2], [3, 4]]
```

The type of a list is written as `List<T>`, where `T` is the type of the elements. The types of the lists
above are `List<Length>`, `List<String>`, and `List<List<Scalar>>`, respectively.

The standard library provides a [number of functions](./list-functions-lists.md) to work with lists. Some
useful things to do with lists are:
```nbt
# Get the length of a list
len([1, 2, 3])  # returns 3

# Sum all elements of a list:
sum([30 cm, 130 cm, 2 m])  # returns 360 cm

# Get the average of a list:
mean([30 cm, 130 cm, 2 m])  # returns 120 cm

# Filter a list:
filter(is_finite, [20 cm, inf, 1 m])  # returns [20 cm, 1 m]

# Map a function over a list:
map(sqr, [10 cm, 2 m])  # returns [100 cm², 4 m²]

# Generate a range of numbers:
range(1, 5)  # returns [1, 2, 3, 4, 5]

# Generate a list of evenly spaced quantities:
linspace(0 m, 1 m, 5)  # returns [0 m, 0.25 m, 0.5 m, 0.75 m, 1 m]
```
