# Type system

Insect is a language with a special type system that treats *physical dimensions* as types.
A type checker infers types for every expression in the program and ensures that everything is correct in terms of physical dimensions, which implies correctness in terms of physical *units*.
For example, the expression `2 meter` has a *type* of `Length`.
The expression `3 inch` *also* has a type of `Length`.
The combined expression `2 meter + 3 inch` is therefore well-typed.
On the other hand, `2 meter + 3 second` is ill-typed, as `3 second` is of type `Time`.

The type system is *static* which means that the correctness of an Insect program is verified before the program starts executing.
Note that certain *runtime* errors (like division-by-zero) can still occur.

## Algebra of types


Types in Insect can be combined in various ways to produce new types.
In its most general form, a type can be thought of as a product of physical (base) dimensions \\( D_k \\) with exponents \\( \alpha_k \in \mathbb{Q} \\):
\\[ \prod_k D_k^{\alpha_k} \\]
For example, the type *Energy* can be represented as *Mass¹ × Length² × Time⁻²*.

### Multiplication

This naturally allows us to *multiply* types (by combining the factors of both products into a single product).
We can use the `*` operator to construct types for physical dimensions that are products of two or more (base) dimensions. For example:
```
dimension Time
dimension Current
dimension Charge = Current * Time
```

### Exponentiation

We can also raise units to arbitrary powers \\( n \in \mathbb{Q} \\), by simply multiplying each \\( \alpha_k \\) with \\( n \\). The syntax uses the `^` exponentiation operator:
```
dimension Length
dimension Volume = Length^3

dimension Time
dimension Frequency = Time^(-1)
```

### Division

Once we have multiplication and exponentiation, we can define the *division* of two types as
```
TypeA / TypeB ≡ TypeA * TypeB^(-1)
```
This is mostly for convenience. It allows us to write definitions like
```
dimension Power = Energy / Time
```

> **Note:** When we talk about products of types in this section, we mean actual, literal products.
> Type theory also has the notion of *product types* which denote something else: compound types — like tuples or structs — that are built by combining two or more types. If we think of types in terms of the sets of all possible values that they represent, then product types represent the Cartesian product of those.

## Type inference and type annotations

The type checker can infer the types of (most) expressions without explicitly declaring them. For example,
the following definition does not mention any types:
```rs
let E_pot = 80 kg × 9.8 m/s² × 5 m
```
However, it is often helpful to specify the type anyway. This way, we can make sure that no mistakes were made:
```rs
let E_pot: Energy = 80 kg × 9.8 m/s² × 5 m
```
The type checker will compare the inferred type with the specified type and raise an error in case of inconsistency.

Function definitions also allow for type annotations, both for the parameters as well as the return type. The following example shows a function that takes a quantity of type `Length` and returns a `Pressure`:
```rs
let p0: Pressure = 101325 Pa
let t0: Temperature = 288.15 K

let gradient = 0.65 K / 100 m

fn air_pressure(height: Length) -> Pressure = p0 · (1 - gradient · height / t0)^5.255
```


## Generic types

Insects type system also supports generic types (type polymorphism).
These can be used for functions that work regardless of the physical dimension of the argument(s).
For example, the type signature of the absolute value function is given by
```rs
fn abs<D>(x: D) -> D
```
where the angle brackets after the function name introduce new type parameters (`D`).
This can be read as: `abs` takes an arbitrary physical quantity of dimension `D` and returns a quantity of the *same* physical dimension `D`.

As a more interesting example, we can look at the `sqrt` function. Its type signature can be written as
```rs
fn sqrt<D>(x: D^2) -> D
```
Alternatively, it could also be specified as `fn sqrt<D>(x: D) -> D^(1/2)`.
