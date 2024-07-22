<!-- NOTE! This file is auto-generated -->

# Core

### `unit_of`

```nbt
fn unit_of<T: Dim>(x: T) -> T
```
(defined in *core::quantities*)

### `value_of`

```nbt
fn value_of<T: Dim>(x: T) -> Scalar
```
(defined in *core::quantities*)

### `is_nan`

```nbt
fn is_nan<T: Dim>(n: T) -> Bool
```
(defined in *core::quantities*)

### `is_infinite`

```nbt
fn is_infinite<T: Dim>(n: T) -> Bool
```
(defined in *core::quantities*)

### `error`
Throw a user-defined error.

```nbt
fn error<T>(message: String) -> T
```
(defined in *core::error*)

### `random` (Standard uniform distribution sampling)
Uniformly samples the interval [0,1).

```nbt
fn random() -> Scalar
```
(defined in *core::random*)

