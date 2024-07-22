# String-related functions

Defined in: `core::strings`

### `str_length`

```nbt
fn str_length(s: String) -> Scalar
```

### `str_slice`

```nbt
fn str_slice(s: String, start: Scalar, end: Scalar) -> String
```

### `chr`

```nbt
fn chr(n: Scalar) -> String
```

### `lowercase`

```nbt
fn lowercase(s: String) -> String
```

### `uppercase`

```nbt
fn uppercase(s: String) -> String
```

### `str_append`

```nbt
fn str_append(a: String, b: String) -> String
```

### `str_find`

```nbt
fn str_find(haystack: String, needle: String) -> Scalar
```

### `str_contains`

```nbt
fn str_contains(haystack: String, needle: String) -> Bool
```

### `str_replace`

```nbt
fn str_replace(s: String, pattern: String, replacement: String) -> String
```

### `str_repeat`

```nbt
fn str_repeat(a: String, n: Scalar) -> String
```

### `bin`

```nbt
fn bin(x: Scalar) -> String
```

### `oct`

```nbt
fn oct(x: Scalar) -> String
```

### `dec`

```nbt
fn dec(x: Scalar) -> String
```

### `hex`

```nbt
fn hex(x: Scalar) -> String
```

### `base`

```nbt
fn base(b: Scalar) -> Fn[(Scalar) -> String]
```

