# String-related functions

Defined in: `core::strings`

### `str_length`
The length of a string.

```nbt
fn str_length(s: String) -> Scalar
```

### `str_slice`
Subslice of a string.

```nbt
fn str_slice(s: String, start: Scalar, end: Scalar) -> String
```

### `chr`
Get a single-character string from a Unicode code point. Example: `0x2764 -> chr`.

```nbt
fn chr(n: Scalar) -> String
```

### `ord`
Get the Unicode code point of the first character in a string. Example: `"❤" -> ord`.

```nbt
fn ord(s: String) -> Scalar
```

### `lowercase`
Convert a string to lowercase.

```nbt
fn lowercase(s: String) -> String
```

### `uppercase`
Convert a string to uppercase.

```nbt
fn uppercase(s: String) -> String
```

### `str_append`
Concatenate two strings.

```nbt
fn str_append(a: String, b: String) -> String
```

### `str_find`
Find the first occurrence of a substring in a string.

```nbt
fn str_find(haystack: String, needle: String) -> Scalar
```

### `str_contains`
Check if a string contains a substring.

```nbt
fn str_contains(haystack: String, needle: String) -> Bool
```

### `str_replace`
Replace all occurrences of a substring in a string.

```nbt
fn str_replace(s: String, pattern: String, replacement: String) -> String
```

### `str_repeat`
Repeat the input string `n` times.

```nbt
fn str_repeat(a: String, n: Scalar) -> String
```

### `bin`
Get a binary representation of a number. Example: `42 -> bin`.

```nbt
fn bin(x: Scalar) -> String
```

### `oct`
Get an octal representation of a number. Example: `42 -> oct`.

```nbt
fn oct(x: Scalar) -> String
```

### `dec`
Get a decimal representation of a number.

```nbt
fn dec(x: Scalar) -> String
```

### `hex`
Get a hexadecimal representation of a number. Example: `2^31-1 -> hex`.

```nbt
fn hex(x: Scalar) -> String
```

### `base`
Convert a number to the given base. Example: `42 -> base(16)`.

```nbt
fn base(b: Scalar) -> Fn[(Scalar) -> String]
```

