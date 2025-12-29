---
icon: lucide/quote
---

# String-related functions

Defined in: `core::strings`

### `str_length`
The length of a string.

```nbt
fn str_length(s: String) -> Scalar
```

!!! example "Example"
    ```nbt
    str_length("Numbat")

        = 6
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=str%5Flength%28%22Numbat%22%29){ .md-button }

### `str_slice`
Subslice of a string.

```nbt
fn str_slice(start: Scalar, end: Scalar, s: String) -> String
```

!!! example "Example"
    ```nbt
    str_slice(3, 6, "Numbat")

        = "bat"    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=str%5Fslice%283%2C%206%2C%20%22Numbat%22%29){ .md-button }

### `chr`
Get a single-character string from a Unicode code point.

```nbt
fn chr(n: Scalar) -> String
```

!!! example "Example"
    ```nbt
    0x2764 -> chr

        = "❤"    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=0x2764%20%2D%3E%20chr){ .md-button }

### `ord`
Get the Unicode code point of the first character in a string.

```nbt
fn ord(s: String) -> Scalar
```

!!! example "Example"
    ```nbt
    "❤" -> ord

        = 10084
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=%22%E2%9D%A4%22%20%2D%3E%20ord){ .md-button }

### `lowercase`
Convert a string to lowercase.

```nbt
fn lowercase(s: String) -> String
```

!!! example "Example"
    ```nbt
    lowercase("Numbat")

        = "numbat"    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=lowercase%28%22Numbat%22%29){ .md-button }

### `uppercase`
Convert a string to uppercase.

```nbt
fn uppercase(s: String) -> String
```

!!! example "Example"
    ```nbt
    uppercase("Numbat")

        = "NUMBAT"    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=uppercase%28%22Numbat%22%29){ .md-button }

### `str_append`
Concatenate two strings.

```nbt
fn str_append(a: String, b: String) -> String
```

!!! example "Example"
    ```nbt
    "!" |> str_append("Numbat")

        = "Numbat!"    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=%22%21%22%20%7C%3E%20str%5Fappend%28%22Numbat%22%29){ .md-button }

!!! example "Example"
    ```nbt
    str_append("Numbat", "!")

        = "Numbat!"    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=str%5Fappend%28%22Numbat%22%2C%20%22%21%22%29){ .md-button }

### `str_prepend`
Concatenate two strings.

```nbt
fn str_prepend(a: String, b: String) -> String
```

!!! example "Example"
    ```nbt
    "Numbat" |> str_prepend("!")

        = "Numbat!"    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=%22Numbat%22%20%7C%3E%20str%5Fprepend%28%22%21%22%29){ .md-button }

!!! example "Example"
    ```nbt
    str_prepend("!", "Numbat")

        = "Numbat!"    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=str%5Fprepend%28%22%21%22%2C%20%22Numbat%22%29){ .md-button }

### `str_find`
Find the first occurrence of a substring in a string.

```nbt
fn str_find(needle: String, haystack: String) -> Scalar
```

!!! example "Example"
    ```nbt
    str_find("typed", "Numbat is a statically typed programming language.")

        = 23
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=str%5Ffind%28%22typed%22%2C%20%22Numbat%20is%20a%20statically%20typed%20programming%20language%2E%22%29){ .md-button }

### `str_contains`
Check if a string contains a substring.

```nbt
fn str_contains(needle: String, haystack: String) -> Bool
```

!!! example "Example"
    ```nbt
    str_contains("typed", "Numbat is a statically typed programming language.")

        = true    [Bool]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=str%5Fcontains%28%22typed%22%2C%20%22Numbat%20is%20a%20statically%20typed%20programming%20language%2E%22%29){ .md-button }

### `str_replace`
Replace all occurrences of a substring in a string.

```nbt
fn str_replace(pattern: String, replacement: String, s: String) -> String
```

!!! example "Example"
    ```nbt
    str_replace("statically typed programming language", "scientific calculator", "Numbat is a statically typed programming language.")

        = "Numbat is a scientific calculator."    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=str%5Freplace%28%22statically%20typed%20programming%20language%22%2C%20%22scientific%20calculator%22%2C%20%22Numbat%20is%20a%20statically%20typed%20programming%20language%2E%22%29){ .md-button }

### `str_repeat`
Repeat the input string `n` times.

```nbt
fn str_repeat(n: Scalar, a: String) -> String
```

!!! example "Example"
    ```nbt
    str_repeat(4, "abc")

        = "abcabcabcabc"    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=str%5Frepeat%284%2C%20%22abc%22%29){ .md-button }

### `base`
Convert a number to the given base.

```nbt
fn base(b: Scalar, x: Scalar) -> String
```

!!! example "Example"
    ```nbt
    42 |> base(16)

        = "2a"    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=42%20%7C%3E%20base%2816%29){ .md-button }

### `bin`
Get a binary representation of a number.

```nbt
fn bin(x: Scalar) -> String
```

!!! example "Example"
    ```nbt
    42 -> bin

        = "0b101010"    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=42%20%2D%3E%20bin){ .md-button }

### `oct`
Get an octal representation of a number.

```nbt
fn oct(x: Scalar) -> String
```

!!! example "Example"
    ```nbt
    42 -> oct

        = "0o52"    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=42%20%2D%3E%20oct){ .md-button }

### `dec`
Get a decimal representation of a number.

```nbt
fn dec(x: Scalar) -> String
```

!!! example "Example"
    ```nbt
    0b111 -> dec

        = "7"    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=0b111%20%2D%3E%20dec){ .md-button }

### `hex`
Get a hexadecimal representation of a number.

```nbt
fn hex(x: Scalar) -> String
```

!!! example "Example"
    ```nbt
    2^31-1 -> hex

        = "0x7fffffff"    [String]
    ```
    [:material-play-circle: Run this example](https://numbat.dev/?q=2%5E31%2D1%20%2D%3E%20hex){ .md-button }

