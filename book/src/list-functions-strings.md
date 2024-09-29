# String-related functions

Defined in: `core::strings`

### `str_length`
The length of a string.

```nbt
fn str_length(s: String) -> Scalar
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=str%5Flength%28%22Numbat%22%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> str_length("Numbat")
    
      str_length("Numbat")
    
        = 6
    
  ```
</details>

### `str_slice`
Subslice of a string.

```nbt
fn str_slice(s: String, start: Scalar, end: Scalar) -> String
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=str%5Fslice%28%22Numbat%22%2C%203%2C%206%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> str_slice("Numbat", 3, 6)
    
      str_slice("Numbat", 3, 6)
    
        = "bat"    [String]
    
  ```
</details>

### `chr`
Get a single-character string from a Unicode code point.

```nbt
fn chr(n: Scalar) -> String
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=0x2764%20%2D%3E%20chr"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> 0x2764 -> chr
    
      chr(10084)
    
        = "❤"    [String]
    
  ```
</details>

### `ord`
Get the Unicode code point of the first character in a string.

```nbt
fn ord(s: String) -> Scalar
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=%22%E2%9D%A4%22%20%2D%3E%20ord"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> "❤" -> ord
    
      ord("❤")
    
        = 10084
    
  ```
</details>

### `lowercase`
Convert a string to lowercase.

```nbt
fn lowercase(s: String) -> String
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=lowercase%28%22Numbat%22%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> lowercase("Numbat")
    
      lowercase("Numbat")
    
        = "numbat"    [String]
    
  ```
</details>

### `uppercase`
Convert a string to uppercase.

```nbt
fn uppercase(s: String) -> String
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=uppercase%28%22Numbat%22%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> uppercase("Numbat")
    
      uppercase("Numbat")
    
        = "NUMBAT"    [String]
    
  ```
</details>

### `str_append`
Concatenate two strings.

```nbt
fn str_append(a: String, b: String) -> String
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=str%5Fappend%28%22Numbat%22%2C%20%22%21%22%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> str_append("Numbat", "!")
    
      str_append("Numbat", "!")
    
        = "Numbat!"    [String]
    
  ```
</details>

### `str_find`
Find the first occurrence of a substring in a string.

```nbt
fn str_find(haystack: String, needle: String) -> Scalar
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=str%5Ffind%28%22Numbat%20is%20a%20statically%20typed%20programming%20language%2E%22%2C%20%22typed%22%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> str_find("Numbat is a statically typed programming language.", "typed")
    
      str_find("Numbat is a statically typed programming language.", "typed")
    
        = 23
    
  ```
</details>

### `str_contains`
Check if a string contains a substring.

```nbt
fn str_contains(haystack: String, needle: String) -> Bool
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=str%5Fcontains%28%22Numbat%20is%20a%20statically%20typed%20programming%20language%2E%22%2C%20%22typed%22%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> str_contains("Numbat is a statically typed programming language.", "typed")
    
      str_contains("Numbat is a statically typed programming language.", "typed")
    
        = true    [Bool]
    
  ```
</details>

### `str_replace`
Replace all occurrences of a substring in a string.

```nbt
fn str_replace(s: String, pattern: String, replacement: String) -> String
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=str%5Freplace%28%22Numbat%20is%20a%20statically%20typed%20programming%20language%2E%22%2C%20%22statically%20typed%20programming%20language%22%2C%20%22scientific%20calculator%22%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> str_replace("Numbat is a statically typed programming language.", "statically typed programming language", "scientific calculator")
    
      str_replace("Numbat is a statically typed programming language.", "statically typed programming language", "scientific calculator")
    
        = "Numbat is a scientific calculator."    [String]
    
  ```
</details>

### `str_repeat`
Repeat the input string `n` times.

```nbt
fn str_repeat(a: String, n: Scalar) -> String
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=str%5Frepeat%28%22abc%22%2C%204%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> str_repeat("abc", 4)
    
      str_repeat("abc", 4)
    
        = "abcabcabcabc"    [String]
    
  ```
</details>

### `base`
Convert a number to the given base.

```nbt
fn base(b: Scalar, x: Scalar) -> String
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=42%20%7C%3E%20base%2816%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> 42 |> base(16)
    
      base(16, 42)
    
        = "2a"    [String]
    
  ```
</details>

### `bin`
Get a binary representation of a number.

```nbt
fn bin(x: Scalar) -> String
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=42%20%2D%3E%20bin"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> 42 -> bin
    
      bin(42)
    
        = "0b101010"    [String]
    
  ```
</details>

### `oct`
Get an octal representation of a number.

```nbt
fn oct(x: Scalar) -> String
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=42%20%2D%3E%20oct"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> 42 -> oct
    
      oct(42)
    
        = "0o52"    [String]
    
  ```
</details>

### `dec`
Get a decimal representation of a number.

```nbt
fn dec(x: Scalar) -> String
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=0b111%20%2D%3E%20dec"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> 0b111 -> dec
    
      dec(7)
    
        = "7"    [String]
    
  ```
</details>

### `hex`
Get a hexadecimal representation of a number.

```nbt
fn hex(x: Scalar) -> String
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=2%5E31%2D1%20%2D%3E%20hex"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> 2^31-1 -> hex
    
      hex(2^31 - 1)
    
        = "0x7fffffff"    [String]
    
  ```
</details>

