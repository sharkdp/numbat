# String-related functions

Defined in: `core::strings`

### `str_length`
The length of a string.

```nbt
fn str_length(s: String) -> Scalar
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=str%5Flength%28%22Numbat%22%29')""></button></div><code class="language-nbt hljs numbat">>>> str_length("Numbat")

    = 6
</code></pre>

</details>

### `str_slice`
Subslice of a string.

```nbt
fn str_slice(s: String, start: Scalar, end: Scalar) -> String
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=str%5Fslice%28%22Numbat%22%2C%203%2C%206%29')""></button></div><code class="language-nbt hljs numbat">>>> str_slice("Numbat", 3, 6)

    = "bat"    [String]
</code></pre>

</details>

### `chr`
Get a single-character string from a Unicode code point.

```nbt
fn chr(n: Scalar) -> String
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=0x2764%20%2D%3E%20chr')""></button></div><code class="language-nbt hljs numbat">>>> 0x2764 -> chr

    = "❤"    [String]
</code></pre>

</details>

### `ord`
Get the Unicode code point of the first character in a string.

```nbt
fn ord(s: String) -> Scalar
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=%22%E2%9D%A4%22%20%2D%3E%20ord')""></button></div><code class="language-nbt hljs numbat">>>> "❤" -> ord

    = 10084
</code></pre>

</details>

### `lowercase`
Convert a string to lowercase.

```nbt
fn lowercase(s: String) -> String
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=lowercase%28%22Numbat%22%29')""></button></div><code class="language-nbt hljs numbat">>>> lowercase("Numbat")

    = "numbat"    [String]
</code></pre>

</details>

### `uppercase`
Convert a string to uppercase.

```nbt
fn uppercase(s: String) -> String
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=uppercase%28%22Numbat%22%29')""></button></div><code class="language-nbt hljs numbat">>>> uppercase("Numbat")

    = "NUMBAT"    [String]
</code></pre>

</details>

### `str_append`
Concatenate two strings.

```nbt
fn str_append(a: String, b: String) -> String
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=str%5Fappend%28%22Numbat%22%2C%20%22%21%22%29')""></button></div><code class="language-nbt hljs numbat">>>> str_append("Numbat", "!")

    = "Numbat!"    [String]
</code></pre>

</details>

### `str_find`
Find the first occurrence of a substring in a string.

```nbt
fn str_find(haystack: String, needle: String) -> Scalar
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=str%5Ffind%28%22Numbat%20is%20a%20statically%20typed%20programming%20language%2E%22%2C%20%22typed%22%29')""></button></div><code class="language-nbt hljs numbat">>>> str_find("Numbat is a statically typed programming language.", "typed")

    = 23
</code></pre>

</details>

### `str_contains`
Check if a string contains a substring.

```nbt
fn str_contains(haystack: String, needle: String) -> Bool
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=str%5Fcontains%28%22Numbat%20is%20a%20statically%20typed%20programming%20language%2E%22%2C%20%22typed%22%29')""></button></div><code class="language-nbt hljs numbat">>>> str_contains("Numbat is a statically typed programming language.", "typed")

    = true    [Bool]
</code></pre>

</details>

### `str_replace`
Replace all occurrences of a substring in a string.

```nbt
fn str_replace(s: String, pattern: String, replacement: String) -> String
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=str%5Freplace%28%22Numbat%20is%20a%20statically%20typed%20programming%20language%2E%22%2C%20%22statically%20typed%20programming%20language%22%2C%20%22scientific%20calculator%22%29')""></button></div><code class="language-nbt hljs numbat">>>> str_replace("Numbat is a statically typed programming language.", "statically typed programming language", "scientific calculator")

    = "Numbat is a scientific calculator."    [String]
</code></pre>

</details>

### `str_repeat`
Repeat the input string `n` times.

```nbt
fn str_repeat(a: String, n: Scalar) -> String
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=str%5Frepeat%28%22abc%22%2C%204%29')""></button></div><code class="language-nbt hljs numbat">>>> str_repeat("abc", 4)

    = "abcabcabcabc"    [String]
</code></pre>

</details>

### `base`
Convert a number to the given base.

```nbt
fn base(b: Scalar, x: Scalar) -> String
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=42%20%7C%3E%20base%2816%29')""></button></div><code class="language-nbt hljs numbat">>>> 42 |> base(16)

    = "2a"    [String]
</code></pre>

</details>

### `bin`
Get a binary representation of a number.

```nbt
fn bin(x: Scalar) -> String
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=42%20%2D%3E%20bin')""></button></div><code class="language-nbt hljs numbat">>>> 42 -> bin

    = "0b101010"    [String]
</code></pre>

</details>

### `oct`
Get an octal representation of a number.

```nbt
fn oct(x: Scalar) -> String
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=42%20%2D%3E%20oct')""></button></div><code class="language-nbt hljs numbat">>>> 42 -> oct

    = "0o52"    [String]
</code></pre>

</details>

### `dec`
Get a decimal representation of a number.

```nbt
fn dec(x: Scalar) -> String
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=0b111%20%2D%3E%20dec')""></button></div><code class="language-nbt hljs numbat">>>> 0b111 -> dec

    = "7"    [String]
</code></pre>

</details>

### `hex`
Get a hexadecimal representation of a number.

```nbt
fn hex(x: Scalar) -> String
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=2%5E31%2D1%20%2D%3E%20hex')""></button></div><code class="language-nbt hljs numbat">>>> 2^31-1 -> hex

    = "0x7fffffff"    [String]
</code></pre>

</details>

