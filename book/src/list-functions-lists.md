# List-related functions

Defined in: `core::lists`

### `len`
Get the length of a list.

```nbt
fn len<A>(xs: List<A>) -> Scalar
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=len%28%5B3%2C%202%2C%201%5D%29')""></button></div><code class="language-nbt hljs numbat">len([3, 2, 1])

    = 3
</code></pre>

</details>

### `head`
Get the first element of a list. Yields a runtime error if the list is empty.

```nbt
fn head<A>(xs: List<A>) -> A
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=head%28%5B3%2C%202%2C%201%5D%29')""></button></div><code class="language-nbt hljs numbat">head([3, 2, 1])

    = 3
</code></pre>

</details>

### `tail`
Get everything but the first element of a list. Yields a runtime error if the list is empty.

```nbt
fn tail<A>(xs: List<A>) -> List<A>
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=tail%28%5B3%2C%202%2C%201%5D%29')""></button></div><code class="language-nbt hljs numbat">tail([3, 2, 1])

    = [2, 1]    [List<Scalar>]
</code></pre>

</details>

### `cons`
Prepend an element to a list.

```nbt
fn cons<A>(x: A, xs: List<A>) -> List<A>
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=cons%2877%2C%20%5B3%2C%202%2C%201%5D%29')""></button></div><code class="language-nbt hljs numbat">cons(77, [3, 2, 1])

    = [77, 3, 2, 1]    [List<Scalar>]
</code></pre>

</details>

### `cons_end`
Append an element to the end of a list.

```nbt
fn cons_end<A>(x: A, xs: List<A>) -> List<A>
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=cons%5Fend%2877%2C%20%5B3%2C%202%2C%201%5D%29')""></button></div><code class="language-nbt hljs numbat">cons_end(77, [3, 2, 1])

    = [3, 2, 1, 77]    [List<Scalar>]
</code></pre>

</details>

### `is_empty`
Check if a list is empty.

```nbt
fn is_empty<A>(xs: List<A>) -> Bool
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=is%5Fempty%28%5B3%2C%202%2C%201%5D%29')""></button></div><code class="language-nbt hljs numbat">is_empty([3, 2, 1])

    = false    [Bool]
</code></pre>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=is%5Fempty%28%5B%5D%29')""></button></div><code class="language-nbt hljs numbat">is_empty([])

    = true    [Bool]
</code></pre>

</details>

### `concat`
Concatenate two lists.

```nbt
fn concat<A>(xs1: List<A>, xs2: List<A>) -> List<A>
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=concat%28%5B3%2C%202%2C%201%5D%2C%20%5B10%2C%2011%5D%29')""></button></div><code class="language-nbt hljs numbat">concat([3, 2, 1], [10, 11])

    = [3, 2, 1, 10, 11]    [List<Scalar>]
</code></pre>

</details>

### `take`
Get the first `n` elements of a list.

```nbt
fn take<A>(n: Scalar, xs: List<A>) -> List<A>
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=take%282%2C%20%5B3%2C%202%2C%201%2C%200%5D%29')""></button></div><code class="language-nbt hljs numbat">take(2, [3, 2, 1, 0])

    = [3, 2]    [List<Scalar>]
</code></pre>

</details>

### `drop`
Get everything but the first `n` elements of a list.

```nbt
fn drop<A>(n: Scalar, xs: List<A>) -> List<A>
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=drop%282%2C%20%5B3%2C%202%2C%201%2C%200%5D%29')""></button></div><code class="language-nbt hljs numbat">drop(2, [3, 2, 1, 0])

    = [1, 0]    [List<Scalar>]
</code></pre>

</details>

### `element_at`
Get the element at index `i` in a list.

```nbt
fn element_at<A>(i: Scalar, xs: List<A>) -> A
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=element%5Fat%282%2C%20%5B3%2C%202%2C%201%2C%200%5D%29')""></button></div><code class="language-nbt hljs numbat">element_at(2, [3, 2, 1, 0])

    = 1
</code></pre>

</details>

### `range`
Generate a range of integer numbers from `start` to `end` (inclusive).

```nbt
fn range(start: Scalar, end: Scalar) -> List<Scalar>
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=range%282%2C%2012%29')""></button></div><code class="language-nbt hljs numbat">range(2, 12)

    = [2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]    [List<Scalar>]
</code></pre>

</details>

### `reverse`
Reverse the order of a list.

```nbt
fn reverse<A>(xs: List<A>) -> List<A>
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=reverse%28%5B3%2C%202%2C%201%5D%29')""></button></div><code class="language-nbt hljs numbat">reverse([3, 2, 1])

    = [1, 2, 3]    [List<Scalar>]
</code></pre>

</details>

### `map`
Generate a new list by applying a function to each element of the input list.

```nbt
fn map<A, B>(f: Fn[(A) -> B], xs: List<A>) -> List<B>
```

<details>
<summary>Examples</summary>

Square all elements of a list.
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=map%28sqr%2C%20%5B3%2C%202%2C%201%5D%29')""></button></div><code class="language-nbt hljs numbat">map(sqr, [3, 2, 1])

    = [9, 4, 1]    [List<Scalar>]
</code></pre>

</details>

### `map2`
Generate a new list by applying a function to each element of the input list. This function takes two inputs: a variable, and the element of the list.

```nbt
fn map2<A, B, C>(f: Fn[(A, B) -> C], other: A, xs: List<B>) -> List<C>
```

<details>
<summary>Examples</summary>

Returns a list of bools corresponding to whether the sublist contains a 2 or not.
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=map2%28contains%2C%202%2C%20%5B%5B0%5D%2C%20%5B2%5D%2C%20%5B1%2C%202%5D%2C%20%5B0%2C%202%2C%203%5D%2C%20%5B%5D%5D%29')""></button></div><code class="language-nbt hljs numbat">map2(contains, 2, [[0], [2], [1, 2], [0, 2, 3], []])

    = [false, true, true, true, false]    [List<Bool>]
</code></pre>

</details>

### `filter`
Filter a list by a predicate.

```nbt
fn filter<A>(p: Fn[(A) -> Bool], xs: List<A>) -> List<A>
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=filter%28is%5Ffinite%2C%20%5B0%2C%201e10%2C%20NaN%2C%20%2Dinf%5D%29')""></button></div><code class="language-nbt hljs numbat">filter(is_finite, [0, 1e10, NaN, -inf])

    = [0, 10_000_000_000]    [List<Scalar>]
</code></pre>

</details>

### `foldl`
Fold a function over a list.

```nbt
fn foldl<A, B>(f: Fn[(A, B) -> A], acc: A, xs: List<B>) -> A
```

<details>
<summary>Examples</summary>

Join a list of strings by folding.
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=foldl%28str%5Fappend%2C%20%22%22%2C%20%5B%22Num%22%2C%20%22bat%22%2C%20%22%21%22%5D%29')""></button></div><code class="language-nbt hljs numbat">foldl(str_append, "", ["Num", "bat", "!"])

    = "Numbat!"    [String]
</code></pre>

</details>

### `sort_by_key`
Sort a list of elements, using the given key function that maps the element to a quantity.

```nbt
fn sort_by_key<A, D: Dim>(key: Fn[(A) -> D], xs: List<A>) -> List<A>
```

<details>
<summary>Examples</summary>

Sort by last digit.
<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=fn%20last%5Fdigit%28x%29%20%3D%20mod%28x%2C%2010%29%0Asort%5Fby%5Fkey%28last%5Fdigit%2C%20%5B701%2C%20313%2C%209999%2C%204%5D%29')""></button></div><code class="language-nbt hljs numbat">fn last_digit(x) = mod(x, 10)
sort_by_key(last_digit, [701, 313, 9999, 4])

    = [701, 313, 4, 9999]    [List<Scalar>]
</code></pre>

</details>

### `sort`
Sort a list of quantities in ascending order.

```nbt
fn sort<D: Dim>(xs: List<D>) -> List<D>
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=sort%28%5B3%2C%202%2C%207%2C%208%2C%20%2D4%2C%200%2C%20%2D5%5D%29')""></button></div><code class="language-nbt hljs numbat">sort([3, 2, 7, 8, -4, 0, -5])

    = [-5, -4, 0, 2, 3, 7, 8]    [List<Scalar>]
</code></pre>

</details>

### `contains`
Returns true if the element `x` is in the list `xs`.

```nbt
fn contains<A>(x: A, xs: List<A>) -> Bool
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=%5B3%2C%202%2C%207%2C%208%2C%20%2D4%2C%200%2C%20%2D5%5D%20%7C%3E%20contains%280%29')""></button></div><code class="language-nbt hljs numbat">[3, 2, 7, 8, -4, 0, -5] |> contains(0)

    = true    [Bool]
</code></pre>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=%5B3%2C%202%2C%207%2C%208%2C%20%2D4%2C%200%2C%20%2D5%5D%20%7C%3E%20contains%281%29')""></button></div><code class="language-nbt hljs numbat">[3, 2, 7, 8, -4, 0, -5] |> contains(1)

    = false    [Bool]
</code></pre>

</details>

### `unique`
Remove duplicates from a given list.

```nbt
fn unique<A>(xs: List<A>) -> List<A>
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=unique%28%5B1%2C%202%2C%202%2C%203%2C%203%2C%203%5D%29')""></button></div><code class="language-nbt hljs numbat">unique([1, 2, 2, 3, 3, 3])

    = [1, 2, 3]    [List<Scalar>]
</code></pre>

</details>

### `intersperse`
Add an element between each pair of elements in a list.

```nbt
fn intersperse<A>(sep: A, xs: List<A>) -> List<A>
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=intersperse%280%2C%20%5B1%2C%201%2C%201%2C%201%5D%29')""></button></div><code class="language-nbt hljs numbat">intersperse(0, [1, 1, 1, 1])

    = [1, 0, 1, 0, 1, 0, 1]    [List<Scalar>]
</code></pre>

</details>

### `sum`
Sum all elements of a list.

```nbt
fn sum<D: Dim>(xs: List<D>) -> D
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=sum%28%5B3%20m%2C%20200%20cm%2C%201000%20mm%5D%29')""></button></div><code class="language-nbt hljs numbat">sum([3 m, 200 cm, 1000 mm])

    = 6 m    [Length]
</code></pre>

</details>

### `linspace`
Generate a list of `n_steps` evenly spaced numbers from `start` to `end` (inclusive).

```nbt
fn linspace<D: Dim>(start: D, end: D, n_steps: Scalar) -> List<D>
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=linspace%28%2D5%20m%2C%205%20m%2C%2011%29')""></button></div><code class="language-nbt hljs numbat">linspace(-5 m, 5 m, 11)

    = [-5 m, -4 m, -3 m, -2 m, -1 m, 0 m, 1 m, 2 m, 3 m, 4 m, 5 m]    [List<Length>]
</code></pre>

</details>

### `join`
Convert a list of strings into a single string by concatenating them with a separator.

```nbt
fn join(xs: List<String>, sep: String) -> String
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=join%28%5B%22snake%22%2C%20%22case%22%5D%2C%20%22%5F%22%29')""></button></div><code class="language-nbt hljs numbat">join(["snake", "case"], "_")

    = "snake_case"    [String]
</code></pre>

</details>

### `split`
Split a string into a list of strings using a separator.

```nbt
fn split(input: String, separator: String) -> List<String>
```

<details>
<summary>Examples</summary>

<pre><div class="buttons"><button class="fa fa-play play-button" title="Run this code" aria-label="Run this code"  onclick=" window.open('https://numbat.dev/?q=split%28%22Numbat%20is%20a%20statically%20typed%20programming%20language%2E%22%2C%20%22%20%22%29')""></button></div><code class="language-nbt hljs numbat">split("Numbat is a statically typed programming language.", " ")

    = ["Numbat", "is", "a", "statically", "typed", "programming", "language."]    [List<String>]
</code></pre>

</details>

