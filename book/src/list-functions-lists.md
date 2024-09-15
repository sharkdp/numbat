# List-related functions

Defined in: `core::lists`

### `len`
Get the length of a list.

```nbt
fn len<A>(xs: List<A>) -> Scalar
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=len%28%5B3%2C%202%2C%201%5D%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> len([3, 2, 1])
    
      len([3, 2, 1])
    
        = 3
    
  ```
</details>

### `head`
Get the first element of a list. Yields a runtime error if the list is empty.

```nbt
fn head<A>(xs: List<A>) -> A
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=head%28%5B3%2C%202%2C%201%5D%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> head([3, 2, 1])
    
      head([3, 2, 1])
    
        = 3
    
  ```
</details>

### `tail`
Get everything but the first element of a list. Yields a runtime error if the list is empty.

```nbt
fn tail<A>(xs: List<A>) -> List<A>
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=tail%28%5B3%2C%202%2C%201%5D%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> tail([3, 2, 1])
    
      tail([3, 2, 1])
    
        = [2, 1]    [List<Scalar>]
    
  ```
</details>

### `cons`
Prepend an element to a list.

```nbt
fn cons<A>(x: A, xs: List<A>) -> List<A>
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=cons%2877%2C%20%5B3%2C%202%2C%201%5D%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> cons(77, [3, 2, 1])
    
      cons(77, [3, 2, 1])
    
        = [77, 3, 2, 1]    [List<Scalar>]
    
  ```
</details>

### `cons_end`
Append an element to the end of a list.

```nbt
fn cons_end<A>(x: A, xs: List<A>) -> List<A>
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=cons%5Fend%2877%2C%20%5B3%2C%202%2C%201%5D%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> cons_end(77, [3, 2, 1])
    
      cons_end(77, [3, 2, 1])
    
        = [3, 2, 1, 77]    [List<Scalar>]
    
  ```
</details>

### `is_empty`
Check if a list is empty.

```nbt
fn is_empty<A>(xs: List<A>) -> Bool
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=is%5Fempty%28%5B3%2C%202%2C%201%5D%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> is_empty([3, 2, 1])
    
      is_empty([3, 2, 1])
    
        = false    [Bool]
    
  ```
* <a href="https://numbat.dev/?q=is%5Fempty%28%5B%5D%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> is_empty([])
    
      is_empty([])
    
        = true    [Bool]
    
  ```
</details>

### `concat`
Concatenate two lists.

```nbt
fn concat<A>(xs1: List<A>, xs2: List<A>) -> List<A>
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=concat%28%5B3%2C%202%2C%201%5D%2C%20%5B10%2C%2011%5D%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> concat([3, 2, 1], [10, 11])
    
      concat([3, 2, 1], [10, 11])
    
        = [3, 2, 1, 10, 11]    [List<Scalar>]
    
  ```
</details>

### `take`
Get the first `n` elements of a list.

```nbt
fn take<A>(n: Scalar, xs: List<A>) -> List<A>
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=take%282%2C%20%5B3%2C%202%2C%201%2C%200%5D%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> take(2, [3, 2, 1, 0])
    
      take(2, [3, 2, 1, 0])
    
        = [3, 2]    [List<Scalar>]
    
  ```
</details>

### `drop`
Get everything but the first `n` elements of a list.

```nbt
fn drop<A>(n: Scalar, xs: List<A>) -> List<A>
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=drop%282%2C%20%5B3%2C%202%2C%201%2C%200%5D%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> drop(2, [3, 2, 1, 0])
    
      drop(2, [3, 2, 1, 0])
    
        = [1, 0]    [List<Scalar>]
    
  ```
</details>

### `element_at`
Get the element at index `i` in a list.

```nbt
fn element_at<A>(i: Scalar, xs: List<A>) -> A
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=element%5Fat%282%2C%20%5B3%2C%202%2C%201%2C%200%5D%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> element_at(2, [3, 2, 1, 0])
    
      element_at(2, [3, 2, 1, 0])
    
        = 1
    
  ```
</details>

### `range`
Generate a range of integer numbers from `start` to `end` (inclusive).

```nbt
fn range(start: Scalar, end: Scalar) -> List<Scalar>
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=range%282%2C%2012%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> range(2, 12)
    
      range(2, 12)
    
        = [2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]    [List<Scalar>]
    
  ```
</details>

### `reverse`
Reverse the order of a list.

```nbt
fn reverse<A>(xs: List<A>) -> List<A>
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=reverse%28%5B3%2C%202%2C%201%5D%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> reverse([3, 2, 1])
    
      reverse([3, 2, 1])
    
        = [1, 2, 3]    [List<Scalar>]
    
  ```
</details>

### `map`
Generate a new list by applying a function to each element of the input list.

```nbt
fn map<A, B>(f: Fn[(A) -> B], xs: List<A>) -> List<B>
```

<details>
<summary>Examples</summary>

* Square all elements of a list.

  <a href="https://numbat.dev/?q=map%28sqr%2C%20%5B3%2C%202%2C%201%5D%29"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> map(sqr, [3, 2, 1])
    
      map(sqr, [3, 2, 1])
    
        = [9, 4, 1]    [List<Scalar>]
    
  ```
</details>

### `filter`
Filter a list by a predicate.

```nbt
fn filter<A>(p: Fn[(A) -> Bool], xs: List<A>) -> List<A>
```

<details>
<summary>Examples</summary>

* Filter all elements greater than \\( 1 \\).

  <a href="https://numbat.dev/?q=fn%20filter%5Ffn%28x%29%20%3D%20x%20%3E%201%0Afilter%28filter%5Ffn%2C%20%5B3%2C%202%2C%201%2C%200%5D%29"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> fn filter_fn(x) = x > 1
    filter(filter_fn, [3, 2, 1, 0])
    
      fn filter_fn(x: Scalar) -> Bool = x > 1
    
      filter(filter_fn, [3, 2, 1, 0])
    
        = [3, 2]    [List<Scalar>]
    
  ```
</details>

### `foldl`
Fold a function over a list.

```nbt
fn foldl<A, B>(f: Fn[(A, B) -> A], acc: A, xs: List<B>) -> A
```

<details>
<summary>Examples</summary>

* Join a list of strings by folding.

  <a href="https://numbat.dev/?q=foldl%28str%5Fappend%2C%20%22%22%2C%20%5B%22Num%22%2C%20%22bat%22%2C%20%22%21%22%5D%29"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> foldl(str_append, "", ["Num", "bat", "!"])
    
      foldl(str_append, "", ["Num", "bat", "!"])
    
        = "Numbat!"    [String]
    
  ```
</details>

### `sort_by_key`
Sort a list of elements, using the given key function that maps the element to a quantity.

```nbt
fn sort_by_key<A, D: Dim>(key: Fn[(A) -> D], xs: List<A>) -> List<A>
```

<details>
<summary>Examples</summary>

* Sort by last digit.

  <a href="https://numbat.dev/?q=fn%20map%5Ffn%28x%29%20%3D%20mod%28x%2C%2010%29%0Asort%5Fby%5Fkey%28map%5Ffn%2C%20%5B701%2C%20313%2C%209999%2C%204%5D%29"><i class="fa fa-play"></i> Run this example</a>
  ```nbt
    >>> fn map_fn(x) = mod(x, 10)
    sort_by_key(map_fn, [701, 313, 9999, 4])
    
      fn map_fn(x: Scalar) -> Scalar = mod(x, 10)
    
      sort_by_key(map_fn, [701, 313, 9999, 4])
    
        = [701, 313, 4, 9999]    [List<Scalar>]
    
  ```
</details>

### `sort`
Sort a list of quantities.

```nbt
fn sort<D: Dim>(xs: List<D>) -> List<D>
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=sort%28%5B3%2C%202%2C%207%2C%208%2C%20%2D4%2C%200%2C%20%2D5%5D%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> sort([3, 2, 7, 8, -4, 0, -5])
    
      sort([3, 2, 7, 8, -4, 0, -5])
    
        = [-5, -4, 0, 2, 3, 7, 8]    [List<Scalar>]
    
  ```
</details>

### `intersperse`
Add an element between each pair of elements in a list.

```nbt
fn intersperse<A>(sep: A, xs: List<A>) -> List<A>
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=intersperse%280%2C%20%5B1%2C%201%2C%201%2C%201%5D%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> intersperse(0, [1, 1, 1, 1])
    
      intersperse(0, [1, 1, 1, 1])
    
        = [1, 0, 1, 0, 1, 0, 1]    [List<Scalar>]
    
  ```
</details>

### `sum`
Sum all elements of a list.

```nbt
fn sum<D: Dim>(xs: List<D>) -> D
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=sum%28%5B3%2C%202%2C%201%5D%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> sum([3, 2, 1])
    
      sum([3, 2, 1])
    
        = 6
    
  ```
</details>

### `linspace`
Generate a list of `n_steps` evenly spaced numbers from `start` to `end` (inclusive).

```nbt
fn linspace<D: Dim>(start: D, end: D, n_steps: Scalar) -> List<D>
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=linspace%280%2C%2010%2C%2011%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> linspace(0, 10, 11)
    
      linspace(0, 10, 11)
    
        = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]    [List<Scalar>]
    
  ```
</details>

### `join`
Convert a list of strings into a single string by concatenating them with a separator.

```nbt
fn join(xs: List<String>, sep: String) -> String
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=join%28%5B%22snake%22%2C%20%22case%22%5D%2C%20%22%5F%22%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> join(["snake", "case"], "_")
    
      join(["snake", "case"], "_")
    
        = "snake_case"    [String]
    
  ```
</details>

### `split`
Split a string into a list of strings using a separator.

```nbt
fn split(input: String, separator: String) -> List<String>
```

<details>
<summary>Examples</summary>

* <a href="https://numbat.dev/?q=split%28%22Numbat%20is%20a%20statically%20typed%20programming%20language%2E%22%2C%20%22%20%22%29"><i class="fa fa-play"></i> Run this example</a>

  ```nbt
    >>> split("Numbat is a statically typed programming language.", " ")
    
      split("Numbat is a statically typed programming language.", " ")
    
        = ["Numbat", "is", "a", "statically", "typed", "programming", "language."]    [List<String>]
    
  ```
</details>

