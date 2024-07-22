<!-- NOTE! This file is auto-generated -->

# Lists

### `len`
Get the length of a list.

```nbt
fn len<A>(xs: List<A>) -> Scalar
```
(defined in *core::lists*)

### `head`
Get the first element of a list. Yields a runtime error if the list is empty.

```nbt
fn head<A>(xs: List<A>) -> A
```
(defined in *core::lists*)

### `tail`
Get everything but the first element of a list. Yields a runtime error if the list is empty.

```nbt
fn tail<A>(xs: List<A>) -> List<A>
```
(defined in *core::lists*)

### `cons`
Prepend an element to a list.

```nbt
fn cons<A>(x: A, xs: List<A>) -> List<A>
```
(defined in *core::lists*)

### `is_empty`
Check if a list is empty.

```nbt
fn is_empty<A>(xs: List<A>) -> Bool
```
(defined in *core::lists*)

### `concat`
Concatenate two lists.

```nbt
fn concat<A>(xs1: List<A>, xs2: List<A>) -> List<A>
```
(defined in *core::lists*)

### `take`
Get the first `n` elements of a list.

```nbt
fn take<A>(n: Scalar, xs: List<A>) -> List<A>
```
(defined in *core::lists*)

### `drop`
Get everything but the first `n` elements of a list.

```nbt
fn drop<A>(n: Scalar, xs: List<A>) -> List<A>
```
(defined in *core::lists*)

### `element_at`
Get the element at index `i` in a list.

```nbt
fn element_at<A>(i: Scalar, xs: List<A>) -> A
```
(defined in *core::lists*)

### `range`
Generate a range of integer numbers from `start` to `end` (inclusive).

```nbt
fn range(start: Scalar, end: Scalar) -> List<Scalar>
```
(defined in *core::lists*)

### `cons_end`
Append an element to the end of a list.

```nbt
fn cons_end<A>(xs: List<A>, x: A) -> List<A>
```
(defined in *core::lists*)

### `reverse`
Reverse the order of a list.

```nbt
fn reverse<A>(xs: List<A>) -> List<A>
```
(defined in *core::lists*)

### `map`
Generate a new list by applying a function to each element of the input list.

```nbt
fn map<A, B>(f: Fn[(A) -> B], xs: List<A>) -> List<B>
```
(defined in *core::lists*)

### `filter`
Filter a list by a predicate.

```nbt
fn filter<A>(p: Fn[(A) -> Bool], xs: List<A>) -> List<A>
```
(defined in *core::lists*)

### `foldl`
Fold a function over a list.

```nbt
fn foldl<A, B>(f: Fn[(A, B) -> A], acc: A, xs: List<B>) -> A
```
(defined in *core::lists*)

### `sort_by_key`
Sort a list of elements, using the given key function that maps the element to a quantity.

```nbt
fn sort_by_key<A, D: Dim>(key: Fn[(A) -> D], xs: List<A>) -> List<A>
```
(defined in *core::lists*)

### `sort`
Sort a list of quantities.

```nbt
fn sort<D: Dim>(xs: List<D>) -> List<D>
```
(defined in *core::lists*)

### `intersperse`
Add an element between each pair of elements in a list.

```nbt
fn intersperse<A>(sep: A, xs: List<A>) -> List<A>
```
(defined in *core::lists*)

### `sum`
Sum all elements of a list.

```nbt
fn sum<D: Dim>(xs: List<D>) -> D
```
(defined in *core::lists*)

### `linspace`
Generate a list of `n_steps` evenly spaced numbers from `start` to `end` (inclusive).

```nbt
fn linspace<D: Dim>(start: D, end: D, n_steps: Scalar) -> List<D>
```
(defined in *core::lists*)

### `join`
Convert a list of strings into a single string by concatenating them with a separator.

```nbt
fn join(xs: List<String>, sep: String) -> String
```
(defined in *core::lists*)

### `split`
Split a string into a list of strings using a separator.

```nbt
fn split(input: String, separator: String) -> List<String>
```
(defined in *core::lists*)

