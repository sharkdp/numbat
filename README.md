<div align="center">

<h1>Numbat</h1>

**[Features] • [Documentation] • [Syntax reference] • [Installation] • [Development]**

<img src="assets/numbat-410.png">

[Features]: #key-features
[Documentation]: https://numbat.dev/doc/
[Syntax reference]: https://numbat.dev/doc/example-numbat_syntax.html
[Installation]: #installation
[Development]: #development

</div>

*Numbat* is a statically typed programming language for scientific computations with
first class support for physical dimensions and units.

## Key features

<img src="assets/numbat-interactive.png" align="right">

*Click to learn more.*

<details>
<summary>
<b>Physical dimensions as types</b>
</summary>
<p></p>

Numbat has a static type system where physical dimensions like `Length` and `Time` *are* types.
Definitions of constants and functions can optionally contain type annotations that will be statically enforced.
If the types are not specified, they will be inferred (`Speed`, `Money` and `Frequency` on the right).

See [this article](https://numbat.dev/doc/type-system.html) to learn more about Numbats type system.
</details>

<details>
<summary>
<b>Rich set of units, constants and functions</b>
</summary>
<p></p>

Numbat comes with a comprehensive standard library that includes a large number of physical units (SI, US Customary, Imperial, Nautical, Astronomical, Atomic, Nuclear, …).
See [this reference page](https://numbat.dev/doc/list-units.html) for a complete overview. It also contains a lot of [mathematical and physical constants](https://numbat.dev/doc/list-constants.html)
as well as a large range of [mathematical functions](https://numbat.dev/doc/list-functions.html).
</details>

<details>
<summary>
<b>Strict syntax</b>
</summary>
<p></p>

Numbats parser never tries to be "smart" on syntactically incorrect input. This means you will either get a (descriptive) error message, or you can trust the result of your calculation. On the interactive terminal, we also pretty-print the user input for a quick double-check.
</details>

<details>
<summary>
<b>Excellent error messages</b>
</summary>
<p></p>

Numbat aims to provide descriptive and helpful error messages:
<img src="assets/numbat-error.png" width="500">
</details>

<details>
<summary>
<b>Focus on interactive use</b>
</summary>
<p></p>

…

</details>

<details>
<summary>
<b>Modular and customizable</b>
</summary>
<p></p>

The whole system of physical dimensions and units is specified in Numbat language and can be modified or replaced

</details>

## Installation



## Development

Run Numbat CLI
```
cargo run -- <numbat args>
```

Install the CLI version
```
cargo install -f --path numbat-cli
```

Run all tests
```
cargo test
```
