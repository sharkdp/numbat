<p align="center"><img src="assets/numbat-410.png"></p>

*Numbat* is a statically typed programming language for scientific computations with
first class support for physical units.

## Key features

  * First class support for physical units and dimensions
  * Static type checking (unit safety)
  * Type inference
  * Strict syntax
  * REPL mode
  * Customizable: the whole system of physical dimensions and units is written in Numbat itself and can be modified or replaced

## Type system

Numbat treats *physical dimensions* like length or time as *types*. A value of `5 meter` is of type `Length`. A value of `2.5 inch`
is also of type `Length`.


## Reasons for rewriting Insect in Rust:

  - Rust is much more popular amongst developers => more possible contributors
  - A redesign from scratch would allow me to focus on areas of improvement:
      - Introducing the concept of physical *dimensions* into the language
      - Experimenting with a static dimension/unit checker
      - Allowing user-defined units => move all of the unit definitions to the language
      - Better parser errors
      - Automated tracking of significant digits
      - Support for rational numbers? Complex numbers? Intervals?
      - Support for binary and hexadecimal numbers, bitwise operators, etc
      - Support for notepad-style computations (Mathematica/Jupyter style)
  - The PureScript implementation is *slow*. A Rust-based parser & interpreter could be much faster. Not just
    on the command-line (startup speed!) but also on the Web (via WASM)
  - It would be a nice playground for a WASM project

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
