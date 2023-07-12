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
