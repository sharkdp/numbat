## Reasons why I rewrote Insect in Rust:

  - Insect is written in PureScript, which is a niche programming language with a (fantastic but) small community. Rust is much more popular amongst developers, so
    it is much easier to find contributors in a Rust project than in a PureScript project. This has been an actual problem with Insect, since I got multiple comments
    over the years that people wanted to contribute, but couldn't.
  - A redesign from scratch allowed me to focus on the following areas of improvement:
      - Introducing the concept of physical *dimensions* into the language (see https://numbat.dev/docs/advanced/dimension-definitions/)
      - Adding a static type system (see https://numbat.dev/docs/advanced/type-system/)
      - Allowing users to define their own units (https://numbat.dev/docs/advanced/unit-definitions/)
      - Being able to define constants and the whole unit system in the language itself (https://numbat.dev/docs/prelude/)
      - Much better error messages
      - Support for notepad-style computations (Mathematica/Jupyter style)
  - The PureScript implementation is *slow*. Numbat's Rust-based parser & interpreter is orders of magnitude faster, not just
    on the command-line (startup speed!) but also on the Web (via WASM)
  - I've always wanted to experiment with WASM and Numbat was a perfect excuse for this.
