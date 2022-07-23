Reasons for rewriting Insect in Rust:

  - Rust is much more popular amongst developers => more possible contributors
  - A redesign from scratch would allow me to focus on areas of improvement:
      - Introducing the concept of physical *dimensions* into the language
        ( see https://gist.github.com/sharkdp/5161bf9f46263c70ec91d66692c6864d )
        ( units ~ types, dimensions ~ kinds? )
      - Experimenting with a static dimension/unit checker
      - Better parser errors
      - Allowing user-defined units => move all of the unit definitions to the Insect language
      - Automated tracking of significant digits
      - Support for rational numbers? Complex numbers? Intervals?
      - Support for binary and hexadecimal numbers, bitwise operators, etc
      - Support for notepad-style computations (Mathematica/Jupyter style)
  - The PureScript implementation is *slow*. A Rust-based parser & interpreter could be much faster. Not just
    on the command-line (startup speed!) but also on the Web (via WASM)
  - It would be a nice playground for a WASM project



See also:
  https://gist.github.com/sharkdp/c8203cf594fe16fea7962b2adbd54a87


Example: dimensionality annotations

  kineticEnergy ∷ (Mass, Speed) ➞ Energy
  kineticEnergy(mass, speed) = 0.5 * mass * speed^2

  kineticEnergy(m: $mass, v: $speed): $energy = 1/2 * m * v^2

  kineticEnergy(m: [mass], v: [speed]): [energy] = 1/2 * m * v^2

  // maybe okay, since it does not clash with other identifiers?
  kineticEnergy(m: mass, v: speed): Energy = 1/2 * m * v^2

  // … or use case-sensitivity:
  kineticEnergy(m: Mass, v: Speed) : Energy = 1/2 * m * v^2

Example:

  Length l = 4 inch
  Mass m = 3 kg
