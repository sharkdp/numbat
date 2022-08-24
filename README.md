Reasons for rewriting Insect in Rust:

  - Rust is much more popular amongst developers => more possible contributors
  - A redesign from scratch would allow me to focus on areas of improvement:
      - Introducing the concept of physical *dimensions* into the language
        ( see https://gist.github.com/sharkdp/5161bf9f46263c70ec91d66692c6864d )
        ( units ~ types, dimensions ~ kinds? )
      - Experimenting with a static dimension/unit checker => that is probably not going to work: sum(kg^i, i, 1, 10)
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
  kineticEnergy(m: #mass, v: #speed): #energy = 1/2 * m * v^2

  kineticEnergy(m: [mass], v: [speed]): [energy] = 1/2 * m * v^2

  // maybe okay, since it does not clash with other identifiers?
  kineticEnergy(m: mass, v: speed): energy = 1/2 * m * v^2

  // … or use case-sensitivity:
  kineticEnergy(m: Mass, v: Speed) : Energy = 1/2 * m * v^2

Example:

  Length l = 4 inch
  Mass m = 3 kg

Or

  reaction_time: time = 1.5 seconds
  braking_power: acceleration = 1.5 * gravity

  stopping_distance(v: speed): length = v * reaction_time + 1/2 * v^2 / braking_power

  stopping_distance(50 km/h)

What about generic functions like:

  sqr(x) -> [x]^2 = x * x

*Important question*: can we resolve all physical dimension checks at compile time?
Probably not. Take this

  sum(kg^i, i, 1, 10)

or this:

  f(l: $length, i: $scalar) = l^i


## Other ideas

include * from SI
include inch from Imperial

extern function sin :: R -> R

dimension pixel

unit meter : length

## Possible compiler optimizations

  Negate(Constant(n)) => Constant(-n)



Resources:

  https://tratt.net/laurie/blog/2020/which_parsing_approach.html
  https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
  https://blog.reverberate.org/2013/09/ll-and-lr-in-context-why-parsing-tools.html
