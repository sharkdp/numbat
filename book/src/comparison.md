# Comparison with other tools

The following table provides a comparison of Numbat with other scientific calculators and programming languages. This comparison
is certainly *not* objective, as we only list criteria that we consider important. If you think that a tool or language is missing
or misrepresented, please [let us know](https://github.com/sharkdp/numbat/issues).

|                                        | Numbat          | [Qalculate](https://qalculate.github.io/) | [Kalker](https://github.com/PaddiM8/kalker) | [GNU Units](https://www.gnu.org/software/units/) | [Frink](https://frinklang.org/) | [Wolfram Alpha](https://www.wolframalpha.com/) |
|----------------------------------------|-----------------|-----------|--------|-----------|-------|---------------|
| FOSS License                           | MIT, Apache-2.0 | GPL-2.0   | MIT    | GPL-3.0   | ❌     | ❌             |
| **Interfaces**                         |                 |           |        |           |       |               |
| Command-line                           | ✓               | ✓         | ✓    | ✓         | ✓     | ✓             |
| Web version                            | ✓               | ❌        | ✓     | ❌         | ❌     | ✓             |
| Graphical                              | ❌              | ✓         | ❌    | ❌         | (✓)   | ✓             |
| **Units**                              |                 |           |        |           |       |               |
| Comprehensive list of units            | ✓               | ✓         | ❌    | ✓         | ✓     | ✓             |
| Custom units                           | ✓               | ✓         | ❌    | ✓         | ✓     | ❌             |
| Physical dimensions                    | ✓               | ❌        | ❌    | ❌         | ❌     | ❌             |
| Currency conversions                   | ✓               | ✓         | ❌    | ❌         | ✓     | ✓             |
| Date and time calculations             | ✓               | ✓         | ❌    | ❌         | ✓     | ✓             |
| **Language features**                  |                 |           |        |           |       |               |
| Custom functions                       | ✓               | ✓        | ✓     | ❌         | ✓     | ❌             |
| Real programming language              | ✓               | ❌        | ❌     | ❌         | ✓     | ?             |
| Strongly typed                         | ✓               | ❌        | ❌     | ❌         | ❌     | ❌             |
| **Calculator features**                |                 |           |        |           |       |               |
| Symbolic calculations                  | ❌               | (✓)        | ❌    | ❌         | (✓)     | ✓             |
| Hex/Oct/Bin mode                       | ✓               | ✓         | ✓     | ✓         | ✓     | ✓             |
| Complex numbers                        | ❌ ([#180](https://github.com/sharkdp/numbat/issues/180))  | ✓        | ✓     | ❌         | ✓     | ✓             |
| Vectors, Matrices                      | ❌               | ✓        | ✓      | ❌         | ✓     | ✓             |

## Detailed comparison

- [Qalculate](https://qalculate.github.io/) is a fantastic calculator with a strong support for units and conversions.
  If you don't need the full power of a programming language, Qalculate is probably more feature-complete than Numbat.
- [Frink](https://frinklang.org/) is a special-purpose programming language with a focus on scientific calculations
  and units of measurement. The language is probably more powerful than Numbat, but lacks a static type system. It's also
  a imperative/OOP language, while Numbat is a functional/declarative language. Frink is not open-source.
- [GNU Units](https://www.gnu.org/software/units/) is probably the most comprehensive tool in terms of pre-defined units.
  Numbat makes it very easy to define [custom units](./unit-definitions.md). If you think that a unit should be part
  of the standard library, please [let us know](https://github.com/sharkdp/numbat/issues).
- [Wolfram Alpha](https://www.wolframalpha.com/) is a very powerful tool, but it's focused on single-line queries instead
  of longer computations. The query language lacks a strict syntax (which some might consider a feature). The tool is not
  open source and sometimes has limitations with respect to the number/size of queries you can make.

## Other interesting tools / languages

- [F#](https://fsharp.org/) is the only programming language that we know of that comes close in terms of having an
  expressive type system that is based on units of measure. In fact, Numbats type system is heavily inspired by F#,
  except that it uses physical dimensions instead of physical units on the type level. Both languages have feature
  full [type inference](./function-definitions.md#type-inference). F# is not listed above, as it's not really suitable
  as a scientific calculator.
