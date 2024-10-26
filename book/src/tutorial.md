# Tutorial

In this tutorial, you will use Numbat to calculate how many bananas you would need to power a
house. This is based on [an article](https://what-if.xkcd.com/158/) in the great *what if?* series
by the author of the xkcd comics.

<p align="center"><img src="https://what-if.xkcd.com/imgs/a/158/hazard.png"></p>

Bananas contain potassium. In its natural form, potassium contains a tiny fraction (0.0117%)
of the isotope <sup style="line-height: 0">40</sup>K, which is radioactive. The idea is to
use the radioactive decay energy as a power source. Open an interactive Numbat session by
typing `numbat` in your favorite terminal emulator. We start by entering a few [facts about
potassium-40](https://en.wikipedia.org/wiki/Potassium-40):

``` numbat
let halflife = 1.25 billion years
let occurrence = 0.0117%
let molar_mass = 40 g / mol
```

New constants are [introduced with the `let` keyword](./constant-definitions.md). We
define these physical quantities with their respective physical units (`years`,
`percent`, `g / mol`) in order to profit from Numbat's unit-safety and unit-conversion
features later on.

Our first goal is to compute the radioactivity of natural potassium. Instead of dealing with the
half-life, we want to know the decay rate. When entering the following computation, you can try
Numbat's auto-completion functionality. Instead of typing out `halflife`, just type `half` and press
`Tab`.

``` numbat
let decay_rate = ln(2) / halflife
```

As you can see, we can use typical [mathematical functions](./list-functions-math.md) such as the
natural logarithm `ln`. Next, we are interested how much radioactivity comes from a certain
mass of potassium:

``` numbat
let radioactivity =
    N_A * occurrence * decay_rate / molar_mass -> Bq / g
```

The `-> Bq / g` part at the end converts the expression to Becquerel per gram. If you type
in `radioactivity`, you should see a result of roughly `31 Bq / g`, i.e. 31 radioactive
decays per second, per gram of potassium.

The unit conversion also serves another purpose. If anything would be wrong with our
calculation at the units-level, Numbat would detect that and show an error.
Unit safety is a powerful concept not just because you can eliminate an entire category
of errors, but also because it makes your computations more readable.

We are interested in the radioactivity of bananas, so we first [introduce a new (base) unit](./unit-definitions.md):

``` numbat
unit banana
```

This lets us write readable code like

``` numbat
let potassium_per_banana = 451 mg / banana

let radioactivity_banana = potassium_per_banana * radioactivity -> Bq / banana
```

and should give you a result of roughly `14 Bq / banana`. Adding unit conversions at the end
of unit definitions is one way to enforce unit safety. An even more powerful way to do this
is to add *type annotations*: For example, to define the [decay energy for a single
potassium-40 atom](https://commons.wikimedia.org/wiki/File:Potassium-40-decay-scheme.svg),
you can optionally add a `: Energy` annotation that will be enforced by Numbat:

``` numbat
let energy_per_decay: Energy = 11% × 1.5 MeV + 89% × 1.3 MeV
```

This also works with custom units since Numbat adds new physical dimensions (types) implicitly:

``` numbat
let power_per_banana: Power / Banana = radioactivity_banana * energy_per_decay
```

You'll also notice that types can be combined via [mathematical operators](./operations.md) such as `/` in this example.

How many bananas we need to power a household is going to depend on the average power consumption
of that household. So we are [defining a simple function](./function-definitions.md)

```numbat
fn household_power(annual_consumption: Energy) -> Power = annual_consumption / year
```

This allows us to finally answer the original question (for a [typical US household in 2021](https://www.eia.gov/tools/faqs/faq.php?id=97))

``` numbat
household_power(10000 kWh) / power_per_banana
```

This should give you a result of roughly 4×10<sup style="line-height: 0">14</sup> bananas[^note].

<p align="center"><img src="https://what-if.xkcd.com/imgs/a/158/10bunches.png"></p>


## Attribution

The images in this tutorial are from [https://what-if.xkcd.com/158/](https://what-if.xkcd.com/158/). They are licensed
under the [Creative Commons Attribution-NonCommercial 2.5 License](https://creativecommons.org/licenses/by-nc/2.5/).
Details and usage notes can be found at [https://xkcd.com/license.html](https://xkcd.com/license.html).

[^note]: Interestingly, the *what if* article comes up with a result of 300 quadrillion bananas,
or 3 × 10<sup style="line-height: 0">17</sup>. This is a factor of 1000 higher. This seems like a
mistake in the original source. All of our other intermediate results are consistent with what has
been computed in the original article.
