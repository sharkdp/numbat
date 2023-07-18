# Tutorial

In this tutorial, you will use Numbat to calculate how many bananas you would need to power a
house. This is based on [an article](https://what-if.xkcd.com/158/) in the great *what if?* series
by the xkcd author.

<p align="center"><img src="https://what-if.xkcd.com/imgs/a/158/hazard.png"></p>

Bananas contain potassium. In its natural form, potassium contains a tiny fraction (0.0117%)
of the isotope <sup style="line-height: 0">40</sup>K, which is radioactive. The idea is to
use the radioactive decay energy as a power source. Open an interactive Numbat session by
typing `numbat` in your favorite terminal emulator. We start by entering a few [facts about
potassium-40](https://en.wikipedia.org/wiki/Potassium-40):

``` numbat
let halflife = 1.25 billion years
let occurence = 0.0117 percent
let molar_mass = 40 g / mol
```

New constants are introduced with the `let` keyword. We define these physical quantities
with their respective physical units (`years`, `percent`, `g / mol`) in order to profit from
Numbats unit-safety and unit-conversion features later on.

Our first goal is to compute the radioactivity of natural potassium. Instead of dealing with the
half-life, we want to know the decay rate. When entering the following computation, you can try
Numbats auto-completion functionality. Instead of typing out `halflife`, just type `half` and press
`Tab`.

``` numbat
let decay_rate = ln(2) / halflife
```

Notice that we use a [builtin mathematical function](./list-functions.md) `ln(…)`
here.



## Attribution

The images in this tutorial are from [https://what-if.xkcd.com/158/](https://what-if.xkcd.com/158/). They are licensed
under the [Creative Commons Attribution-NonCommercial 2.5 License](https://creativecommons.org/licenses/by-nc/2.5/).
Details and usage notes can be found at [https://xkcd.com/license.html](https://xkcd.com/license.html).

