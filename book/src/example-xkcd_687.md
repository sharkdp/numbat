<!-- This file is autogenerated! Do not modify it -->

# XKCD 687
<a href="https://numbat.dev/?q=%23+Dimensional+analysis%0A%23%0A%23+https%3A%2F%2Fxkcd.com%2F687%2F%0A%0Alet+core_pressure+%3D+3.5+million+atmospheres%0Alet+prius_milage+%3D+50+miles+per+gallon%0Alet+min_width_channel+%3D+21+miles%0A%0A%23+Make+sure+that+the+result+is+dimensionless%3A%0Alet+r%3A+Scalar+%3D%0A++++planck_energy+%2F+core_pressure+%C3%97+prius_milage+%2F+min_width_channel%0A%0Aprint%28%22%7Br%7D+%E2%89%88+%CF%80+%3F%22%29%0A"><i class="fa fa-play"></i> Run this example</a>

``` numbat
# Dimensional analysis
#
# https://xkcd.com/687/

let core_pressure = 3.5 million atmospheres
let prius_milage = 50 miles per gallon
let min_width_channel = 21 miles

# Make sure that the result is dimensionless:
let r: Scalar =
    planck_energy / core_pressure × prius_milage / min_width_channel

print("{r} ≈ π ?")
```

<p align="center" style="margin-top: 2em"><a href="https://xkcd.com/687/"><img src="https://imgs.xkcd.com/comics/dimensional_analysis.png" alt="XKCD 687" style="max-width: 100%"></a><br>Source: <a href="https://xkcd.com/687/">https://xkcd.com/687/</a></p>
