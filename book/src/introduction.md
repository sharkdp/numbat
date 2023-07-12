# Introduction

> *"You see, Vergon 6 was once filled with the super-dense substance known as dark matter, each pound of which weighs over 10,000 pounds."* — Futurama, S1E4

Numbat is a high precision scientific calculator with full support for physical units. You can use
it for simple mathematical computations:
```
>>> 1920/16*9

    = 1080 

>>> 2^32

    = 4294967296 

>>> sqrt(1.4^2 + 1.5^2) * cos(pi/3)^2

    = 0.512957 
```
The real strengh of Numbat, however, is to perform calculations with physical units:
```
>>> sin(30°)

    = 0.5 

>>> 8 km / (1 h + 25 min)

    = 5.64706 km/h

>>> 80 kg * 9.8 m/s^2 * 250 cm -> kcal

    = 0.468451 kcal

>>> 14 € -> JPY

    = 2153.76 ¥
```
