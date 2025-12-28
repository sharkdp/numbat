---
icon: lucide/atom
---

# Dimension definitions

New (physical) dimensions can be introduced with the `dimension` keyword. Similar like for [units](./unit-definitions.md), there are base dimensions (like *length*, *time* and *mass*) and dimensions that are derived from those base dimensions (like *momentum*, which is *mass* · *length* / *time*). Base dimensions are simply introduced by declaring their name:
``` numbat
dimension Length
dimension Time
dimension Mass
```
Derived dimensions need to specify their relation to base dimensions (or other derived dimensions). For example:
``` numbat
dimension Velocity = Length / Time
dimension Momentum = Mass * Velocity
dimension Force = Mass * Acceleration = Momentum / Time
dimension Energy = Momentum^2 / Mass = Mass * Velocity^2 = Force * Length
```
In the definition of `Force` and `Energy`, we can see that multiple *alternative definitions* can be specified. This is entirely optional. When given, the compiler will make sure that all definitions are equivalent.
