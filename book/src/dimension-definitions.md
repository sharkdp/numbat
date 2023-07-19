# Dimension definitions

New (physical) dimensions can be introduced with the `dimension` keyword. Similar to [units](./unit-definitions.md), there are base dimensions (like *length*, *time* and *mass*) and dimensions that are derived from those base dimensions (like *momentum*, which is *mass* · *length* / *time*). Base dimensions are simply introduced by declaring their name:
``` numbat
dimension Length
dimension Time
dimension Mass
```
Derived dimensions need to specify their relation to base dimensions (or other derived dimensions). For example:
``` numbat
dimension Speed = Length / Time
dimension Momentum = Mass * Speed
dimension Force = Mass * Acceleration = Momentum / Time
dimension Energy = Momentum^2 / Mass = Mass * Speed^2 = Force * Length
```
In the definition of `Force` and `Energy`, we can see that *alternative definitions* can be given. This is entirely optional. If specified, the compiler will make sure that all definitions are equivalent.

## Custom dimensions

It is often useful to introduce 'fictional' physical dimensions. For example, we might want to do calculations with
screen resolutions and 'dot densities'. Introducing a new dimension for *dots* then allows us to define units like `dpi` without sacrificing unit safety:
``` numbat
dimension Dot

@aliases(dots)
unit dot: Dot

unit dpi = dots / inch

fn inter_dot_spacing(resolution: Dot / Length) -> Length = 1 dot / resolution

inter_dot_spacing(72 dpi) -> µm  # 353 µm
```
