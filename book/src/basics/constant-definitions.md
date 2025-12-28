# Constants

New constants can be introduced with the `let` keyword:
```nbt
let pipe_radius = 1 cm
let pipe_length = 10 m
let Δp = 0.1 bar
```

Definitions may contain a type annotation after the identifier (`let Δp: Pressure = 0.1 bar`). This annotation will be verified by the type checker. For more complex definitions
it can be desirable to add type annotations, as it often improves readability and allows
you to catch potential errors early:
```nbt
let μ_water: DynamicViscosity = 1 mPa·s
let Q: FlowRate = π × pipe_radius^4 × Δp / (8 μ_water × pipe_length)
```
