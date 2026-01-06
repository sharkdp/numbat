---
icon: lucide/decimals-arrow-right
---

# Number notation

Numbers in Numbat can be written in the following forms:

* Integer notation
    * `12345`
    * `12_345` — with decimal separators
* Floating point notation
    * `0.234`
    * `.234` — without the leading zero
* Scientific notation
    * `1.234e15`
    * `1.234e+15`
    * `1e-9`
    * `1.0e-9`
* Non-decimal bases notation
    * `0x2A` — Hexadecimal
    * `0o52` — Octal
    * `0b101010` — Binary
* Non-finite numbers
    * `NaN` — Not a number
    * `inf` — Infinity

## Convert numbers to other bases

You can use the `bin`, `oct`, `dec` and `hex` functions to convert numbers to binary, octal, decimal and hexadecimal bases,
respectively. You can call those using `hex(2^16 - 1)`, but they are also available as targets of the conversion operator `->`/`to`,
so you can write expressions like:

```nbt
0xffee -> bin
42 -> oct
2^16 - 1 -> hex

# using 'to':
0xffee to bin
```

You can also use `base(b, n)` to convert a number `n` to base `b`. Using the reverse function application operator `|>` you can write
this in a similar style to the previous examples:
```nbt
273 |> base(3)
144 |> base(12)
```

## Number representation

Internally, Numbat uses [64-bit floating-point](https://en.wikipedia.org/wiki/Double-precision_floating-point_format)
numbers to represent all numeric values. This limits precision to approximately 15–16 significant decimal digits.
While this is sufficient for most scientific and engineering calculations, you might run into problems if you compute
differences between very large integers and expect to get an exact integer result.

!!! info "Future plans"
    We are planning to switch to an arbitrary-precision numeric type in the future.
    See [issue #4](https://github.com/sharkdp/numbat/issues/4) for details.
