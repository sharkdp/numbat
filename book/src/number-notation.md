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

## Convert numbers to other bases

You can use the `bin`, `oct`, and `hex` functions to convert numbers to binary, octal, and hexadecimal bases, respectively.
As with any other function, you can call those using `hex(2^16 - 1)`, but it's often more convenient to use the `… // hex`
convention to convert a number.

Examples:
```nbt
0xffee // bin
42 // oct
2^16 - 1 // hex
```
