---
icon: lucide/git-branch
---

# Conditionals

Numbat has `if-then-else` conditional expressions with the following
syntax
``` nbt
if <cond> then <expr1> else <expr2>
```
where `<cond>` is a condition that evaluates to a Boolean value, like
`3 ft < 3 m`. The types of `<expr1>` and `<expr2>` need to match.

For example, you can define a simple step function using

```nbt
fn step(x) = if x < 0 then 0 else 1
```
