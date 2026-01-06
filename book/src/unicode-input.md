---
icon: lucide/text-cursor-input
---

# Unicode input

Numbat supports a convenient way to input Unicode characters using tab-completion. Simply type a backslash `\` followed by a keyword, then press ++tab++ to convert it to the corresponding Unicode symbol. For example, typing `\alpha` and pressing ++tab++ will produce `α`.

This feature is available in both the [command-line version](cli/usage.md) and the [web version](web/usage.md).

| Input | Result | Description |
|-------|--------|-------------|
| `\alpha` | α | Greek letter alpha |
| `\beta` | β | Greek letter beta |
| `\gamma` | γ | Greek letter gamma |
| `\delta` | δ | Greek letter delta |
| `\epsilon` | ϵ | Greek letter epsilon |
| `\varepsilon` | ε | Greek letter epsilon (variant) |
| `\zeta` | ζ | Greek letter zeta |
| `\eta` | η | Greek letter eta |
| `\theta` | θ | Greek letter theta |
| `\vartheta` | ϑ | Greek letter theta (variant) |
| `\iota` | ι | Greek letter iota |
| `\kappa` | κ | Greek letter kappa |
| `\lambda` | λ | Greek letter lambda |
| `\mu` | μ | Greek letter mu |
| `\nu` | ν | Greek letter nu |
| `\xi` | ξ | Greek letter xi |
| `\pi` | π | Greek letter pi |
| `\rho` | ρ | Greek letter rho |
| `\sigma` | σ | Greek letter sigma |
| `\tau` | τ | Greek letter tau |
| `\upsilon` | υ | Greek letter upsilon |
| `\phi` | ϕ | Greek letter phi |
| `\varphi` | φ | Greek letter phi (variant) |
| `\chi` | χ | Greek letter chi |
| `\psi` | ψ | Greek letter psi |
| `\omega` | ω | Greek letter omega |
| `\Gamma` | Γ | Greek capital letter gamma |
| `\Delta` | Δ | Greek capital letter delta |
| `\Theta` | Θ | Greek capital letter theta |
| `\Lambda` | Λ | Greek capital letter lambda |
| `\Pi` | Π | Greek capital letter pi |
| `\Sigma` | Σ | Greek capital letter sigma |
| `\Phi` | Φ | Greek capital letter phi |
| `\Psi` | Ψ | Greek capital letter psi |
| `\Omega` | Ω | Greek capital letter omega |
| `\^0` … `\^9` | ⁰¹²³⁴⁵⁶⁷⁸⁹ | Superscript digits |
| `\^+` | ⁺ | Superscript plus |
| `\^-` | ⁻ | Superscript minus |
| `\_0` … `\_9` | ₀₁₂₃₄₅₆₇₈₉ | Subscript digits |
| `\_+` | ₊ | Subscript plus |
| `\_-` | ₋ | Subscript minus |
| `\pm` | ± | Plus-minus sign |
| `\cdot` | ⋅ | Dot operator |
| `\cdotp` | · | Middle dot |
| `\times` | × | Multiplication sign |
| `\div` | ÷ | Division sign |
| `\to`, `\rightarrow` | → | Right arrow |
| `\ge` | ≥ | Greater than or equal |
| `\le` | ≤ | Less than or equal |
| `\dots`, `\ldots` | … | Ellipsis |
| `\1/2` | ½ | One half |
| `\degree` | ° | Degree |
| `\arcmin` | ′ | Arc minute |
| `\arcsec` | ″ | Arc second |
| `\ohm` | Ω | Ohm |
| `\Angstrom` | Å | Angstrom |
| `\percent` | % | Percent |
| `\perthousand` | ‰ | Per mille |
| `\pertenthousand` | ‱ | Per ten thousand |
| `\dollar` | $ | US dollar |
| `\euro` | € | Euro |
| `\sterling`, `\pound` | £ | Pound sterling |
| `\yen` | ¥ | Japanese yen |
| `\rupee` | ₹ | Indian rupee |
| `\won` | ₩ | South Korean won |
| `\lira` | ₺ | Turkish lira |
| `\peso` | ₱ | Philippine peso |
| `\baht` | ฿ | Thai baht |
| `\shekel` | ₪ | Israeli shekel |
| `\hbar` | ℏ | Reduced Planck constant |
| `\planck` | ℎ | Planck constant |

!!! note
    This feature is inspired by [Julia's Unicode input system](https://docs.julialang.org/en/v1/manual/unicode-input/).
