# Installation

## From pre-built binaries (Linux, macOS, Windows)

There are pre-built binaries for lots of different operating systems and platforms
available [on the release page](https://github.com/sharkdp/numbat/releases).

The sections below refer to more specific installation methods.

## Linux

### Arch Linux

In Arch Linux and Arch based distributions, you can install the prebuilt package of Numbat from the
[AUR](https://aur.archlinux.org/packages/numbat-bin).

```bash
yay -S numbat-bin
```

You can also install the [numbat](https://aur.archlinux.org/packages/numbat)
AUR package, which will download the source and compile it.

```bash
yay -S numbat
```

## From source

Clone the Git repository, and build Numbat with `cargo`:

``` bash
git clone https://github.com/sharkdp/numbat
cd numbat/
cargo install -f --path numbat-cli
```

Or install the latest release using

``` bash
cargo install numbat-cli
```
