# Installation

## Linux

### On Ubuntu

*... and other Debian-based Linux distributions.*

Download the latest `.deb` package from [the release page](https://github.com/sharkdp/numbat/releases)
and install it via `dpkg`. For example:

``` bash
curl -LO https://github.com/sharkdp/numbat/releases/download/v1.6.2/numbat_1.6.2_amd64.deb
sudo dpkg -i numbat_1.6.2_amd64.deb  
```

### On Arch Linux

In Arch Linux and Arch based distributions, you can install the prebuilt package of Numbat from the
[AUR](https://aur.archlinux.org/packages/numbat-bin).

``` bash
yay -S numbat-bin
```

You can also install the [numbat](https://aur.archlinux.org/packages/numbat)
AUR package, which will download the source and compile it.

``` bash
yay -S numbat
```

## macOS

### Homebrew

You can install Numbat with Homebrew:

``` bash
brew install numbat
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
