# Installation

## Linux

### Try immediately

On Linux, you can call

``` bash
curl -sSf https://numbat.dev/try.sh | sh
```

to try it immediately (unpacks to `/tmp` and executes from there).

### Manual installation

For a proper installation, download the latest release
[here](https://github.com/sharkdp/numbat/releases).

Unpack the archive and move the `modules/` folder to `~/.config/numbat`,
or one of the [other supported directories](./cli-customization.md#module-paths).

``` bash
mkdir -p ~/.config/numbat
cp -r numbat/modules ~/.config/numbat/
```

## From source

Clone the Git repository, and build Numbat with `cargo`:

``` bash
git clone https://github.com/sharkdp/numbat
cd numbat/
cargo install -f --path numbat-cli
```

And copy the `modules/` folder to `~/.config/numbat/`, or the
[respective folder on your operating system](./cli-customization.md#module-paths).

``` bash
mkdir -p ~/.config/numbat
cp -r modules/ ~/.config/numbat/
```
