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
