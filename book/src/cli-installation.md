# Installation

Numbat does not have an official release yet,
but you can try out a pre-release version.

## Linux

On Linux, you can call

``` bash
curl -sSf https://numbat.dev/try.sh | sh
```

to try it immediately (unpacks to `/tmp` and executes from there).

For a proper installation, download the latest beta release
[here](http://numbat.dev/numbat.zip).

Then unpack the archive and move the `modules/` folder to `~/.config/numbat`
(create this folder if it does not exist)

## From source

Clone the Git repository, and build Numbat with `cargo`:

``` bash
git clone https://github.com/sharkdp/numbat
cd numbat/
cargo install -f --path numbat-cli
```
