#!/bin/bash

set -euo pipefail

# Make sure that Rust version is == 1.76.0 for now to avoid running into
# https://github.com/rustwasm/wasm-pack/issues/1389
# With newer versions, we get panics when (for example) running:
# https://numbat.dev/doc/example-paper_size.html
if ! rustc --version | grep -q "1.76.0"; then
    echo "Please switch to Rust version 1.76.0."
    echo "(rustup default 1.76.0)"
    exit 1
fi

rm -rf www/pkg
wasm-pack build --target=web --out-dir=www/pkg
