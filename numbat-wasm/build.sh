#!/bin/bash

set -euo pipefail

# Make sure that Rust version is equal to 1.76, to work around
# https://github.com/rustwasm/wasm-pack/issues/1389

if ! rustc --version | grep -q '1.76'; then
    echo "Rust version must be 1.76 to generate work WASM code at the moment"
    exit 1
fi


rm -rf www/pkg
wasm-pack build --target=web --out-dir=www/pkg
