#!/bin/bash

set -euo pipefail

wasm-pack build --features=wee_alloc

(
    cd www/
    rm -rf dist/
    npm run build
)
