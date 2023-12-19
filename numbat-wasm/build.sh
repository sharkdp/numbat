#!/bin/bash

set -euo pipefail

rm -rf www/pkg
wasm-pack build --features=wee_alloc --target=web --out-dir=www/pkg
