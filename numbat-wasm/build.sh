#!/bin/bash

set -euo pipefail

rm -rf www/pkg
wasm-pack build --target=web --out-dir=www/pkg
