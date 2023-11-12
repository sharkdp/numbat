#!/bin/bash

set -euo pipefail

wasm-pack build --features=wee_alloc -t web

curl https://cdnjs.cloudflare.com/ajax/libs/ace/1.9.6/ace.min.js -o pkg/ace.min.js

cp -r pkg/ ../assets/articles

