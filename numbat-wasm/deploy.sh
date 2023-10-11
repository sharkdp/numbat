#!/bin/bash

set -euo pipefail

current_branch=$(git rev-parse --abbrev-ref HEAD)

wasm-pack build --features=wee_alloc

(
    cd www/
    rm -rf dist/
    npm run build
)

rsync --archive --stats --progress --human-readable -r www/ shark.fish:numbat.dev/angles/
rsync --archive --stats --progress --human-readable -r www/dist/ shark.fish:numbat.dev/angles/
