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

current_branch=$(git rev-parse --abbrev-ref HEAD)

if [[ "$current_branch" != "master" ]]; then
    echo "You are currently on the '$current_branch' branch, not 'master'."
    exit 1
fi

bash build.sh

rsync --archive --stats --progress --human-readable -r www/ shark.fish:numbat.dev/
