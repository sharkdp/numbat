#!/bin/bash

set -euo pipefail

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cd "$SCRIPT_DIR"

uv run python build_docs.py

rsync --delete --archive --stats --progress --human-readable site/ shark.fish:numbat.dev/doc/
