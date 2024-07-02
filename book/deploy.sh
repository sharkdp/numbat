#!/bin/bash

set -euo pipefail

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cd "$SCRIPT_DIR"

python build.py

rsync --delete --archive --stats --progress --human-readable book/html/ shark.fish:numbat.dev/doc/
