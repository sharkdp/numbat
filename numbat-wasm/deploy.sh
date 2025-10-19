#!/bin/bash

set -euo pipefail

current_branch=$(git rev-parse --abbrev-ref HEAD)

if [[ "$current_branch" != "master" ]]; then
    echo "You are currently on the '$current_branch' branch, not 'master'."
    exit 1
fi

bash build.sh

rsync --archive --stats --progress --human-readable -r www/ shark.fish:numbat.dev/
