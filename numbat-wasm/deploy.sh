#!/bin/bash

set -euo pipefail

current_branch=$(git rev-parse --abbrev-ref HEAD)

bash build.sh

rsync --archive --stats --progress --human-readable -r www/ shark.fish:numbat.dev/angles/
