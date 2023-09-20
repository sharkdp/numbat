#!/bin/bash

set -euo pipefail

wasm-pack build

cd www/
npm run build

rsync --archive --stats --progress --human-readable -r ~/software/numbat/numbat-wasm/www/ shark.fish:numbat.dev/
rsync --archive --stats --progress --human-readable -r ~/software/numbat/numbat-wasm/www/dist/ shark.fish:numbat.dev/
