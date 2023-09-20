#!/bin/bash

set -euo pipefail

cargo build --release --locked

rm -rf /tmp/numbat
mkdir /tmp/numbat

cp target/release/numbat /tmp/numbat/
strip /tmp/numbat/numbat

cp -r modules/ /tmp/numbat/
cp -r examples/ /tmp/numbat/
cp LICENSE-APACHE LICENSE-MIT README.md /tmp/numbat

(
    cd /tmp
    zip -r numbat.zip numbat/
)

mv /tmp/numbat.zip .
