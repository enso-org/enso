#!/bin/sh
cargo watch -i .gitignore -i "pkg/*" -s "wasm-pack build --out-dir '../../target/web' lib/core"