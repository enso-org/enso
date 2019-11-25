#!/bin/sh
cargo run --manifest-path=script/rust/Cargo.toml --bin test-all -- \
    --node --firefox --chrome --headless
