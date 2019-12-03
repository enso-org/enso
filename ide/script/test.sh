#!/bin/sh
cargo test &&
cargo run --manifest-path=script/rust/Cargo.toml --bin test-all -- \
   --node --firefox --chrome --headless
