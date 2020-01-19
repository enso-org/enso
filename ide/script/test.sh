#!/bin/bash
cargo test &&
cargo run --manifest-path=script/rust/Cargo.toml --bin test-all -- --headless --firefox
