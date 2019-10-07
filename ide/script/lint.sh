#!/bin/sh
cargo clean -p basegl -p basegl-backend-webgl -p basegl-prelude -p basegl-system-web
cargo clippy -- -D warnings