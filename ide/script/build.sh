#!/bin/bash
wasm-pack build $@ --no-typescript --out-dir '../../target/web' lib/core
