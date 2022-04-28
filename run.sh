#!/bin/bash
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cargo build --profile buildscript --target-dir $SCRIPT_DIR/target/enso-build --package enso-build3 && $SCRIPT_DIR/target/enso-build/debug/enso-build3 "$@"
