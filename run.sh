#!/bin/bash
set -e
# Get the directory of the script, as per https://stackoverflow.com/a/246128
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cargo build --profile buildscript --target-dir $SCRIPT_DIR/target/enso-build --package enso-build3 --bin enso-build3
$SCRIPT_DIR/target/enso-build/buildscript/enso-build3 $@
