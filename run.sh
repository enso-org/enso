#!/bin/bash
set -e # Exit on error.

# Get the directory of the script, as per https://stackoverflow.com/a/246128
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

TARGET_DIR="${SCRIPT_DIR}target/enso-build/"
TARGET_EXE="${TARGET_DIR}buildscript/enso-build3"
if [ ! -f "$TARGET_EXE" ]; then
    cargo build --profile buildscript --target-dir "$SCRIPT_DIR" --package enso-build3
fi

"$TARGET_EXE" $@
