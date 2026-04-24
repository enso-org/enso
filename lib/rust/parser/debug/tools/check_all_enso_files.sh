#!/bin/sh

# Parses all the `.enso` source files in the standard library and test suite,
# and reports any syntax error found.

set -e

cargo build --release -p enso-parser-debug --bin check_syntax

find distribution/ test/ -name '*.enso' -exec target/rust/release/check_syntax '{}' '+'
