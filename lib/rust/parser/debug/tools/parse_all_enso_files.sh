#!/bin/sh

# Parses all the `.enso` source files in the standard library and test suite,
# and emits their debug representations to standard output. This supports
# testing that a refactor in the parser doesn't affect the results of parsing
# valid files.
#
# Usage example:
#
# ./lib/rust/parser/debug/tools/parse_all_enso_files.sh > branch.ast
# git checkout develop
# ./lib/rust/parser/debug/tools/parse_all_enso_files.sh > develop.ast
# diff develop.ast branch.ast

set -e

cargo build -p enso-parser-debug --bin enso-parser-debug
cargo build -p enso-parser-debug --bin lexer

ENSO_FILES=$(find distribution/ test/ -name '*.enso' -print | sort)
for x in $ENSO_FILES; do
	echo -n "$x "
	target/rust/debug/lexer <$x >/dev/null
	target/rust/debug/enso-parser-debug <$x
done

