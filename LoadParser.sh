#!/bin/bash -x

set -e

GRAALVM=$HOME/bin/graalvm/
OUT=build/parser

mkdir -p build/parser
cargo run -p enso-parser-generate-java --bin enso-parser-generate-java -- $OUT

$GRAALVM/bin/javac $OUT/*java --source-path ./lib/rust/parser/generate-java/java/ -d $OUT

# run test
cargo run -p enso-parser-generate-java --bin java-tests > $OUT/GeneratedFormatTests.java
$GRAALVM/bin/java -cp $OUT GeneratedFormatTests

cargo build -p enso-parser-cdylib
$GRAALVM/bin/java -cp $OUT LoadParser.java
