#!/bin/bash -x

GRAALVM=$HOME/bin/graalvm/
OUT=build/parser

mkdir -p build/parser
cargo run -p enso-parser-generate-java --bin enso-parser-generate-java -- $OUT

$GRAALVM/bin/javac $OUT/*java --source-path ./lib/rust/parser/generate-java/java/ -d $OUT

cargo build -p enso-parser-cdylib
$GRAALVM/bin/java -cp $OUT LoadParser.java
