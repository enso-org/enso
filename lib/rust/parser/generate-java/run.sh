#!/bin/sh

set -e

echo $0 | grep lib/rust || ( echo This tool must be run from the repo root, as lib/rust/parser/generate-java/run.sh; exit 1 )

BASE=target/generated_java
OUT=$BASE/org/enso/syntax2
LIB=lib/rust/parser/generate-java/java
mkdir -p $OUT
cargo test -p enso-parser-generate-java
cargo run -p enso-parser-generate-java --bin enso-parser-generate-java -- $OUT
cargo run -p enso-parser-generate-java --bin java-tests > $BASE/GeneratedFormatTests.java
javac -classpath "$LIB:$BASE" -d $BASE $BASE/GeneratedFormatTests.java
java -classpath $BASE GeneratedFormatTests
