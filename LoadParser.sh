#!/bin/bash -x

set -e

GRAALVM=$HOME/bin/graalvm/

# build runtime.jar including the new parser classes
sbt --java-home $GRAALVM bootstrap
sbt --java-home $GRAALVM syntax-rust-definition/compile
sbt --java-home $GRAALVM buildEngineDistribution

# run test: parser all .enso files in the repository
cargo build -p enso-parser-jni
$GRAALVM/bin/java -cp runtime.jar LoadParser.java
