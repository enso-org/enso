# Snapshot Tests

The parser's main tests are a set of snapshot tests, in the `enso-parser-debug`
crate. These tests cover the Rust parser itself; the backend and frontend each
have integration tests within their own testing frameworks.

## Running the Tests

The tests can be run with `cargo test -p enso-parser-debug`. They are also run
in CI and must pass for any PR modifying the parser to be accepted.

## Updating the Tests

The tests use the Insta snapshot testing framework. When expected test results
have changed, Insta can be used to update the test files.

First Insta must be installed:

`cargo install cargo-insta`

Then, to review the changes in test case results:

`cargo insta test -p enso-parser-debug --review`
