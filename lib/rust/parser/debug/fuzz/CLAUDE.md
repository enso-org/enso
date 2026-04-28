# enso-parser-fuzz

AFL-driven fuzz harness for the Enso parser. Runs `enso-parser` (with the
`debug` feature) against arbitrary byte streams and flags crashes/timeouts.

## Run

Needs the `cargo-afl` toolchain (`cargo install cargo-afl`). Typical loop:

```
cargo afl build --release
cargo afl fuzz -i corpus/ -o findings/ target/release/enso-parser-fuzz
```

Corpus and findings are not tracked in git.
