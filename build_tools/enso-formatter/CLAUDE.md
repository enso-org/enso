# enso-formatter

Small regex-based source formatter that enforces the Enso Rust style rules that
`rustfmt` can't express — primarily the four-group import ordering (sub-modules
/ prelude / local / external) and the `// === SectionName ===` section-header
spacing from `docs/style-guide/rust.md`.

Run via the build CLI:

```
./run lint
```

or directly against a directory. The CI lint job calls this and fails if it
would rewrite any file.

Not a replacement for `rustfmt` — run `cargo fmt` first, this tool second.
