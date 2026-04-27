# enso-parser-debug

CLI + library for inspecting parser output. Given Enso source, dumps the AST in
an S-expression-like format (`enso-metamodel-lexpr`). Useful for:

- Manually verifying parser behavior on tricky snippets.
- Producing golden files for `insta` snapshot tests.
- Debugging lexer/macro-resolution issues without building the full engine.

## Snapshot tests

This crate uses `insta`. Update with `cargo insta review` after intentional
parser changes.

## fuzz/

`enso-parser-fuzz` (in the `fuzz/` subdir) runs AFL against the parser. See
`fuzz/CLAUDE.md`.
