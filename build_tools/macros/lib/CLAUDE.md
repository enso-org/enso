# enso-build-macros-lib

Proc-macro support library consumed by `enso-build`'s `build.rs` scripts. Emits
code from YAML inputs (via `serde_yaml`) using `syn`/`quote`/`proc-macro2` and
`enso-macro-utils` helpers.

This isn't a `proc-macro = true` crate — it's a plain library that emits token
streams for build-time consumption. Import from a `[build-dependencies]` block.
