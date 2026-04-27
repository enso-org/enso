# enso-build-base

The foundation crate for the build CLI. Holds only types and helpers that
_every_ other `build_tools` crate needs, with a deliberately small dependency
set (`anyhow`, `fn-error-context`, `futures`, `serde`, `tracing`).

Keep this crate lean — adding a heavyweight dep here inflates compile time
across the whole build CLI.
