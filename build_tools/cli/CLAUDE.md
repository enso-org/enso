# enso-build-cli

`clap`-driven CLI on top of `enso-build`. This is what `./run` ultimately
invokes.

**Status: legacy.** `./run` and this CLI are scheduled to be replaced by direct
Bazel targets. Still maintained for existing flows — don't add new commands here
if the functionality can live as a Bazel target instead.

Keep logic thin — anything beyond "parse args, set up tracing, hand off to
`enso-build`" should move into the library crate.

## Invocation surface

Top-level commands mirror Targets: `backend`, `engine`, `gui`, `ide`, `release`,
etc. Each has nested subcommands (`build`, `test`, `watch`, …). Discover via
`./run <target> --help` rather than greppin' through sources.

## Logging

`tracing` + `tracing-subscriber`. The CLI configures a human-friendly subscriber
by default; pass `RUST_LOG=debug` to turn the volume up.
