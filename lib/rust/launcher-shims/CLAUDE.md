# launcher-shims

Tiny Rust binaries whose sole job is to forward all their arguments to the real
Enso launcher while pretending to be a specific launcher version. Exists purely
to support the launcher **self-upgrade** test suite at
`engine/launcher/src/test/scala/org/enso/launcher/upgrade/UpgradeSpec.scala`.

## Why binaries (not a bash script)

- Self-upgrade tests need real executables — Windows has hard constraints around
  moving running `.exe` files.
- Building the full native-image launcher repeatedly is too slow to iterate on.
- A shim compiles in seconds and behaves like a launcher binary to the OS.

See `README.md` for the full explanation, including how the shim overrides the
reported version and perceived binary location via `InternalOpts`.

`publish = false`. Never release this to crates.io.
