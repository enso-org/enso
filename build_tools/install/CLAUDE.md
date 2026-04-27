# build_tools/install

Windows installer/uninstaller for the Enso IDE. Four crates:

- `config/` — `enso-install-config`. Shared config/plan types, embedded-resource
  handling. Pulled in by the build scripts of installer/uninstaller so the same
  layout is visible at compile-time.
- `/` (the crate here) — `enso-install`. The runtime library: lock file
  management (`named-lock`), tracing setup, manifest planning, post-install
  hooks. Uses `mslnk` (shortcuts) and `winreg` (registry) on Windows.
- `installer/` — `enso-installer`. The actual installer binary. Bundles a
  gzipped tarball of the IDE as a build-time resource and extracts it on run.
- `uninstaller/` — `enso-uninstaller`. Self-replacing binary (`self-replace`
  crate) that removes the install and tidies the registry.

## Windows-only

Most real logic is behind `cfg(windows)`. On other hosts the crates still
compile as stubs for editor support.

## Release bundling

The Enso build CLI invokes `cargo build` on these crates after a full IDE build
so it can embed the IDE tarball as an RC resource (`embed-resource`). Don't run
these manually unless you're debugging packaging.

## Gotcha: AWS SDK versions

`enso-build` pins old `aws-sdk-*` versions (0.21, 0.51). If you bump those,
verify the install path still compiles — `ide-ci`'s S3 helpers are consumed here
for upload.
