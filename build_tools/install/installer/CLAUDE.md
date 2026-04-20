# enso-installer

The Windows IDE installer binary. Embeds a gzipped tarball (produced by
`enso-build` and sized in GB) as an RC resource via `embed-resource`, shows a
`native-windows-gui` progress dialog, and extracts into Program Files / the user
profile.

## Build flow

`build.rs` takes the IDE tarball path from environment variables
(`ENSO_BUILD_*`) populated by the build CLI, gzips+tars it, and hands the
resulting file to `embed-resource`. Without those env vars set, the build still
succeeds but the installer has no payload — useful for editor/`cargo check` but
not for shipping.

## Runtime flow

`src/main.rs` reads the embedded blob, decompresses it (`flate2` + `tar`),
writes files, registers entries via `enso-install`, and hands off to the
uninstaller for teardown. Logs to `%LOCALAPPDATA%\Enso\logs` via `tracing`.
