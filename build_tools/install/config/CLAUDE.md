# enso-install-config

Shared types for the installer/uninstaller. Encodes the install plan (paths,
registry entries, shortcut targets, uninstall metadata) that both halves read.

Also embeds build-time resources via `embed-resource` from `build.rs` of
consumers.

Keep serialization stable — the uninstaller may read a manifest written by a
much older installer.
