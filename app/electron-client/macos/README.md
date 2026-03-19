# macOS Bazel Build Workflow

This directory contains the scripts used to produce a macOS DMG from the
Bazel-built Electron app.

## Overview

The workflow is:

1. Build/stage an unsigned `.app` with Bazel.
2. Optionally sign engine archives and the app bundle.
3. Optionally notarize and staple the app bundle.
4. Build a DMG from the prepackaged app with `electron-builder`.

## Entry point

Use:

```bash
pnpm -C app/electron-client macos:build-dmg [options]
```

Defaults are for development builds:

- signing disabled
- notarization disabled

CI should explicitly enable both:

```bash
pnpm -C app/electron-client macos:build-dmg --sign --notarize
```

## Options

- `--app <path>`: use an existing `.app` bundle and skip Bazel staging.
- `--entitlements <path>`: entitlements plist path.
- `--sign`: enable signing.
- `--notarize`: enable notarization (requires `--sign`).
- `--verbose`: print executed commands and subprocess output.

## Environment variables

`build-dmg.mjs` only requires variables for enabled stages.

For signing (`--sign`):

- `CSC_LINK`
- `CSC_KEY_PASSWORD`
- `APPLETEAMID`

For notarization (`--notarize`):

- `APPLEID`
- `APPLEIDPASS`
- `APPLETEAMID`

For DMG versioning:

- `ENSO_IDE_VERSION` (optional, defaults to `0.0.0-dev`)

## Scripts in this directory

- `build-dmg.mjs`: orchestrates the full workflow.
- `sign-archives.mjs`: internal module that signs native files inside
  archives/resources.
- `sign-app.mjs`: internal module that signs nested app components and app
  bundle.
- `notarize-app.mjs`: internal module that notarizes and staples the app bundle.
- `electron-builder-dmg-only.cjs`: minimal DMG-only electron-builder config.
- `stage-bazel-app.sh`: Bazel helper target used to stage the built app.
