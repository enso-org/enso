# app/common

Published as package `enso-common`. Pure-TS utilities shared across the Electron
client, the GUI (both halves), and other app-side packages. Meant to be the
boundary-crossing neutral ground — anything Vue- or React-specific belongs in
its own package.

## Contents

- `src/accessToken.ts` — token persistence (AWS Cognito / Enso Cloud).
- `src/services/` — small HTTP clients against Enso Cloud endpoints.
- `src/text/`, `src/text.ts` — i18n text table + lookup helpers used by both
  dashboard and Electron UI.
- `src/download.ts` — HTTP download with progress.
- `src/options.ts` — command-line option parser used by the Electron shell.
- `src/utilities/` — generic TS helpers.
- `src/constants.ts` — app-wide constants (URLs, feature flags, version
  strings).

## Rules

- No Vue, React, DOM, or Node-specific APIs at module top level (dynamic imports
  / runtime checks are OK).
- Every dep must be either isomorphic or explicitly scoped to a single entry
  point.
- Breaking changes ripple into `enso-gui` and `enso` (Electron client) — compile
  both before merging.
