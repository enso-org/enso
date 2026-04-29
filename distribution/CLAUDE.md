# distribution/

Inputs, templates, and authored content that make up a **shipped Enso
distribution**. Most runtime behavior lives elsewhere; this directory is mostly
data and layout.

## Layout

- `lib/Standard/` — **The Enso standard library.** `.enso` source files. One
  directory per module (`Base`, `Table`, `Database`, `AWS`, …). Every module
  pins a version under `0.0.0-dev/` (the placeholder version used during
  development — release engineering rewrites it). See `lib/Standard/CLAUDE.md`.
- `engine/` — Auxiliary files shipped alongside the compiled engine (component
  group metadata, etc.).
- `launcher/` — Bundled resources for the Enso launcher.
- `bin/` — Scripts shipped in the distribution.
- `edition.template.yaml` — Template for edition manifests produced at release
  time.
- `manifest.template.yaml` — Template for the distribution manifest.
- `enso.bundle.template` — Structure of the full Enso bundle (engine + stdlib +
  launcher).
- `launcher-manifest.yaml` — Manifest describing launcher version metadata.

## Editions and libraries

Library versions and supported engine versions are grouped into **editions**
(see `docs/libraries/editions.md`). The edition template in this directory is
the source of truth for the layout a release artifact produces.

## Changing the standard library

Most changes land in `lib/Standard/<Module>/0.0.0-dev/src/` (Enso) paired with
matching helpers in `std-bits/<module>/`. Always run the matching
`test/<Module>_Tests` suite.

## Do not commit

- Built artifacts (`dist/` at repo root is for outputs).
- Pinned third-party binaries beyond what's needed for bootstrapping.
