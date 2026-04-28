# electron-client

The Electron main process. Package name: `enso`. Produces the installable
desktop binary (AppImage/DMG/exe + installer).

## Layout

- `src/` — Electron main process TypeScript.
- `buildInfo.ts`, `esbuildConfig.ts`, `bundle.ts`, `dist.ts`, `watch.ts` — build
  orchestration (bundles `src/` with esbuild, runs `electron-builder`).
- `electron-builder-config.cjs` / `electron-builder-config.ts` — packaging
  config (icons, code signing, entitlements).
- `macos/`, `entitlements.mac.plist` — macOS DMG/notarization extras.
- `tasks/` — one-shot CLI tasks exposed for `vite-node`.
- `assets/` — icons/images that end up in the installer.
- `paths.ts` — canonical path resolution (userData, logs, bundled engine).
- `fileAssociations.ts` — `.enso` file-association handlers.
- `export-config.mjs` — emits the final Vite config for packaging.

## What this process does

- Serves the GUI bundle (from `app/gui/dist/`) in a BrowserWindow.
- Runs the Project Manager in-process via the TS `project-manager-shim` (the
  Scala Project Manager is no longer used, not even in the packaged Electron
  build).
- Implements the custom `enso://` protocol handler used by Enso Cloud links.
- Handles deep-links, auto-update (electron-updater via electron-builder),
  custom window chrome.

## Build

Currently driven by `./run ide build` (the legacy Enso build CLI). Don't call
`electron-builder` or `pnpm run dist` manually unless you're debugging packaging
— the build CLI wires the engine bundle, GUI, and Electron together with the
right env vars. `./run` is slated to be replaced by a Bazel target; check for
one before assuming `./run` is the only path.

`watch:linux` / `watch:macos` / `watch:windows` scripts are for local iteration
once `./run ide build` has produced the engine bundle.

## Env vars of note

- `ENSO_BUILD_IDE`, `ENSO_BUILD_BACKEND` — output locations.
- `ENSO_BUILD_IDE_BUNDLED_ENGINE_VERSION` — which engine to bundle.
- `ENSO_POLYGLOT_YDOC_SERVER` — URL for the (polyglot) ydoc server when running
  against a cloud backend.
- `ENSO_IDE_VERSION`, `ENSO_IDE_COMMIT_HASH` — embedded into buildinfo.

## Tests

Playwright-driven E2E tests in `tests/` + `playwright.config.ts`. They launch a
packaged (or unpackaged) build. Runs are long; avoid in inner dev loops.
