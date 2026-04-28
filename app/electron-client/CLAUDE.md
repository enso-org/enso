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

## Local Claude agent

`src/claudeAgent.ts` shells out to the user-installed `claude` CLI executable
(assumed to be on `PATH`) via `cross-spawn` for headless, single-turn generation
of User Defined Components. Invocation flags:
`--print --output-format json --json-schema <RESPONSE_SCHEMA> --system-prompt <SYSTEM_PROMPT> --tools "" --setting-sources "" --no-session-persistence`.
The prompt is always written to the child's stdin (uniform handling regardless
of length). `--setting-sources ""` keeps the invocation hermetic (no user
settings/plugins/`CLAUDE.md` discovery) without touching auth; `--bare` is
deliberately avoided because it would re-introduce the `ANTHROPIC_API_KEY`
requirement.

The renderer reaches the IPC via `window.api.ai.generateComponent(...)` (see
`enso-gui/src/electronApi.ts`) over channel `Channel.generateAiComponent`. The
shared request/response types live in `enso-common/src/ai.ts` so both halves of
the IPC agree on the shape. At main-process startup `claudeAgent.ts` runs a
best-effort `claude --version` probe and logs the result; failure is non-fatal —
the first real IPC call surfaces the ENOENT error to the renderer as a toast.

Gotchas:

- With `--json-schema` active, the CLI puts the schema-validated payload in the
  envelope's `structured_output` field (pre-decoded object); the envelope's
  plain `result` field is left empty. Read from `structured_output` first; only
  fall back to `result` for older CLI releases.
- The CLI lookup uses `cross-spawn`, not `node:child_process`. Reason: a user
  who installed Claude Code via `npm install -g @anthropic-ai/claude-code` on
  Windows ends up with a `claude.cmd` shim in the npm global bin dir (npm wraps
  every bin entry through `cmd-shim`, regardless of whether the target is a
  `.exe`). `child_process.spawn('claude', …)` without `shell: true` will not
  resolve `.cmd` extensions on Windows and returns ENOENT. `cross-spawn` handles
  `.cmd`/`.ps1` lookup and argument quoting for `cmd.exe` while staying a no-op
  on POSIX. The unit test mocks `cross-spawn` (default export) rather than
  `node:child_process`.
- Electron IPC serializes with structured clone, which strips class prototypes.
  `Err(...)` from `enso-common/src/utilities/data/result` arrives at the
  renderer as a plain `{ payload, context }` — `ResultError`'s methods are gone.
  The renderer half (`ai.ts`) rebuilds the `Result` with `Ok()` / `Err()` right
  after the IPC call so downstream callers see a well-formed error.

## Tests

Two layers, same directory, different runners — controlled by the `testIgnore`
rule in `playwright.config.ts`:

- `tests/headless/*.test.ts` — Vitest unit tests for main-process code. No
  Electron, no DOM. Fast. Run with `corepack pnpm vitest --run tests/headless`.
  This is where `claudeAgent.test.ts` lives.
- `tests/*.spec.ts` — Playwright end-to-end tests that launch the packaged
  Electron binary from `dist/ide/` and drive the app from a real user's
  perspective (login → dashboard → project → graph editor). `electronTest.ts`
  extends Playwright's `test` fixture to spawn Electron and exposes helpers like
  `loginAsTestUser`, `createNewProject`, `openComponentBrowser`. See
  `tests/README.md` for prerequisites (a built `dist/ide/`, credentials at
  `playwright/.auth/user.json`). Run with
  `corepack pnpm -r --filter enso ide-integration-test [path.spec.ts]`. Runs are
  long (minutes), so avoid them in inner dev loops.

When adding a Playwright test that needs an external dependency the CI doesn't
have yet (e.g. `aiNode.spec.ts` needs the local `claude` CLI), gate the whole
describe block on an env flag (`process.env.ENSO_TEST_AI === '1'`) and note the
flag in the plan's verification section so per-step smokes still exercise it
locally.
