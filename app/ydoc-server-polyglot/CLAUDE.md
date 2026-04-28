# ydoc-server-polyglot

The `ydoc-server` bundled with esbuild into a single CommonJS file so it can be
loaded by GraalJS inside the GraalVM engine process. Uses `commonjs` package
type (the rest of the pnpm workspace uses `module`).

## Why this exists

The Enso engine ships as a GraalVM process that already has a JS runtime
(GraalJS). Rather than requiring a separate Node.js to run the ydoc server next
to it, we bundle the server into a polyglot script the engine can embed. This is
the **production** deployment for the collaborative editing story.

## Build

- `pnpm run compile` → `node ./build.mjs build` (esbuild-driven, writes
  `dist/main.cjs`).
- `pnpm run dev:watch` → esbuild watch mode.

## Constraints

GraalJS is **not** Node.js:

- Only the subset of Node APIs that `graaljs` polyfills is usable.
  Filesystem/process/net work, but some newer APIs or native bindings do not.
- No dynamic `import()` of ESM — everything must end up CJS after bundling.
- WebSocket support comes from the engine host, not `ws`; the bundle stubs this.

When adding a dep to `ydoc-server`, verify the bundled output still loads under
GraalJS before merging (see `docs/infrastructure/ydoc.md`).

## Source

All logic lives in `ydoc-server`. This package is pure packaging — `src/` should
stay minimal.
