# project-manager-shim

A TypeScript re-implementation of (part of) the Scala Project Manager service. **Dev/test only** — production uses the real Scala PM from `lib/scala/project-manager`.

## Why

The GUI wants a project manager socket to talk to during `pnpm dev:vite`. Running the full Scala/Java PM requires a long build and a GraalVM JDK. The shim speaks enough of the same protocol to let the GUI and Electron client operate against a local engine distribution for UI iteration.

## Subpath exports

Consumers import by subpath (see `package.json#exports`):
- `project-manager-shim` — public helpers (`downloadEnsoEngine`, `findEnsoExecutable`, …).
- `project-manager-shim/fs` — filesystem glue.
- `project-manager-shim/handler` — JSON-RPC request handler.
- `project-manager-shim/projectService` — project-lifecycle service.

## Limits

- Not feature-complete — advanced features (cloud projects, multi-engine, upgrade) fall back on the real PM.
- `scripts/download-engine.js` fetches a prebuilt engine distribution; cache is under `~/.cache/enso` (or platform equivalent).

## If you need more

Before extending the shim, consider whether the real Scala PM should be built and used instead — divergence between the two has bit us before. Update both protocols together.
