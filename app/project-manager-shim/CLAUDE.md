# project-manager-shim

TypeScript Project Manager implementation used by **both the dev workflow and
the shipped Electron build**. The name "shim" is historical — this package began
as a dev-only stand-in for the Scala Project Manager under
`lib/scala/project-manager`, but the Scala PM is no longer wired into any build,
so this TS service is now the real thing.

## Why

The GUI needs a project manager socket for `pnpm dev:vite` and the packaged
Electron app. Running the old Scala/Java PM required a long build and a GraalVM
JDK. This package speaks the same protocol natively against a local engine
distribution — fast to iterate on, no JVM needed.

## Subpath exports

Consumers import by subpath (see `package.json#exports`):

- `project-manager-shim` — public helpers (`downloadEnsoEngine`,
  `findEnsoExecutable`, …).
- `project-manager-shim/fs` — filesystem glue.
- `project-manager-shim/handler` — JSON-RPC request handler.
- `project-manager-shim/projectService` — project-lifecycle service.

## Engine download

`scripts/download-engine.js` fetches a prebuilt engine distribution; cache is
under `~/.cache/enso` (or platform equivalent).

## Scope

Historically some advanced features (cloud projects, multi-engine, upgrade)
deferred to the old Scala PM. Those paths now need to live here or be served by
the language server / cloud directly — don't add Scala-PM back-references when
extending this package.
