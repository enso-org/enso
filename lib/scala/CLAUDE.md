# lib/scala

Scala support libraries for the engine. Each subdir is an SBT subproject (see
`build.sbt`, `project/`). These are the building blocks that the runtime,
language server, and launcher share.

## Module map

- `pkg/` — **Enso package format** (`package.yaml`, version layout, sources).
  Owned by the compiler and every CLI tool.
- `project-manager/` — **No longer used.** Was a JSON-RPC service managing Enso
  projects on the user's machine. The production Project Manager is now the
  TypeScript `app/project-manager-shim/` (used by both dev and the Electron
  build). This tree is retained for history; don't extend it.
- `distribution-manager/` — Resolves the Enso "distribution" (engine version +
  standard library edition) on disk.
- `runtime-version-manager/` — Downloads and installs engine/GraalVM versions;
  handles upgrades.
- `editions/` + `edition-updater/` — The "editions" abstraction — a named bundle
  of engine version + library versions. See `docs/libraries/editions.md`.
- `library-manager/` + `downloader/` — Library download/resolution (from
  LibraryRepository / Enso Cloud).
- `searcher/` — Completion database backing the LS `search/*` endpoints.
- `refactoring-utils/` — Shared rename/refactor helpers consumed by the LS.
- `cli/` — Shared argument-parsing / CLI support used by launcher and runner.
- `json-rpc-server/` — Akka-based JSON-RPC server core (used by
  `language-server`).
- `filewatcher/` — File-system watcher (inotify/FSEvents/ReadDirectoryChangesW)
  used by the LS.
- `connected-lock-manager/` + `connected-lock-manager-server/` +
  `locking-test-helper/` — Distributed lock management for coordinating
  cloud/desktop access to the same project.
- `text-buffer/` — Rope-like text buffer used by the LS.
- `semver/` — Semver types + parsing.
- `yaml/` — Typed YAML (SnakeYAML wrapper).
- `task-progress-notifications/` — Protocol-agnostic progress-event model.
- `process-utils/`, `profiling-utils/` — Process + profiling helpers.
- `version-output/` — Formats `--version` strings with JVM/Graal info.
- `logging-config/`, `logging-service/`, `logging-service-logback/`,
  `logging-truffle-connector/`, `logging-utils/`, `logging-utils-akka/` —
  Unified logging story. See `docs/infrastructure/logging.md`.
- `common-polyglot-core-utils/` — Helpers needed by both language and embedder
  (the rare place that can sit on both sides of the polyglot boundary).
- `bench-processor/` — Annotation processor generating JMH wrappers for Enso
  benchmarks.
- `testkit/` — Shared scalatest scaffolding.

## Conventions

- Follow `docs/style-guide/scala.md`.
- No cycles. If two subprojects need each other, a third shared subproject is
  probably needed.
- Prefer small modules over a few sprawling ones — the JPMS story relies on it.
- Anywhere a Java-native rewrite is possible, see `docs/runtime-roadmap.md`:
  we're migrating Scala → Java.
