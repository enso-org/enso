# project/

SBT meta-build: plugins, helpers, and task definitions that drive `build.sbt`.
Every non-trivial build concern the engine has lives here as a Scala file.

## High-impact files

- `JPMSPlugin.scala` + `JPMSUtils.scala` — Generates JPMS `--module-path`,
  `--add-modules`, patches, and exports for every SBT project with
  `.enablePlugins(JPMSPlugin)`. The engine is a JPMS app; touching modules
  without this plugin produces broken runtimes. See in-file docstring.
- `FrgaalJavaCompiler.scala` — Wires up the [Frgaal](https://frgaal.org/) Java
  compiler so we can use modern Java language features while targeting older
  bytecode versions.
- `NativeImage.scala` — Native Image task definitions for the launcher and
  `ensoup`.
- `GraalVM.scala` — Graal detection and options.
- `Cargo.scala` — Calls into the Rust workspace (for parser JNI + wasm).
- `GenerateFlatbuffers.scala` — Runs `flatc` (from `toolchains/flatc/`) to
  generate the LS binary-protocol bindings.
- `Distribution.scala` + `DistributionPackage.scala` — Produces the engine
  distribution directory layout (matches `distribution/`).
- `Editions.scala` — Edition-manifest generation.
- `GatherLicenses.scala` — Drives `sbt-license-report` and the legal-review
  checks.
- `IRCaches.scala` — Compiles IR caches that ship with the stdlib for fast
  startup.
- `BazelSupport.scala` — Hooks for the coexisting Bazel build.
- `EnsoLint.scala` — Project-wide lint rules invoked as an SBT task.
- `EnsoProjects.scala` — Shared project-configuration DSL used by `build.sbt`.
- `SmallJDK.scala` — Builds a trimmed JDK image for the distribution.
- `Dependencies.scala` — Central dependency version catalog.
- `plugins.sbt` — SBT plugin versions. Pinned to known-good combinations; don't
  bump casually.

## Conventions

- Any reusable build step goes into a plugin or an object here, not inline in
  `build.sbt`.
- Scala 2 syntax (SBT's version). Don't try to import Scala 3 features.
- When adding a new JPMS module, wire it through `JPMSPlugin` — don't hand-roll
  module flags.
