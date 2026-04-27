# build_tools

The Rust implementation of the Enso build CLI. The repo root `./run` script
dispatches to `cli/` (crate `enso-build-cli`). Historically this was the
canonical build orchestrator for the engine, GUI, IDE, backend bundle, release
artifacts, and Windows installer.

**Status: legacy, being replaced by Bazel.** `./run` still works — developers
and CI invoke it daily — but the plan is to replace it with direct Bazel targets
across the repo. Don't extend the build CLI with new functionality when a Bazel
target could cover the same case. Migrate existing flows to Bazel when you touch
them.

See `README.md` for the principles and the concept of a **Target** (an artifact
that can be built from sources or downloaded).

## Crates

- `base/` — `enso-build-base`. Tiny shared types (error context, futures
  helpers) with minimal deps. Pulled in by everything else.
- `ci_utils/` — `ide-ci`. The kitchen-sink toolbox: process spawning, HTTP,
  archive handling, GitHub via `octocrab`, S3/ECR uploads, path utilities,
  logging. Most of the build logic's raw "do a thing on the OS" code lives here.
- `build/` — `enso-build`. Domain-specific build logic on top of `ide-ci`
  (engine, GUI, IDE, backend, release, changelog, CI YAML generation inputs).
  Large — probably the biggest Rust crate in the repo.
- `cli/` — `enso-build-cli`. The `clap`-driven entry point. Thin layer on top of
  `enso-build`.
- `ci-gen/` — `enso-build-ci-gen`. Generates the GitHub Actions YAML files under
  `.github/workflows/`. Run when the CI matrix changes.
- `enso-formatter/` — Small tool enforcing the import-grouping / section-header
  style from `docs/style-guide/rust.md` across our Rust sources.
- `install/` — Windows installer/uninstaller toolchain. Four crates: `install`
  (shared library), `install/config`, `install/installer`,
  `install/uninstaller`.
- `macros/lib/` — `enso-build-macros-lib`. Proc-macro helpers consumed by
  `enso-build` at build time.

## Conventions

- **Targets are artifacts.** Every buildable thing is a Target: produces a
  single directory of artifacts, can be built locally, downloaded from CI, or
  released. Keep new build steps shaped like Targets.
- **Portable.** These crates must compile and function on Linux, macOS, and
  Windows. Gate OS-specific code with `cfg(...)`.
- **Thin CLI.** Keep argument parsing in `cli/` and move logic into `build/`.
  The CLI shouldn't own knowledge about artifacts.
- **Don't re-implement tools that exist.** We explicitly prefer to shell out to
  the canonical tool (SBT, pnpm, cargo) rather than reimplement its behavior.

## Running (legacy path)

For now, `./run` still drives most flows from the repo root:

```
./run <command> [options]       # Linux/macOS (dispatches via cargo or bazel)
./run --help                     # enumerate commands
./run ide build                  # top-level IDE target
./run backend test               # test the engine bundle
```

`run.cmd` / `run.ps1` are the Windows equivalents. See `README.md` for the
target/subcommand catalog. Before reaching for `./run`, check whether a native
Bazel target exists (e.g. `bazel build //...`, `bazel test //...`) — Bazel is
where this directory is headed.
