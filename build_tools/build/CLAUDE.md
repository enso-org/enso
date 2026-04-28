# enso-build

Domain logic of the Enso build CLI. Owns the Target concept (see `README.md` in
`build_tools/`) and implements all the artifact producers.

**Status: legacy**, slated for replacement by Bazel. Maintain existing targets,
but don't add new ones when a Bazel target can cover the same case.

## Module map

Each module handles one artifact family or one integration:

- `engine/` + `engine.rs` — Scala/Java engine (runtime + language server).
  Shells out to SBT. Note: the Scala Project Manager is no longer part of this
  bundle — the Electron app uses the TypeScript `app/project-manager-shim/`
  instead.
- `ide/` + `ide.rs` — Top-level IDE target. Glues GUI and backend.
- `project/` + `project.rs` — GUI side: invokes pnpm/Vite.
- `release/` + `release.rs` — Cutting releases, signing, uploading assets.
- `repo/` + `repo.rs` — Git repo state, commit hashes, branch conventions.
- `changelog/` + `changelog.rs` — Parses + validates `CHANGELOG.md` entries
  (one-per-PR rule).
- `rust/` — Rust workspace helpers (running cargo across members, WASM builds).
- `ci/`, `ci_gen/`, `ci.rs`, `ci_gen.rs` — CI-matrix and GitHub Actions YAML
  generation inputs. Output is written by `enso-build-ci-gen`.
- `aws/` + `aws.rs` — S3/ECR uploads of artifacts and container images.
- `cloud_tests/` — Harness for running tests against the Enso Cloud.
- `libraries_tests.rs` — Runs the per-library test projects in `test/` against a
  built engine.
- `enso.rs`, `env.rs`, `config.rs`, `context.rs`, `paths.rs` — Core types: build
  context, paths, environment, configuration.
- `postgres.rs` — Local Postgres for tests.
- `httpbin.rs` — Spawns `tools/http-test-helper/` for Base_Tests.

## Dependency choices

- `octocrab` (forked in `[workspace.dependencies]`) for GitHub API.
- `handlebars` for templated text output.
- `aws-sdk-*` and `aws-config` (pinned to 0.21 / 0.51) — don't bump without
  coordinating with the installer code.

## Patterns

- Long workflows use `tokio`. Avoid blocking file I/O in async contexts — prefer
  `ide-ci` helpers.
- Add new build steps as types implementing the target-like traits used
  elsewhere in the module; the CLI layer in `enso-build-cli` wires them to
  `clap` subcommands.
- Build outputs live in `dist/` (top-level) by convention; resolve the exact
  path from the context, not hardcoded strings.
