# tools/

Utility binaries and scripts that aid engine/GUI development. **Not** a catch-all for misc files — each subdir has a specific justified purpose.

## Subdirs

- `http-test-helper/` — Local HTTP mock server used by `test/Base_Tests`. Java Bazel target.
- `enso4igv/` — Enso language support for the GraalVM Ideal Graph Visualizer (IGV). Load this when debugging Truffle compilation graphs. See `docs/runtime-guide.md` §Tips.
- `build-performance/` — Benchmark script (`bench-build.sh`) for measuring SBT/Bazel build performance.
- `performance/` — Engine benchmarks and analysis scaffolding (`engine-benchmarks/`, `benchmark-analysis/`).
- `ci/` — CI helper scripts: `check-changelog.js`, docker images (`docker/`), nightly release (`nightly/`), release tooling (`releases/`).
- `native-image-config-cleanup/` — TypeScript tool that de-duplicates Native Image `reflect-config.json` / `resource-config.json` across GraalVM builds.
- `legal-review/`, `legal-review-helper/` — Third-party dependency license-review inputs and helpers.
- `simple-library-server/` — Minimal HTTP library-server stub for testing the library download flow without touching the real library repository.

## Conventions

- Each tool must stay runnable without the full repo build. If it needs the engine, make that explicit via environment variables or args.
- Don't put anything here that belongs in `build_tools/` (orchestration) or `engine/`/`lib/` (production code).
