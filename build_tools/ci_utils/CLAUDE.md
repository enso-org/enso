# ide-ci

Kitchen-sink library used by the rest of the build CLI. Provides the "do a
thing" helpers:

- Processes (command building, output capture, logging).
- Filesystem (path normalization, zip/tar, symlinks, `fs_extra`).
- HTTP (`reqwest`) and GitHub (`octocrab`).
- Archive formats (`zip`, `tar`, `flate2`).
- Path-system quirks (`path-slash`, `path-absolutize`).
- Process inspection (`sysinfo`, `dependency_runner`).
- Generic async plumbing (`tokio`, `flume`, `indicatif` progress bars).
- HTTP mocking for tests (`wiremock`).

The crate is deliberately broad — adding a dep here is cheap compared to adding
it everywhere that would otherwise need it. Keep domain logic out though;
`enso-build` is the place for Enso-specific knowledge.

## Feature conventions

No feature flags — everything is on. If a consumer only needs a subset, they
still only pay for what the linker keeps.

## Error conventions

Use `anyhow::Result` everywhere. Attach context with `fn-error-context` or
`.context(...)`. Never silently swallow errors — the build CLI must fail loudly.
