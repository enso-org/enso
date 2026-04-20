# internal/

Small Node.js and Bazel helper scripts that wire the JS-side build to the rest
of the repo. Not published; ignored from linting.

## What each thing does

- `postinstall.mjs` — Runs after `pnpm install`. Generates tsconfig files (+
  similar outputs) via Bazel's `//:write_generated` (CI) or `//:write_all`
  (local). CI only writes untracked files, so stale committed tsconfigs don't
  silently break Windows builds — see PR #14956.
- `checkTsconfigs.mjs` — CI check that committed tsconfig files match what the
  generator would produce; fails PRs with drift.
- `stampFiles.bzl`, `workspaceStatus.mjs`, `stableStatus.mjs` — Bazel `--stamp`
  support. Produces version/commit metadata that gets embedded into GUI bundles.
- `generateVersionInfo.mjs`, `runWithVersionInfo.mjs` — Compute and inject
  version info (commit hash, release channel) at build time.
- `envReplacer.mjs` — Vite plugin-ish env-variable substitutor.
- `prettierJson.mjs` — Custom Prettier config entry for JSON.
- `dependenciesVersions.cjs` — Central catalog of deps versions to keep in sync
  between pnpm / Bazel / other consumers.

## Conventions

- ESM (`.mjs`) unless a CJS consumer forces `.cjs`.
- Keep each script single-purpose and independent of others — these run in a mix
  of install, CI, and bazel contexts.
- No TypeScript here — these must work before pnpm install completes.
