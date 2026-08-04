# enso-build-ci-gen

Binary that emits the repo's `.github/workflows/*.yml` files from the job
definitions in `enso-build`. Serializes via `serde_yaml`.

Run when:

- You add or remove a CI job.
- You change matrix expansions (OS/JDK/Node versions).
- You restructure the workflow file layout.
- You flip `RELEASE_RUNNER_TYPE` in `enso-build`'s `ci_gen` module — the knob
  that moves the release pipeline (`promote.yml` + `release.yml`, used by the
  nightly) between the self-hosted fleet and GitHub-hosted runners.
- You flip `RELEASE_DEPLOYS_RUNTIME_TO_CLOUD` (same module) — whether releases
  push the runtime image to ECR and dispatch the Cloud build-image workflow.
  Off while the Enso Cloud is disabled; requires a valid `CI_PRIVATE_TOKEN` to
  re-enable.
- You change `MACOS_BACKEND_FALLBACK_RELEASE` (same module) — when set, the
  macOS backend job is omitted and the macOS IDE embeds the engine bundle of
  the pinned older release (GitHub-hosted macs can't fit the native-image
  build). `None` restores normal macOS backend builds.
- You flip `MACOS_SIGN_ARTIFACTS` (same module) — whether the macOS IDE build
  signs and notarizes. Off while Apple notarization is unavailable (expired
  Developer agreement / no portal access); the app then needs manual Gatekeeper
  approval.

Check in the regenerated YAML as part of the same commit. The CI enforces that
the checked-in YAML matches what this tool would produce, so drift fails PRs.
