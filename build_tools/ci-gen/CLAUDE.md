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

Check in the regenerated YAML as part of the same commit. The CI enforces that
the checked-in YAML matches what this tool would produce, so drift fails PRs.
