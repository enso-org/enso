---
name: testing-and-evaluation
description:
  Instructions for changes verification. It's a mandatory read when 1. asked for
  running any kind of tests, or 2. Finished implementing a milestone - always
  after finishing a plan.
---

# Required Verification steps

- Cheap steps, do after finishing a modification task:
  - typecheck of modified packages
  - prettier of modified files
- After every bigger step:
  - lint of modified packages
  - Unit tests of modified packages
- After reaching a milestone or finishing plan implementation:
  - if GUI part was changed, run playwright integration tests from app/gui
  - always, regardless of what was changed: electron package tests from
    app/electron-client, including AI tests.

# How to run Electron Package tests

Run `git submodule update --init --recursive` before the build. Without it
`--mode staging` silently falls back to defaults and the resulting binary fails
the very first e2e step (login screen never renders, console shows
`Failed to fetch`).

The ide build should be with staging environment by default (`--mode staging`
option in case of `./run ide build` script). The credentials file should be
provided by user. If working on a worktree, try to copy it from main repo.
