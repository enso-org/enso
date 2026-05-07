---
name: package-testing
description:
  Instructions for changes verification. It's a mandatory read when 1. asked for
  running any kind of tests, or 2. Finished implementing a milestone - always
  after finishing a plan.
---

# Required Verification steps

- Typecheck and lint of modified packages
- Unit tests of modified packages
- if GUI part was changed, run playwright integration tests from app/gui
- always after reaching a milestone or finishing plan implementation, regardless
  of what was changed: electron package tests from app/electron-client,
  including AI tests.

# How to run Electron Package tests

The ide build should be with staging environment by default (`--mode staging`
option in case of `./run ide build` script). The credentials file should be
provided by user. If working on a worktree, try to copy it from main repo.
