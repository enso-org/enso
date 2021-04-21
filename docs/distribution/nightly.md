---
layout: developer-doc
title: Nightly Builds
category: distribution
tags: [distribution, release, nightly]
order: 10
---

# Nightly Builds

This document describes the infrastructure for nightly builds.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- TODO

<!-- /MarkdownTOC -->

## Scheduled Builds

After each weekday, at 4am UTC a scheduled workflow prepares a release from the latest commit on the `main` branch.

A new release is not made if there were no new commits to `main` since the previous release.

Each nightly build gets the current date appended to the version and since commits on `main` should have versions marked as SNAPSHOT (for example `1.2.3-SNAPSHOT`), the nightly release version will then look like `1.2.3-SNAPSHOT.1970-01-01`.

## Manual Builds

It is possible to trigger a nightly release manually by adding `[release: nightly]` to the commit message.

TODO: which branches this works on, handling collisions

## Release Retention

Only the 3 most recent nightly releases are kept. When a nightly release finishes building it removes any older releases to keep only the most recent 3.

Thus, setting a nightly version for a project will require updating it very soon (currenty after about 3 days). The launcher issues a warning when creating a project with a nightly version and if the version is not available it explains that this may be due to the retention policy.

