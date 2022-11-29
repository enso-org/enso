---
layout: developer-doc
title: Nightly Builds
category: distribution
tags: [distribution, release, nightly]
order: 10
---

# Nightly Builds

This document describes the infrastructure for Enso's automated nightly builds.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Triggering the Build](#triggering-the-build)
- [Nightly Build Versions](#nightly-build-versions)
- [Release Notes](#release-notes)

<!-- /MarkdownTOC -->

## Triggering the Build

The build can be triggered by two possible events:

- automatically, at 5am UTC after each working day (that is, on Tuesday to
  Saturday),
- manually, dispatching the
  [nightly.yml](https://github.com/enso-org/enso/actions/workflows/nightly.yml)
  workflow.

The nightly build is based off of the state of the `develop` branch at the
moment when it was triggered, unless a different branch has been specified when
triggering the build.

Thanks to
[GitHub's concurrency settings](https://docs.github.com/en/actions/reference/workflow-syntax-for-github-actions#concurrency),
only one nightly or regular release job may be running at a given time. If a
build is triggered while another one is in progress, it will be pending until
the first one finishes.

## Nightly Build Versions

Build script automatically generates a new version for each nightly build, based
on the current date and previous tags.

The nightly release version is consists of the following parts:

- core version (`major.minor.patch`), which is the next stable version after the
  last release,
- prerelease suffix (`-nightly.YYYY.MM.DD`), where `YYYY.MM.DD` is the date of
  the build,
- optionally, index suffix (`.N`), where `N` is the index of the nightly build
  on a given day.

For example, if the last stable release was `2021.1.1`, the next nightly build
will be `2021.2.0-nightly.2021.05.10`.

Nightly releases are periodically removed from the GitHub releases page, so they
should not be used for anything other than testing.

As each release has a default edition associated with it, so do the nightlies.
We follow convention that the edition name is the same as the version. This
convention should not be relied upon, as it may change in the future.

## Release Notes

The release notes for nightly builds are generated automatically from the
template in `build/build/release-body.md`. The template is filled with a number
of placeholders, which are then replaced with the actual values. Please refer to
the file to learn more.

## Changelog

Each PR should update the first, `Enso Next`, section in `CHANGELOG.md`. These
changes will be later moved to the specific section for the full release. This
section is also used to fill the release notes for the nightly builds.

Most PRs should update the release notes, so there is a PR check that ensures
the file was modified. However, in some situations there is no need to update
the notes. In such case `CI: No changelog needed` label should be added to the
PR, so the check is skipped.

The changelog should keep consistent formatting:

- each version should be delimited by a top-level section (`#` in Markdown),
- the first section should always be called `Enso Next`,
- all subsequent sections should be called `Enso <version> (<date>)`.
