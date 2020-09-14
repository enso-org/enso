---
layout: developer-doc
title: Fallback Launcher Release Infrastructure
category: distribution
tags: [distribution, launcher, fallback]
order: 6
---

# Fallback Launcher Release Infrastructure

This document describes the fallback infrastructure that can be enabled to keep
launcher updates functioning even if the primary release provider stops working.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Fallback Mechanism in the Launcher](#fallback-mechanism-in-the-launcher)
- [Fallback Infrastructure Specification](#fallback-infrastructure-specification)
  - [Requirements Inherited from Current Launcher Upgrade Scheme](#requirements-inherited-from-current-launcher-upgrade-scheme)
- [Current Fallback Infrastructure Implementation](#current-fallback-infrastructure-implementation)
  - [Updating the Release List](#updating-the-release-list)
  - [Marking the Release As Broken](#marking-the-release-as-broken)

<!-- /MarkdownTOC -->

## Fallback Mechanism in the Launcher

The launcher has a built-in fallback mechanism for its self-upgrades. If a
request to the default release provider (currently GitHub Releases) fails, the
fallback mechanism is queried to see if it is enabled and available. If it is
enabled, the original request is re-tried with it. So in case the primary
release provider becomes permanently disabled, the fallback mechanism can be
enabled allowing launcher updates to function without forcing the users to do
the update manually.

Currently a fallback mechanism for engine releases is not provided and there are
no plans to do so, since if the engine release provider becomes obsolete, a new
launcher version can be released that will know about the new engine provider.

## Fallback Infrastructure Specification

The launcher assumes that the fallback infrastructure is located at the URL
[`https://launcherfallback.release.enso.org/`](https://launcherfallback.release.enso.org/)
and is accessible over HTTPS. This URL should not change to ensure that old
versions of the launcher can be upgraded.

Following files are required under that URL:

- `fallback-manifest.yaml` - determines if the fallback mechanism is enabled.
  Should contain a single key `enabled` that should be set to `true` to enable
  the fallback.
- `release-list.json` - contains a list of all available releases. It should
  contain a single field `releases` that is a list of entries. Each entry should
  have the following fields:
  - `tag` - tag associated with the release
  - `assets` - a list of filenames of assets available in the release, including
    the `broken` file if the release was
    [marked as broken](./release-policy.md#marking-a-release-as-broken).

Moreover, it should provide a subdirectory called `launcher`. This subdirectory
should contain directories for each release from the release list. Each
directory is named after the release's `tag`. This directory should contain the
assets listed in the release list. So assets are accessible under the URL
`https://launcherfallback.release.enso.org/launcher/<release-tag>/<asset-filename>`.

So for example, if the `release-list.json` has the following contents:

```json
{
  "releases": [
    {
      "tag": "0.1.0-test",
      "assets": [
        "broken",
        "launcher-manifest.yaml",
        "enso-launcher-0.1.0-test-linux-amd64.tar.gz"
      ]
    }
  ]
}
```

The launcher package should be accessible under the URL
`https://launcherfallback.release.enso.org/launcher/0.1.0-test/enso-launcher-0.1.0-test-linux-amd64.tar.gz`.

The fallback infrastructure must be able to provide all released launcher
versions.

### Requirements Inherited from Current Launcher Upgrade Scheme

The fallback releases must also adhere to the release scheme as it was defined
when the first launcher version was released. That brings the following
requirements:

- The version is a semantic versioning string.
- The tag has format `enso-<version>`.
- The release contains an asset called `launcher-manifest.yaml` which contains
  at least one key, `minimum-version-for-upgrade` which is a semantic versioning
  string.
- The release contains at least the following packages:
  - `enso-launcher-<version>-linux-amd64.tar.gz`
  - `enso-launcher-<version>-macos-amd64.tar.gz`
  - `enso-launcher-<version>-windows-amd64.zip`

## Current Fallback Infrastructure Implementation

Currently the fallback mechanism is disabled, as it should only be enabled in
case the primary release provider becomes permanently unavailable (or at least
in case the unavailability is not temporary and is known to span a long enough
time-frame that a decision is made that the fallback is necessary).

However, to be sure that we can easily enable it, the infrastructure is already
in a working state. It is currently built on top of an AWS S3 bucket which
provides the files explained in the
[specification above](#fallback-infrastructure-specification). Each release
uploaded to GitHub Releases is automatically uploaded to this S3 bucket so it
also acts as a backup.

If it is necessary, all that needs to be done to enable the fallback is to set
the `enabled` property in the manifest to `true`.

### Updating the Release List

When a new release is built on the CI, its packages are uploaded to the S3
bucket, but also the `release-list.json` has to be updated. This is handled by
downloading the file, appending to it and re-uploading it. This mechanism is not
safe from race conditions if multiple release jobs were running at the same
time, so we require that release jobs are invoked serially (and since new
releases are not added extremely frequently, this should not be an issue
currently). If this ever becomes a problem, a more complex solution can be
developed which synchronizes access to the release list.

### Marking the Release As Broken

When a release is marked as broken on GitHub (by uploading a file called
`broken` to the assets), a GitHub Action is triggered which uploads the same
file to the release and modifies the `release-list.json` to include the `broken`
asset.

The action uploads the added `broken` file to the release's files on S3 and
updates the `release-list.json` to include the broken mark in the asset list.
The update is done in the same way as
[described above](#updating-the-release-list), so broken marks should also be
added one at a time and not at the same time as releases are made to avoid race
conditions.
