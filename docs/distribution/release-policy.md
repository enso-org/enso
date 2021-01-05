---
layout: developer-doc
title: Release Policy
category: distribution
tags: [distribution, release, release-policy, policy]
order: 3
---

# Release Policy

As an open-source project and programming language, it is incredibly important
that we have a well-defined release policy. This document defines said policy.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Versioning](#versioning)
  - [Launcher Versioning](#launcher-versioning)
- [Release Branches](#release-branches)
- [Release Workflow](#release-workflow)
  - [Tag Naming](#tag-naming)
  - [GitHub Releases](#github-releases)
  - [Release Notes](#release-notes)
- [Version Support](#version-support)
- [Working on the Current Release](#working-on-the-current-release)
- [Backporting Fixes](#backporting-fixes)

<!-- /MarkdownTOC -->

## Versioning

Releases of Enso are versioned using [semantic versioning](https://semver.org).
Where `a.b.c-tag` is the version string, `a` is the major version, `b`, is the
minor version, `c` is the patch version, and `tag` is additional metadata, the
following hold:

- Breaking changes to language behaviour or the public API will result in a
  major version increase.
- Addition of functionality in a backwards-compatible manner will result in a
  minor version increase.
- Backwards-compatible bug fixes will result in a patch version increase.
- The tag will indicate pre-release or beta versions, and will increase when any
  pre-release change is made. These are not intended to be stable.

### Launcher Versioning

The launcher is released alongside Enso releases, so the launcher version is
tied to the Enso version that it is released with.

## Release Branches

A release branch in the Enso repository is a branch prefixed with `release/`.
Release branches obey the following rules:

- One release branch exists per major version, and is named `release/n.x`, where
  `n` is the major version, and the rest is literal.
- A release branch must contain _tags_ corresponding to released versions of
  Enso. Once a release has been made, no further changes may be made to that
  release.
- A tagged release must contain a `CHANGELOG` file that describes the changes
  contained in that release.

It should be noted that general development still takes place on the `main`
branch of the repository.

## Release Workflow

Cutting a release for Enso proceeds as follows:

1.  If no release branch exists for the current major version, one should be
    created.
2.  Release notes should be made up to date.
3.  A commit representing the release should be tagged, and the tag pushed to
    GitHub.
4.  CI will create a draft release for this tag, as well as build and upload the
    appropriate artefacts.
5.  The release notes for the version being released should be copied into the
    release body on GitHub.
6.  The release must be verified by two members of the engine team, and the QA
    team.
7.  Once approval has been gained from these members, the release may be made
    official.

### Tag Naming

Tags for releases are named as follows `enso-version`, where `version` is the
semver string (see [versioning](#versioning)) representing the version being
released.

### GitHub Releases

A release is considered _official_ once it has been made into a release on
[GitHub](https://github.com/enso-org/enso/releases). Once official, a release
may not be changed in any way, except to mark it as broken.

#### Manifest File

Each GitHub release contains an asset named `manifest.yaml` which is a YAML file
containing metadata regarding the release. The manifest is also included in the
root of an Enso version package. It has at least the following fields:

- `minimum-launcher-version` - specifies the minimum version of the launcher
  that should be used with this release of Enso,
- `graal-vm-version` - specifies the exact version of GraalVM that should be
  used with this release of Enso,
- `graal-java-version` - as GraalVM versions may have different variants for
  different Java versions, this specifies which variant to use.

For example:

```yaml
minimum-launcher-version: 0.0.1
graal-vm-version: 20.1.0
graal-java-version: java11
```

The `minimum-launcher-version` should be updated whenever a new version of Enso
introduces changes that require a more recent launcher version. This value is
stored in
[`distribution/manifest.template.yaml`](../../distribution/manifest.template.yaml)
and other values are added to this template at build time.

#### Release Assets Structure

Each release contains a build of the Enso engine and native launcher binaries
for each supported platform. Moreover, for convenience, it should include
bundles containing native launcher binaries and the latest engine build for each
platform. So each release should contain the following assets:

- `enso-bundle-<version>-linux-amd64.zip`
- `enso-bundle-<version>-macos-amd64.zip`
- `enso-bundle-<version>-windows-amd64.zip`
- `enso-engine-<version>.zip`
- `enso-launcher-<version>-linux-amd64.zip`
- `enso-launcher-<version>-macos-amd64.zip`
- `enso-launcher-<version>-windows-amd64.zip`
- `manifest.yaml`

#### Marking a Release as Broken

We intend to _never_ delete a release from GitHub, as users may have projects
that depend on specific versions of Enso. Instead, we provide a mechanism for
marking releases as broken that works as follows:

- An empty file named `broken` is uploaded to the release.
- The release description is edited to visibly mark the release as broken.

A broken release is one that _must not_ be downloaded by the launcher unless a
project specifies _an exact version match_, and it _must not_ be used in new
projects by the launcher unless _explicitly_ specified by the user as an exact
version match.

### Release Notes

Release notes should contain a summary of the changes made between the last
release and the current release. They should follow the template given below:

```md
# Enso x.y.z (YYYY-MM-DD)

## Language

- A list of language-level changes.

## Type System

- A list of type-system changes.

## Interpreter

- A list of changes to the Enso interpreter.

## Runtime

- A list of changes to the Enso runtime.

## Tooling

- A list of changes to the Enso language tooling.

## Libraries

- A list of changes to the Enso core libraries.

## Stabilised Features

- A list of stabilised APIs and/or features.

## Misc

- A list of miscellaneous changes.

## Internal Only

- A list of changes that do not have user-facing impact, but represent
  significant improvements to the internals of Enso and related tools.
```

If there are no changes for a section, the section should contain a bullet point
that reads "Nothing".

The changelog file is an ongoing record of changes, and may diverge between
`main` and the various release branches.

## Version Support

We aim to support a given major version for some period of time after the
release of the next major version. For a detailed breakdown of the major
versions that are currently supported, please see the [security](./security.md)
document.

## Working on the Current Release

When working on the current release, development should take place against the
`main` branch. When it is time to cut a release, the new commits on the main
branch are cherry-picked onto the current release branch. From there, the
release proceeds as described in [release workflow](#release-workflow) above.

## Backporting Fixes

Supporting a major version for some time after the release of the next major
version will sometimes require backporting a fix to the previous major version
from the current version or from `main`.

Backporting should only be used for applying _fixes_, not the addition of new
features.

The process for performing such a backport is as follows:

1.  Create a new branch called `backport/version/fix-name`, where `version`
    matches the version string of the corresponding release branch. This branch
    should branch off the corresponding release branch.
2.  Back-port the fix to the newly created `backport` branch. This can be done
    by:
    - Cherry-picking the commit and performing fixups (preferred).
    - Re-implementing the fix manually (if cherry-picking will not work due to
      progression of the codebase).
3.  Submit your `backport/version/fix-name` branch for review as a pull-request
    into the `release/version` branch.
4.  Once the PR has passed CI and been approved by the appropriate reviewers, it
    can be merged into the release branch.
