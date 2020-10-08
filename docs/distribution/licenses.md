---
layout: developer-doc
title: Licenses
category: distribution
tags: [distribution, licenses]
order: 6
---

# Licenses

When distributing Enso, we include code from many dependencies that are used
within it. We need to ensure that we comply with the licenses of the
dependencies that we distribute with Enso.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Gathering Used Dependencies](#gathering-used-dependencies)
  - [SBT](#sbt)
  - [Rust](#rust)
- [Preparing the Distribution](#preparing-the-distribution)
  - [Review](#review)
- [Standard Library](#standard-library)
- [Bundles](#bundles)

<!-- /MarkdownTOC -->

## Gathering Used Dependencies

As a first step, we need to gather a list of which dependencies are used in the
distributed artifacts.

### SBT

We use a `GatherLicenses` task that uses the `sbt-license-report` and other
sources to gather copyright information related to the used dependencies.

To configure the task, `GatherLicenses.distributions` should be set with
sequence of distributions. Each distribution describes one component that is
distributed separately and should include all references to all projects that
are included as part of its distribution. Currently, we have the `launcher`
distribution that consists of one `launcher` component and the `engine`
distribution which includes `runtime`, `engine-runner` and `project-manager`.

Another relevant setting is `GatherLicenses.licenseConfigurations` which defines
which `ivy` configurations are considered to search for dependencies. Currently
it is set to only consider `compile` dependencies, as dependencies for
`provided`, `test` or `benchmark` are not distributed and there are no
`assembly`-specific dependencies.

`GatherLicenses.configurationRoot` specifies where the review tool will look for
the files specifying review state and `GatherLicenses.distributionRoot`
specifies where the final notice packages should be generated.

To run the automated license gathering task, run `enso/gatherLicenses` in SBT.
This will create a report and packages which are described in the
[next section](#preparing-the-distribution).

### Rust

We do not distribute any Rust-based artifacts in this repository.

> The actionables for this section are:
>
> - When the parser is rewritten to Rust and is distributed within the
>   artifacts, this section should be revisited to describe a scheme of
>   gathering dependencies used in the Rust projects.
> - It would be good to re-use the SBT task as much as possible, possibly by
>   creating a frontend for it using `cargo-license`.

## Preparing the Distribution

When a new dependency is added, the `enso/gatherLicenses` should be re-run to
generate the updated report.

The report can be opened in review mode by launching a server located in
`tools/legal-review-helper` by running `npm start` in that directory.
Alternatively, `enso/openLegalReviewReport` can be used instead to automatically
open the report in review-mode after generating it (but it requires `npm` to be
visible on the system PATH in SBT).

The report will show what license the dependencies use and include any copyright
notices and files found within each dependency.

Each copyright notice and file should be reviewed to decide if it should be kept
or ignored. If a notice is automatically detected in a wrong way, it should be
ignored and a fixed one should be added manually. The review process is
described in detail in the [next section](#review).

Each new type of license has to be reviewed to ensure that it is compatible with
our distribution and usage scheme. Licenses are reviewed per-distribution, as
for example the binary distribution of the launcher may impose different
requirements than distribution of the engine as JARs.

After the review is done, the `enso/gatherLicenses` should be re-run again to
generate the updated packages that are included in the distribution. Before a PR
is merged, it should be ensure that there are no warnings in the generation. The
packages are located in separate subdirectories of the `distribution` directory
for each artifact.

> This may possibly be automated in the next PR that includes the current legal
> review. The generating task could write a file indicating if there were any
> warnings and containing a hash of the dependency list. The build job on CI
> could then run a task `verifyLegalReview` which would check if there are no
> warnings and if the review is up to date (by comparing the hash of the
> dependency list at the time of the review with the current one).

> TODO [RW] currently the auto-generated notice packages are not included in the
> built artifacts. That is because the legal review settings have not yet been
> prepared. Once that is done, the CI should be modified accordingly. This will
> be updated in the next PR.

### Review

The review can be performed manually by modifying the settings inside of the
`tools/legal-review` directory or it can be partially automated.

#### Review Process

> TODO [RW] write details

1. Open the review in edit mode using the helper script.
   - You can type `enso / openLegalReviewReport` if you have `npm` in your PATH
     as visible from SBT.
   - Or you can just run `npm start` (and `npm install` if needed) in the
     `tools/legal-review-helper` directory.
1. Review licenses
   - ...
1. Review which files to include
   - ...
1. Review copyright notices
   - ...
1. Ensure that there are no more warnings.
1. Re-run `enso/gatherLicenses`.

The updates performed using the web script are remembered locally, so they will
not show up after the refresh. If you ever need to open the edit mode after
closing its window, you should re-generate the report using
`enso/gatherLicenses` or just open it using `enso/openLegalReviewReport` which
will refresh it automatically.

#### Review Configuration

The review state is driven by configuration files located in
`tools/legal-review`. This directory contains separate subdirectories for each
artifact.

The subdirectory for each artifact may contain the following entries:

- `notice-header` - contains the header that will start the main generated
  NOTICE
- `files-add` - directory that may contain additional files that should be added
  to the notice package
- `reviewed-licenses` - directory that may contain files for reviewed licenses;
  the files should be named with the normalized license name and they should
  contain a path to that license's file
- and for each dependency, a subdirectory named as its `packageName` with
  following entries:
  - `files-add` - directory that may contain additional files that should be
    added to the subdirectory for this package
  - `files-keep` - a file containing names of files found in the package sources
    that should be included in the package
  - `files-ignore` - a file containing names of files found in the package
    sources that should not be included
  - `custom-license` - a file that indicates that the dependency should not
    point to the default license, but it should contain a custom one within its
    files
  - `copyright-keep` - copyright lines that should be included in the notice
    summary for the package
  - `copyright-keep-context` - copyright lines that should be included
    (alongside with their context) in the notice summary for the package
  - `copyright-ignore` - copyright lines that should not be included in the
    notice summary for the package
  - `copyright-add` - a single file whose contents will be added to the notice
    summary for the package

Manually adding files and copyright, modifying the notice header and marking
licenses as reviewed has to be done manually. But deciding if a file or
copyright notice should be kept or ignored can be done much quicker using the
GUI launched by `enso/openLegalReviewReport`. The GUI is a very simple one - it
assumes that the report is up to date and uses the server to modify the
configuration. The configuration changes are not refreshed automatically -
instead if the webpage is refreshed after modifications it may contain stale
information - to get up-to-date information, `enso/openLegalReviewReport` or
`enso/gatherLicenses` has to be re-run.

## Standard Library

The dependencies of standard library are built using Maven, so they have to be
handled separately. Currently there are not many of them so this is handled
manually. If that becomes a problem, they could be attached to the frontend of
the `GatherLicenses` task.

The third-party licenses for Java extensions of the standard library are
gathered in the `third-party-licenses` directory in the `Base` library. The
gathering process is partially-automatic, triggered by the `package` goal of the
associated Maven configuration file. However when another dependency is added to
the standard library, its licenses should be reviewed before merging the PR.

## Bundles

Beside the launcher and engine components, the distributed bundles also contain
a distribution of GraalVM CE. This distribution however contains its own licence
and notices within itself, so no further action should be necessary in that
regard.
