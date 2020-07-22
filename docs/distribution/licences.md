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
  - [Launcher](#launcher)
  - [Engine Components](#engine-components)

<!-- /MarkdownTOC -->

## Gathering Used Dependencies

As a first step, we need to gather a list of which dependencies are used in the
distributed artifacts.

### SBT

We can use the plugin `sbt-license-report` to gather a list of used dependencies
and their licences. To use it, run `enso/dumpLicenseReport` in the SBT shell.
This will gather dependency information for all the subprojects. For each
subproject, `license-reports` directory will be created in its `target`
directory, containing the reports in multiple formats.

It is important to note that the report for the root `enso` project will not
contain dependencies of the subprojects. Instead, to list the relevant
dependencies, you need to look at the reports for the subprojects that are
actually built into distributable artifacts. For now these are: `runtime`,
`runner`, `project-manager` and `launcher`.

#### `sbt-license-report` Configuration

Settings for the plugin are defined in the `licenseSettings` variable in
[`build.sbt`](../../build.sbt). The settings have to be applied to each project
by adding `.settings(licenseSettings)` to the project definition (defining these
settings at the top level or in the `ThisBuild` configuration yielded no
effects, so this workaround is required).

The most relevant setting is `licenseConfigurations` which defines which `ivy`
configurations are considered to search for dependencies.

Currently it is set to only consider `compile` dependencies, as dependencies for
`provided`, `test` or `benchmark` are not distributed.

### Rust

We do not distribute any Rust-based artifacts in this repository.

> The actionables for this section are:
>
> - When the parser is rewritten to Rust and is distributed within the
>   artifacts, this section should be revisited to describe a scheme of
>   gathering dependencies used in the Rust projects.

## Preparing the Distribution

When a new dependency is added, its transitive dependencies have to be analysed
as described in the previous section. Various action has to be taken depending
on the particular licences.

For most dependencies under the Apache, MIT or BSD licences, a copy of the
licence has to be included within the distribution and if the dependency
includes a `NOTICE` file, the contents of this file have to be reproduced within
the distribution by adding them to an aggregate `NOTICE` file. To find these
`NOTICE` files it may be necessary to walk through the `JAR`s containing the
dependencies or visit project websites of each dependency.

### Launcher

As the launcher is distributed as a native binary executable, the licences and
notices have to be included separately.

The licences should be put in the
[`distribution/launcher/components-licences`](../../distribution/launcher/components-licences)
directory. The notices should be gathered in
[`distribution/launcher/NOTICE`](../../distribution/launcher/NOTICE). These
files are included by the CI build within the built artifacts.

### Engine Components

> The actionables for this section are:
>
> - The engine components as distributed as a JAR archive that everyone can
>   open. At least some of the dependencies contain their licences within that
>   archive. It should be checked if this is enough or if the licences and
>   notices should be replicated in the distributed packages anyway.

### Bundles

Beside the launcher and engine components, the distributed bundles also contain
a distribution of GraalVM CE. This distribution however contains its own licence
and notices within itself, so no further action should be necessary in that
regard.
