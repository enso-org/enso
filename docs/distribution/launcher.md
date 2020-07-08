---
layout: developer-doc
title: Enso Launcher
category: distribution
tags: [distribution, launcher]
order: 4
---

# Enso Launcher
The launcher is used to run Enso commands (like the REPL, language server etc.)
and seamlessly manage Enso versions. This document describes it's features. Its
command-line interface is described in the [CLI](./launcher-cli.md) document.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Launcher Distribution](#launcher-distribution)
- [Launcher Build](#launcher-build)
  - [Portability](#portability)
- [Project Management](#project-management)
  - [Creating a Project](#creating-a-project)
  - [Per-Project Enso Version](#per-project-enso-version)
  - [Project Configuration](#project-configuration)
- [Enso and Graal Version Management](#enso-and-graal-version-management)
  - [GraalVM Override](#graalvm-override)
  - [Downloading Enso Releases](#downloading-enso-releases)
  - [Downloading GraalVM Releases](#downloading-graalvm-releases)
- [Running Enso Components](#running-enso-components)
- [Global User Configuration](#global-user-configuration)
- [Updating the Launcher](#updating-the-launcher)
  - [Minimal Required Launcher Version](#minimal-required-launcher-version)
  - [Downloading Launcher Releases](#downloading-launcher-releases)

<!-- /MarkdownTOC -->

## Launcher Distribution
The launcher is distributed as a native binary for each platform (Windows,
Linux, Mac). It is distributed in a ZIP archive as described in
[Enso Home Layout](./distribution.md#enso-home-layout), except that the
component directories are empty (they will be populated when Enso versions are
downloaded by the launcher on-demand). The only non-empty directory is `bin`
which is where the launcher binary is placed. The ZIP file should also contain a
[README](../../distribution/launcher/README.md) describing the distribution and
basic usage.

The distribution structure is portable - this directory structure can be placed
anywhere in the system. The launcher places all components in this directory
structure by accessing them as relative to the location of its binary.

## Launcher Build
The launcher is built using
[GraalVM Native Image](https://www.graalvm.org/docs/reference-manual/native-image/)
which compiles the JVM code into a native binary ahead of time, resulting in a
small and fast launching executable.

### Portability
On Linux, it is possible to statically link all libraries required by the Native
Image, thus ensuring portability between Linux distributions.

On Windows and Mac, it is not possible to statically link against system
libraries, but this should not hinder portability as the system libraries are
generally compatible between distribution versions on these platforms.
Non-system dependencies are included in the binary on these platforms as well.

## Project Management
The launcher provides basic project management utilities for the command-line
user.

### Creating a Project
It allows to create an empty project in a specified directory with the basic
configuration based on user's config.

### Per-Project Enso Version
Project configuration can specify the exact Enso version that should be used
inside that project. The launcher automatically detects if it is in a project
(by traversing the directory structure). The current project can also be
specified by the `--path` parameter. All components launched inside a project
use the version specified in the project configuration, or outside a project,
the default version.

> The actionables for this section are:
> 
> - Decide how to support inexact project bounds (like `>=3.1, <4` - when should
>   the launcher check for new versions) and resolvers.
> - Decide how to support nightly builds.

### Project Configuration
The command-line allows to edit project configuration, for example: change the
author's name or Enso version.

## Enso and Graal Version Management
The launcher automatically manages required Enso versions. When running inside a
project, if the version specified in project configuration is not installed, it
is installed automatically.

If an Enso version is no longer needed, it can be removed with the `uninstall`
command.

Moreover, GraalVM distributions tied to the installed Enso versions are managed
automatically by the launcher. When a new Enso version is installed, it also
ensures that the correct GraalVM version is installed and that it is used for
launching this version of Enso. When a managed GraalVM distribution is no longer
used by any installed version of Enso, it is automatically removed.

### GraalVM Override
While the launcher manages its own installation of GraalVM to ensure that the
right JVM version is used to launch each version of Enso, the user can override
this mechanism to use the installed system JVM instead. This is an advanced
feature and should rarely be used.

The launcher will check the system JVM and refuse to launch Enso if it is not a
GraalVM distribution. It will also print a warning if the major or minor version
is different then required by that particular Enso version.

### Downloading Enso Releases
The releases are discovered and downloaded using the
[GitHub API](https://docs.github.com/en/rest/reference/repos#releases). As
described in the [Release Policy](./release-policy.md#github-releases), each
release contains a manifest file that is downloaded first. It specifies if this
Enso version can be used with the current launcher or an upgrade is needed, as
described in
[Minimal Required Launcher Version](#minimal-required-launcher-version). If the
version is correct, the binary file containing the Enso components distribution
is downloaded. The manifest also specifies which GraalVM version should be used
with this version of Enso. If that version of GraalVM is not present on the
system it is also downloaded and installed.

Releases [marked as broken](./release-policy.md#marking-a-release-as-broken) are
ignored by the launcher unless it is specified by an exact version match. In
that case it is downloaded, but a warning is printed.

### Downloading GraalVM Releases
GraalVM is downloaded from its
[GitHub releases page](https://github.com/graalvm/graalvm-ce-builds/releases)
using GitHub API, similarly as Enso releases.

## Running Enso Components
The primary purpose of the launcher is running various Enso components, namely
the REPL, running a project, Enso scripts or the language server.

The launcher automatically infers which Enso version to used, based on the
parameters and configuration:

 - When running a project or the language server, the version specified in
   project configuration is used.
 - When running the REPL, if the current directory is inside a project,
   the project version is used, otherwise the `default` version from the global
   configuration is used. The current path is the working directory unless
   overridden with the `--path` parameter.
 - When running an Enso script, if that script is located inside a project, the
   project version is used, if it is outside a project, the `default` version is
   used.

Additional arguments passed to the launcher are forwarded to the launched
component.

## Global User Configuration
The launcher allows to edit global user configuration, saved in the `config`
directory inside the Enso distribution structure.

This configuration specifies the `default` Enso version used outside of projects
and used for creating new projects. It also specifies default project metadata
used when creating a project with the `new` command.

## Updating the Launcher
Besides managing Enso versions, the launcher has the ability to also update
itself. By default it is updated to the latest version available, but it also
allows downgrades by specifying a version explicitly.

### Minimal Required Launcher Version
Each version of Enso can specify the minimum version of launcher required to run
it. This version is specified in a
[manifest file](./release-policy.md#manifest-file) that should be included as an
artifact for every Enso release.

Moreover, if a given project uses some new build features, it may require a
newer version of the launcher. Thus, project configuration can also specify a
minimal required launcher version.

If the launcher detects that the installed version is older than one of the two
criteria above, it asks the user to upgrade the launcher using the `upgrade`
command.
 
### Downloading Launcher Releases
The launcher is released alongside Enso, so each new release of Enso also
contains native artifacts for the launcher for each platform. They are
downloaded in the same way as Enso distribution. The launcher does not have to
be updated as often as Enso itself - it only has to be updated when a project or
a new Enso version requires a more recent launcher or the user explicitly wants
to.

#### Fallback Method
To ensure that the launcher can be safely updated even if the distribution
scheme changes, there should be support for a fallback upgrade scheme that is
used if the default upgrade process fails.

This fallback scheme is only intended for situations where the current default
scheme is broken indefinitely. So for simplicity, it does not allow to choose an
arbitrary version but only to upgrade to the latest version of the launcher. In
the very rare case in which the user wants to downgrade after the default
distribution scheme has changed, they have to first upgrade to the latest
version of the launcher which will use a new distribution scheme. Then, on that
latest version, it may be possible to downgrade back to an old version (which is
distributed on the new distribution scheme).

Thus, when migrating to a new distribution scheme, old versions should also be
preserved, but the fallback upgrade scheme does not have to keep track of all
the versions, but only the latest one.
 
It can be implemented by uploading the most recent artifacts to some fixed
domain, like `launcherupgrade.release.enso.org`.
