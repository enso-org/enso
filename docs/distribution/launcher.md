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
- [Project Management](#project-management)
  - [Creating a Project](#creating-a-project)
  - [Per-project Enso Version](#per-project-enso-version)
  - [Project Configuration](#project-configuration)
- [Enso and Graal Version Management](#enso-and-graal-version-management)
  - [GraalVM Override](#graalvm-override)
  - [Downloading Enso Releases](#downloading-enso-releases)
  - [Downloading GraalVM Releases](#downloading-graalvm-releases)
- [Running Enso Components](#running-enso-components)
  - [REPL](#repl)
  - [Running Scripts](#running-scripts)
  - [Running Project Files](#running-project-files)
  - [Language Server](#language-server)
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
README describing the distribution and basic usage.

The distribution structure is portable - this directory structure can be placed
anywhere in the system. The launcher places all components in this directory
structure by accessing them as relative to the location of its binary.

It is a good idea to add the `bin` folder to your system `PATH` so that you can
run the launcher just by typing `enso`.

## Project Management
The launcher provides basic project management utilities for the command-line
user.

### Creating a Project
It allows to create an empty project in a specified directory with the basic
configuration based on user's config.

### Per-project Enso Version
Project configuration can specify the exact Enso version that should be used
inside that project. The launcher automatically detects if it is in a project
(by traversing the directory structure). The current project can also be
specified by the `--path` parameter. All components launched inside a project
use the version specified in the configuration.

> The actionables for this section are:
> 
> - Decide how to support inexact project bounds (like `>=3.1, <4`) and
> resolvers.

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
automatically by the tool. When a new Enso version is installed, it also ensures
that the correct GraalVM version is installed and that it is used for launching
this version of Enso. When a managed GraalVM distribution is no longer used by
any installed version of Enso, it is automatically removed.

### GraalVM Override
While the launcher manages its own installation of GraalVM to ensure that the
right JVM version is used to launch each version of Enso, the user can override
this mechanism to use the installed system JVM instead. This is an advanced
feature and should rarely be used.

The launcher will check the system JVM and refuse to launch Enso if it is not a
GraalVM distribution. It will also print a warning if the major version is
different then required by that particular Enso version.

### Downloading Enso Releases
TODO GitHub releases

### Downloading GraalVM Releases
TODO GitHub releases of Graal CE

> The actionables for this section are:
> 
> - Should the launcher also allow to manage versions of Graal EE in the future?

## Running Enso Components
The primary purpose of the launcher is running various Enso components, namely
the REPL, running a project, Enso scripts or the language server.

The launcher automatically decides which Enso version to used, based on the
project configuration. If used outside an Enso project, the `default` version
from global user configuration is used.

Additional arguments passed to the launcher are forwarded to the launched
components.

### REPL
TODO

### Running Scripts
TODO

### Running Project Files
TODO

### Language Server
TODO

## Global User Configuration
The launcher allows to edit global user configuration, saved in the `config`
directory inside the Enso distribution structure.

This configuration specifies the `default` Enso version used outside of projects
and used for creating new projects. It also specifies default metadata used when
creating a project with the `new` command.

## Updating the Launcher
Besides managing Enso versions, the launcher has the ability to also update
itself.

### Minimal Required Launcher Version
Each version of Enso can specify the minimum version of launcher required to run
it. This version is (TODO?) specified in the release metadata on GitHub.

Moreover, if a given project uses some new build features, it may require a
newer version of the launcher. Thus, project configuration can also specify a
minimal required launcher version.

The launcher chooses the more recent of the two. If the launcher detects that
the installed version is too old to run a specific project / Enso version, it
performs a launcher version upgrade. If possible, the launcher should resume any
commands that were interrupted by the upgrade.
 
### Downloading Launcher Releases
TODO GitHub releases

#### Fallback Method
To ensure that the launcher can be safely updated even if the distribution
scheme changes, there should be support for a fallback upgrade scheme that is
triggered if the default upgrade process fails. It can be implemented by hosting
the latest version on some subdomain of `enso.org`.