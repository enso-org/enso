---
layout: developer-doc
title: Enso Launcher CLI
category: distribution
tags: [distribution, launcher, cli, command]
order: 4
---

# Enso Launcher CLI
This document describes available command-line options of the Enso launcher.

This document is a DRAFT. The explanations are just to give an idea of the
commands. It will be updated with detailed explanations when the CLI is
developed.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Commands](#commands)
  - [`new`](#new)
  - [`install`](#install)
  - [`uninstall`](#uninstall)
  - [`list`](#list)
  - [`default`](#default)
  - [`config`](#config)
  - [`run`](#run)
  - [`repl`](#repl)
  - [`language-server`](#language-server)
  - [`upgrade`](#upgrade)
  - [`version`](#version)
  - [`help`](#help)
- [General Options](#general-options)
  - [`--path`](#--path)
  - [`--version`](#--version)
  - [`--use-system-jvm`](#--use-system-jvm)

<!-- /MarkdownTOC -->

## Commands

### `new <project-name>`
Create a new, empty project in a specified directory.
By default uses the `default` enso version, which can be overriden with
`--version`.

Examples:
```bash
enso new project1
enso new project2 --version 2.0
```

### `install <version-string>`
Installs a specific version of Enso.

### `uninstall <version-string>`
Uninstalls a specific version of Enso.

### `list`
Lists all installed versions of Enso and managed GraalVM distributions.

### `default <version-string>`
Sets the default Enso version used outside of projects.

If run without arguments, displays currently configured `default` version.

### `config [--global] <config-path> <config-value>`
Can be used to manage project configuration or global user configuration (if
outside a project or with the `--global` flag).

These are mostly used for configuration of newly created projects.

Examples:
```
enso config --global user.name Example User
``` 

### `run <filename>`
Runs a project or an Enso script file.

### `repl`
Launches an Enso repl.

### `language-server <server-parameters>`
Launches the language server for a given project.

### `upgrade [optional: exact version]`
Checks for updates of the launcher and downloads any new versions.

### `version`
Prints the version of the installed launcher as well as the full version string
of the currently selected Enso distribution.

### `help`
Print this summary of available command and their usage.

## General Options

### `--path=<path>`
Specify path to the current project. Configuration from this project is used to
determine the Enso version.

If not specified, the directory tree is traversed to find if one of the parent
directories contains an Enso project. If no project is found, the `default`
version is used.

### `--version=<version>`
Overrides the inferred (project local or `default`) version when running a
command.

### `--use-system-jvm`
Tells the launcher to use the default JVM (based on `JAVA_HOME`) instead of the
managed one. Will not work if the set-up JVM version is not GraalVM.