---
layout: developer-doc
title: Enso Launcher CLI
category: distribution
tags: [distribution, launcher, cli, command]
order: 5
---

# Enso Launcher CLI
This document describes available command-line options of the Enso launcher.

> The actionables for this document are:
>
> - This document is a draft. The explanations are just to give an idea of the
>   commands. It will be updated with detailed explanations when the CLI is
>    developed.

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

### `new`
Create a new, empty project in a specified directory.
By default uses the `default` enso version, which can be overriden with
`--version`.

Examples:
```bash
enso new project1 --path /somewhere/on/the/filesystem
    # creates project called project1 in the specified directory
    # using the `default` Enso version
enso new project2 --version 2.0.1
    # creates the project in the current directory, using the 2.0.1 version
```

### `install`
Installs a specific version of Enso.

Examples:
```bash
enso install 2.0.1
```

### `uninstall`
Uninstalls a specific version of Enso.

Examples:
```bash
enso uninstall 2.0.1
```

### `list`
Lists all installed versions of Enso and managed GraalVM distributions.

### `default`
Sets the default Enso version used outside of projects.

If run without arguments, displays currently configured `default` version.

Examples:
```bash
enso default 2.0.1
enso default # prints 2.0.1
```

### `config`
Can be used to manage project configuration or global user configuration (if
outside a project or with the `--global` flag).

If only the config path is provided, currently configured value is printed.

Examples:
```bash
enso config --global user.name Example User
enso config author.name Example User
enso config author.name # prints Example User
``` 

### `run`
Runs a project or an Enso script file.

Examples:
```bash
enso run script.enso # runs the file in script mode
enso run path/to/project1 # runs the project
enso run # runs the current project based on current directory
``` 

### `repl`
Launches an Enso repl.

Examples:
```bash
enso repl
``` 

### `language-server`
Launches the language server for a given project.

Examples:
```bash
enso language-server 
  --server \
  --root-id 3256d10d-45be-45b1-9ea4-7912ef4226b1 \
  --path /tmp/content-root
```

### `upgrade`
Checks for updates of the launcher and downloads any new versions.

Examples:
```bash
enso upgrade 2.0.1
enso upgrade # selects the latest version
``` 

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