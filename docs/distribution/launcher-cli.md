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
>   developed.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Commands](#commands)
  - [`new`](#new)
  - [`install engine`](#install-engine)
  - [`uninstall engine`](#uninstall-engine)
  - [`install distribution`](#install-distribution)
  - [`uninstall distribution`](#uninstall-distribution)
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
  - [`--version`](#--version)
  - [`--use-system-jvm`](#--use-system-jvm)
- [JVM Options](#jvm-options)

<!-- /MarkdownTOC -->

## Commands

### `new`

Create a new, empty project in a specified directory. By default uses the
`default` enso version, which can be overriden with `--version`.

Examples:

```bash
> enso new project1 --path /somewhere/on/the/filesystem
    # creates project called project1 in the specified directory
    # using the `default` Enso version
> enso new project2 --version 2.0.1
    # creates the project in the current directory, using the 2.0.1 version
```

### `install engine`

Installs a specific version of the Enso engine.

Examples:

```bash
> enso install engine 2.0.1
```

### `uninstall engine`

Uninstalls a specific version of the Enso engine.

Examples:

```bash
> enso uninstall engine 2.0.1
```

### `install distribution`

Installs a portable Enso distribution into system-defined directories, as
explained in
[Installed Enso Distribution Layout](./distribution.md#installed-enso-distribution-layout).
By default, it asks the user for confirmation, but this can be skipped by adding
a `--yes` flag.

Examples:

```
> extraction-location/bin/enso install distribution
This will install Enso to ~/.local/share/enso/.
Configuration will be placed in ~/.config/enso/.
The universal `enso` launcher will be placed in ~/.local/bin/.
Do you want to continue? [Y/n]
```

### `uninstall distribution`

Uninstalls an installed Enso distribution from the installation location
described in
[Installed Enso Distribution Layout](./distribution.md#installed-enso-distribution-layout).
It removes the universal launcher and all components. By default, it asks the
user for confirmation, but this can be skipped by adding a `--yes` flag.

Examples:

```
> enso uninstall distribution
This will completely uninstall Enso from ~/.local/share/enso/,
remove configuration from ~/.config/enso/
and the launcher script from ~/.local/bin/.
Do you want to continue? [y/N]
```

### `list`

Lists all installed versions of Enso and managed GraalVM distributions.

Optional arguments are `enso` to just list Enso installations or `runtime` to
list the installed runtimes.

Examples:

```bash
> enso list
Enso 2.0.1 -> GraalVM 20.1.0-java11
Enso 2.2.3 -> GraalVM 20.1.0-java11
Enso 3.0.0 -> GraalVM 21.1.0-java14
> enso list enso
2.0.1
2.2.3
3.0.0
> enso list runtime
GraalVM 20.1.0-java11 (used by 2 Enso installations)
GraalVM 21.1.0-java14 (used by 1 Enso installation)
```

### `default`

Sets the default Enso version used outside of projects.

If run without arguments, displays currently configured `default` version.

Examples:

```bash
> enso default 2.0.1
default set to version 2.0.1
> enso default
default version is 2.0.1
```

### `config`

Can be used to manage project configuration or global user configuration (if
outside a project or with the `--global` flag).

If only the config path is provided, currently configured value is printed.

Examples:

```bash
> enso config --global author.name Example User
> enso config author.name Example User
> enso config author.name
Example User
```

### `run`

Runs a project or an Enso script file.

Examples:

```bash
> enso run script.enso # runs the file in script mode
> enso run path/to/project1 # runs the project
> enso run # runs the current project based on current directory
```

### `repl`

Launches an Enso REPL. If outside a project, it uses the `default` Enso version.
If run inside a project or an optional project path is specified, the REPL is
run in the context of that project, using the version specified in its
configuration.

Examples:

```bash
> enso repl # version and context depend on current working directory
> enso repl /path/to/project # runs the REPL in context of the specified project
```

### `language-server`

Launches the language server for a given project.

Examples:

```bash
> enso language-server \
    --root-id 3256d10d-45be-45b1-9ea4-7912ef4226b1 \
    --path /tmp/content-root
```

### `upgrade`

Checks for updates of the launcher and downloads any new versions.

Examples:

```bash
> enso upgrade
Launcher has been upgraded to the latest (3.0.2) version.
> enso upgrade 2.0.1
Launcher has been downgraded to version 2.0.1.
```

### `version`

Prints the version of the installed launcher as well as the full version string
of the currently selected Enso distribution.

```bash
> enso version

Enso Launcher
Version:    0.0.1
Built with: scala-2.13.3 for GraalVM 20.1.0
Built from: main @ 919ffbdfacc44cc35a1b38f1bad5b573acdbe358
Running on: Linux 4.15.0-109-generic (amd64)
Currently selected Enso version:
Enso Compiler and Runtime
Version:    0.0.1
Built with: scala-2.13.3 for GraalVM 20.1.0
Built from: main @ 919ffbdfacc44cc35a1b38f1bad5b573acdbe358
Running on: OpenJDK 64-Bit Server VM, GraalVM Community, JDK 11.0.7+10-jvmci-20.1-b02
            Linux 4.15.0-108-generic (amd64)
```

Besides `enso version`, `enso --version` is also supported and yields the same
result.

### `help`

Print this summary of available command and their usage.

## General Options

### `--version`

Overrides the inferred (project local or `default`) version when running a
command.

### `--use-system-jvm`

Tells the launcher to use the default JVM (based on `JAVA_HOME`) instead of the
managed one. Will not work if the set-up JVM version is not GraalVM.

## JVM Options

For commands that launch an Enso component inside a JVM (`repl`, `run` and
`language-server`), parameters that the launcher does not know about (for
example introduced in versions of Enso newer than the launcher knows about) may
be passed after a double dash (`--`), i.e. `enso repl -- --someUnknownFlag`.

Moreover, it is possible to pass parameters to the JVM that is used to launch
these components, which may be helpful with debugging. A parameter of the form
`--jvm.argumentName=argumentValue` will be passed to the JVM as
`-DargumentName=argumentValue`.
