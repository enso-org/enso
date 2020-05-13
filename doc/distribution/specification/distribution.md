# The Enso Distribution
This document describes the behaviour of the first prototype of an Enso
distribution, together with some future expansion plans.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Installation Home Layout](#installation-home-layout)
- [Universal Launcher Script](#universal-launcher-script)
- [Layout of a Distribution of a Single Version of Enso](#layout-of-a-distribution-of-a-single-version-of-enso)
    - [Package Sets](#package-sets)

<!-- /MarkdownTOC -->

## Installation Home Layout
*Please keep in mind that this system is just a proposal and none of it is
implemented yet.*

All of Enso's binaries, packages, etc. are installed into a directory in
the user's home directory. For macOS and linux distributions that's `~/.enso`,
by default. The distribution is fully portable, so it never makes any
assumptions about actually being placed in any particular directory.

The directory structure is as follows:

```
~/.enso
├── bin
│   └── enso                  # the universal launcher script, responsible for choosing the appropriate compiler version
├── dist                      # per-compiler-version distribution directories
│   ├── default -> enso-1.2.0 # a symlink to the version that should be used when no version is explicitely specified
│   ├── enso-1.0.0            # a full distribution of given Enso version, described below
│   │   └── <truncated>
│   └── enso-1.2.0            # a full distribution of given Enso version, described below
│       └── <truncated>
└── jvm                       # a directory storing (optional) distributions of the JVM used by the Enso distributions
    └── graalvm-ce-27.1.1
```

> The actionables for this section are:
>
> - Determine the appropriate method for per-user installation on windows.

## Universal Launcher Script
The universal launcher script should be able to launch the proper version of
Enso executable based on the version specified in the project being run,
or use the default version if none specified.

> The actionables for this section are:
>
> - Determine the proper technology to implement the script (i.e. Java/Bash).
> - Determine the features needed in the launcher script.

## Layout of a Distribution of a Single Version of Enso
This section describes the structure of a single version distribution. This
system is intended to be implemented first and used e.g. for the Enso nightly
builds / releases.

Such a distribution can be used as a standalone version, assuming the required
version of the JVM is installed and used.

The layout of such a distribution is as follows:

```
enso-1.0.0
├── bin           # Contains all the executable tools and their dependencies.
│   └── enso.jar  # Che main executable of the distribution. CLI entry point.
├── lib           # Contains all the installed libraries for this compiler version.
│   ├── Http      # Every library directory may contain multiple versions of the same library.
│   │   ├── 3.1.0 # Every version sub-directory is just an Enso package containing the library.
│   │   │   ├── package.yaml
│   │   │   ├── polyglot
│   │   │   └── src
│   │   │       ├── Http.enso
│   │   │       └── Socket.enso
│   │   └── 5.3.5
│   │       ├── package.yaml
│   │       ├── polyglot
│   │       └── src
│   │           ├── Http.enso
│   │           └── Socket.enso
│   └── Base
│       └── 1.0.0
│           ├── package.yaml
│           └── src
│               ├── List.enso
│               ├── Number.enso
│               └── Text.enso
└── package_sets # package sets for non-package run modes (described below)
    ├── default.yaml
    └── my_package_set.yaml
```

### Package Sets
A package set is a manifest containing library versions that are made available
for importing when no `package.yaml` is available (e.g. when using the CLI to
run a single file). The package set used is `default.yaml` and there is no way
of changing the behavior currently.

Example contents of the `default.yaml` file are as follows:

```yaml
libraries:
  - name: Base
    version: 1.0.0
  - name: Http
    version: 5.3.5
```
