---
layout: developer-doc
title: The Enso Distribution
category: distribution
tags: [distribution, layout]
order: 1
---

# The Enso Distribution
This document provides a specification of how the Enso distribution should
be structured and how it should behave.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Enso Home Layout](#enso-home-layout)
- [Universal Launcher Script](#universal-launcher-script)
- [Layout of an Enso Version Package](#layout-of-an-enso-version-package)
  - [Standard Library](#standard-library)
  - [Resolvers](#resolvers)

<!-- /MarkdownTOC -->

## Enso Home Layout
> The actionables for this section are:
>
> - Finalize the decision if Enso is distributed as a portable directory
>   structure or if the launcher keeps the installations in system-defined
>   directories.

All files in the directory structure, except for configuration, can be safely
removed and the launcher will re-download them if needed.

The directory structure is as follows:

```
install-location
├── bin
│   └── enso                # The universal launcher script, responsible for choosing the appropriate compiler version (TODO [RW] it may be stored in a different place).
├── config                  # (TODO [RW] This configuration may be stored somewhere else.)
│   └── global-config.yaml  # Global user configuration.
├── dist                    # Per-compiler-version distribution directories.
│   ├── 1.0.0               # A full distribution of given Enso version, described below.
│   │   └── <truncated>
│   └── 1.2.0               # A full distribution of given Enso version, described below.
│       └── <truncated>
├── runtime                 # A directory storing (optional) distributions of the JVM used by the Enso distributions.
│   └── graalvm-ce-27.1.1
├── lib
│   └── src                 # Contains sources of downloaded libraries.
│       └── Dataframe       # Each library may be stored in multiple version.
│           └── 1.7.0       # Each version contains a standard Enso package.
│               ├── package.yaml
│               └── src
│                   ├── List.enso
│                   ├── Number.enso
│                   └── Text.enso
└── resolvers               # Contains resolver specifications, described below.
    ├── lts-1.56.7.yaml
    └── lts-2.0.8.yaml
```

## Universal Launcher Script
The [universal launcher script](./launcher.md) should be able to launch the
proper version of Enso executable based on the version specified in the project
being run, or use the default version if none specified. It should also be able
to launch other Enso components, provided as
[plugins](./launcher.md#running-plugins).

**Note**
This launcher is under development. Until it is in a ready-to-use state, the
Enso version packages provide simple launcher scripts in the `bin` directory of
that package. They will be removed when the universal launcher matures.

## Layout of an Enso Version Package
This section describes the structure of a single version distribution. This
system is intended to be implemented first and used e.g. for the Enso nightly
builds / releases.

Such a distribution can be used as a standalone version, assuming the required
version of the JVM is installed and used.

The layout of such a distribution is as follows:

```
enso-1.0.0
├── manifest.yaml    # A manifest file defining metadata about this Enso version.
├── component        # Contains all the executable tools and their dependencies.
│   ├── runner.jar   # The main executable of the distribution. CLI entry point.
│   └── runtime.jar  # The language runtime. It is loaded by other JVM components, like the runner.
└── std-lib          # Contains all the libraries that are pre-installed within that compiler version.
    ├── Http         # Every version sub-directory is just an Enso package containing the library.
    │     ├── package.yaml
    │     ├── polyglot
    │     └── src
    │         ├── Http.enso
    │         └── Socket.enso
    └── Base
          ├── package.yaml
          └── src
              ├── List.enso
              ├── Number.enso
              └── Text.enso
```

> **Implementation Note:**
> This structure makes use of deep nesting, which may give some with knowledge
> of Windows' path-name limits pause (windows paths are historically limited to
> 256 characters). However, there is no special action required to handle this
> limit as long as we are building on top of the JVM. The JVM automatically
> inserts the `\\?\` prefix required to bypass the windows path length limit.

### Standard Library
The standard library is a set of libraries shipped with the compiler.
Whether a given package belongs to standard library can be a bit of an
arbitrary choice, but the following are some guidelines:
1. Fundamental packages – basic collections and utilities should be a part
   of standard library.
2. Packages relying on the compiler internals (e.g. the internal object
   representation). An example of such a package would be `Generic`, exposing
   reflective access to Enso objects.
3. Packages that the compiler relies on, e.g. compile error definitions, stack
   traces etc.

### Resolvers
**Note** This system is not implemented yet.

A resolver is a manifest containing library versions that are automatically
available for import in any project using the resolver.

Example contents of a resolver file are as follows:

```yaml
enso-version: 1.0.7
libraries:
  - name: Base
    version: 1.0.0
  - name: Http
    version: 5.3.5
```
