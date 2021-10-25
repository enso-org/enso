---
layout: developer-doc
title: The Enso Distribution
category: distribution
tags: [distribution, layout]
order: 1
---

# The Enso Distribution

This document provides a specification of how the Enso distribution should be
structured and how it should behave.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Universal Launcher](#universal-launcher)
- [Enso Distribution Layout](#enso-distribution-layout)
  - [Portable Enso Distribution Layout](#portable-enso-distribution-layout)
  - [Installed Enso Distribution Layout](#installed-enso-distribution-layout)
  - [Installing from a Portable Distribution](#installing-from-a-portable-distribution)
- [Layout of an Enso Version Package](#layout-of-an-enso-version-package)
  - [Standard Library](#standard-library)
- [Enso Home Layout](#enso-home-layout)

<!-- /MarkdownTOC -->

## Universal Launcher

The [universal launcher](./launcher.md) should be able to launch the proper
version of Enso executable based on the version specified in the project being
run, or use the default version if none specified. It should also be able to
launch other Enso components, provided as
[plugins](./launcher.md#running-plugins).

> This launcher is under development. Until it is in a ready-to-use state, the
> Enso version packages provide simple launcher scripts in the `bin` directory
> of that package. They are a temporary replacement for the launcher
> functionality, so once the universal launcher matures, they will be removed.
> The universal launcher will not call the components through these scripts, as
> it must have full control over which JVM is chosen and its parameters.

## Enso Distribution Layout

Enso is distributed as a portable package that can be extracted anywhere on the
system and run. It can also be installed for the local user into system-defined
directories, as explained below.

### Portable Enso Distribution Layout

All files in the directory structure, except for the configuration, can be
safely removed, and the launcher will re-download them if needed.

The directory structure is as follows:

```
extraction-location
├── bin
│   └── enso                # The universal launcher, responsible for choosing the appropriate compiler version.
├── config
│   └── global-config.yaml  # Global user configuration.
├── dist                    # Per-compiler-version distribution directories.
│   ├── 1.0.0               # A full distribution of given Enso version, described below.
│   │   └── <truncated>
│   └── 1.2.0               # A full distribution of given Enso version, described below.
│       └── <truncated>
├── runtime                 # A directory storing distributions of the JVM used by the Enso distributions.
│   └── graalvm-ce-java11-27.1.1
├── lib                     # Contains sources of downloaded libraries.
│   └── Standard            # Each prefix (usually corresponding to the author) is placed in a separate directory.
│       └── Dataframe       # Each library may be stored in multiple versions.
│           └── 1.7.0       # Each version contains a standard Enso package.
│               ├── package.yaml
│               └── src
│                   ├── List.enso
│                   ├── Number.enso
│                   └── Text.enso
├── editions                # Contains Edition specifications.
│   ├── 2021.4.yaml
│   └── nightly-2021-06-31.yaml
├── README.md               # Information on layout and usage of the Enso distribution.
├── .enso.portable          # A file that allows the universal launcher to detect that if it is run from this directory, it should run in portable distribution mode.
└── THIRD-PARTY             # Contains licences of distributed components, including the NOTICE file.
```

### Installed Enso Distribution Layout

After installation, the directory structure is following:

```
ENSO_DATA_DIRECTORY
├── dist                    # Per-compiler-version distribution directories.
│   ├── 1.0.0               # A full distribution of given Enso version, described below.
│   │   └── <truncated>
│   └── 1.2.0               # A full distribution of given Enso version, described below.
│       └── <truncated>
├── runtime                 # A directory storing (optional) distributions of the JVM used by the Enso distributions.
│   └── graalvm-ce-java11-27.1.1
├── lib                     # Contains sources of downloaded libraries.
│   └── Standard            # Each prefix (usually corresponding to the author) is placed in a separate directory.
│       └── Dataframe       # Each library may be stored in multiple versions.
│           └── 1.7.0       # Each version contains a standard Enso package.
│               ├── package.yaml
│               └── src
│                   ├── List.enso
│                   ├── Number.enso
│                   └── Text.enso
└── editions                # Contains Edition specifications.
    ├── 2021.4.yaml
    └── nightly-2021-06-31.yaml

ENSO_CONFIG_DIRECTORY
└── global-config.yaml      # Global user configuration.

ENSO_BIN_DIRECTORY
└── enso                    # The universal launcher, responsible for choosing the appropriate compiler version.
```

Where `ENSO_DATA_DIRECTORY`, `ENSO_CONFIG_DIRECTORY` and `ENSO_BIN_DIRECTORY`
are environment variables that define the directory structure. They can be used
to override placement of the components listed above. However, most of the time
they do not have to be set, as they use system-specific defaults.

If not set, each of these three environment variables defaults to the following
value, depending on the system:

|                          | Linux                                                               | macOS                                         | Windows                      |
| ------------------------ | ------------------------------------------------------------------- | --------------------------------------------- | ---------------------------- |
| `ENSO_DATA_DIRECTORY`    | `$XDG_DATA_HOME/enso/` which defaults to `$HOME/.local/share/enso`  | `$HOME/Library/Application Support/org.enso/` | `%LocalAppData%/enso`        |
| `ENSO_CONFIG_DIRECTORY`  | `$XDG_CONFIG_HOME/enso/` which defaults to `$HOME/.config/enso`     | `$HOME/Library/Preferences/org.enso/`         | `%LocalAppData%/enso/config` |
| `ENSO_BIN_DIRECTORY`     | `$XDG_BIN_HOME` which defaults to `$HOME/.local/bin`                | `$HOME/.local/bin`                            | `%LocalAppData%/enso/bin`    |
| `ENSO_RUNTIME_DIRECTORY` | `$XDG_RUNTIME_DIR/enso/` or if its missing, `ENSO_DATA_DIRECTORY`   | `ENSO_DATA_DIRECTORY`                         | `%LocalAppData%/enso`        |
| `ENSO_LOG_DIRECTORY`     | `$XDG_CACHE_HOME/enso` or if its missing, `ENSO_DATA_DIRECTORY/log` | `$HOME/Library/Logs/org.enso/`                | `%LocalAppData%/enso/log`    |

### Installing from a Portable Distribution

After downloading and extracting the portable distribution, the user can run
`extraction-location/bin/enso install distribution` to install it locally. This
will copy the files from the portable distribution into the installed locations
which are described above and then remove the original files.

On Linux and macOS, if `ENSO_BIN_DIRECTORY` (`~/.local/bin` by default) is not
on system `PATH`, the installer will issue a warning, telling the user how they
can add it. On Windows, the installer automatically adds `ENSO_BIN_DIRECTORY` to
the user's `PATH`.

The installed distribution can be removed by running
`enso uninstall distribution`.

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
├── native-libraries # Contains all shared libraries that are used by JVM components.
│   └── parser.so    # The language parser. It is loaded by the runtime component.
│                    # Alternative extensions are .dll Windows and .dylib on Mac.
└── lib          # Contains all the libraries that are pre-installed within that compiler version.
    └── Standard
        ├── Http
        │   └── 0.1.0     # Every version sub-directory is just an Enso package containing the library.
        │       ├── package.yaml
        │       ├── polyglot
        │       └── src
        │           ├── Http.enso
        │           └── Socket.enso
        └── Base
            └── 0.1.0
                ├── package.yaml
                └── src
                    ├── List.enso
                    ├── Number.enso
                    └── Text.enso
```

> **Implementation Note:** This structure makes use of deep nesting, which may
> give some with knowledge of Windows' path-name limits pause (windows paths are
> historically limited to 256 characters). However, there is no special action
> required to handle this limit as long as we are building on top of the JVM.
> The JVM automatically inserts the `\\?\` prefix required to bypass the windows
> path length limit.

### Standard Library

The standard library is a set of libraries shipped with the compiler. Whether a
given package belongs to standard library can be a bit of an arbitrary choice,
but the following are some guidelines:

1. Fundamental packages – basic collections and utilities should be a part of
   standard library.
2. Packages relying on the compiler internals (e.g. the internal object
   representation). An example of such a package would be `Generic`, exposing
   reflective access to Enso objects.
3. Packages that the compiler relies on, e.g. compile error definitions, stack
   traces etc.

## Enso Home Layout

The location called in some places `<ENSO_HOME>` is the place where user's
projects and similar files are stored. Currently it is specified to always be
`$HOME/enso`.

It has the following structure:

```
<ENSO_HOME>
├── projects             # Contains all user projects.
├── libraries            # Contains all local libraries that can be edited by the user.
│   └── Prefix1          # Contains libraries with the given prefix.
│       └── Library_Name # Contains a package of a local library.
└── editions             # Contains custom, user-defined editions that can be used as a base for project configurations.
```
