---
layout: developer-doc
title: Local Repository
category: distribution
tags: [distribution, project-manager, offline, local]
order: 7
---

# Local Repository

A `LocalReleaseProvider` is implemented that allows to install components from
local (offline) repositories. This functionality can be used to allow installing
bundled components.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Local Repository Structure](#local-repository-structure)
  - [General Repository Structure](#general-repository-structure)
  - [Engine Repository Structure](#engine-repository-structure)
  - [GraalVM Repository Structure](#graalvm-repository-structure)
- [Usage in Project Manager](#usage-in-project-manager)

<!-- /MarkdownTOC -->

## Local Repository Structure

This section explains how a local repository has to be structured in order to
work with `LocalReleaseProvider`.

### General Repository Structure

In general a local repository should be a separate directory that contains only
directories corresponding to releases of a single component. Repositories for
separate components should be kept separately. For each provided release it
should contain a directory called after the release's tag. That directory of
each release should just contain assets associated with that release.

### Engine Repository Structure

The engine repository contains a directory `enso-<VERSION>` for each release
that resembles the GitHub release structure. The directory for each release
should contain a `manifest.yaml` file and the package. As separate bundles are
created for each operating system, only the package for the desired operating
system is required (normally releases contain packages for all supported
systems). It's naming scheme is the same as in GitHub releases, that is
`enso-engine-<VERSION>-<OS>-<ARCH>.{zip|tar.gz}`. The extension is `zip` for
Windows and `tar.gz` for other platforms. Currently, the only supported `ARCH`
is `amd64`. The `OS` can be one of `windows`, `linux`, `macos`.

For example, a local engine repository could look like this:

```
localengine
└── enso-0.1.2-rc.9
    ├── enso-engine-0.1.2-rc.9-linux-amd64.tar.gz
    └── manifest.yaml
```

### GraalVM Repository Structure

The GraalVM repository contains a directory `vm-<VERSION>` for each release.
Inside of that directory, a package should be included (again as in case of the
engine, only the package for the current operating system is required). The
package name should be
`graalvm-ce-java<JAVA_VERSION>-<OS>-<ARCH>-<VERSION>.{zip|tar.gz}`. The
extension is `zip` on Windows and `tar.gz` on other platforms. The `ARCH` should
be the same as for the engine. The `OS` can be one of `windows`, `linux`,
`darwin`. **Note that the MacOS package has different naming scheme for GraalVM
than it has for the engine**.

For example, a local GraalVM repository can look like this:

```
localruntime
└── vm-20.2.0
    └── graalvm-ce-java11-linux-amd64-20.2.0.tar.gz
```

## Usage in Project Manager

Command line options can be used to enable the local repositories in the project
manager. `--local-engine-repository` sets the path to the engine repository and
`--local-graal-repository` sets the path to the GraalVM repository. The provided
paths should be absolute to be sure that they are resolved correctly.

For example, starting the project manager as written below will first look for
engines and GraalVM runtimes in the provided local repositories (but if they are
not found, online repository will be used as fallback, if it is available).

```bash
./project-manager --local-engine-repository /a/b/bundle/engines --local-graal-repository /a/b/bundle/graalvm
```

For the above command to work properly, the directory `/a/b/bundle/` may have
the following structure:

```
bundle
├── other files (project-manager binary etc.)
├── engines
│   └── enso-0.1.2-rc.9
│       ├── enso-engine-0.1.2-rc.9-linux-amd64.tar.gz
│       └── manifest.yaml
└── graalvm
    └── vm-20.2.0
        └── graalvm-ce-java11-linux-amd64-20.2.0.tar.gz
```
