---
layout: developer-doc
title: Getting Enso
category: summary
tags: [contributing, installation]
order: 5
---

# Getting Enso

Enso packages can currently be obtained from the per-commit CI builds. See
[the build workflow on GitHub Actions](https://github.com/enso-org/enso/actions?query=workflow%3A%22Engine+CI%22+branch%3Amain),
which should show a list of recent CI builds. The workflow of interest is
`Engine CI`. You can navigate to the most recent build, which will display a
list of attached artifacts. The artifact of interest is `enso-engine-<version>`
(currently `enso-engine-0.1.0`).

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Dependencies](#dependencies)
- [Running Enso](#running-enso)
- [Troubleshooting](#troubleshooting)

<!-- /MarkdownTOC -->

## Dependencies

The Enso distribution requires to be run with the appropriate version of
GraalVM. You can get the Community Edition pre-built distributions from
[the GitHub releases site](https://github.com/graalvm/graalvm-ce-builds/releases).
It is important to run Enso with exactly the version specified here. Given that
Graal is still a relatively young project, even the minor version changes
introduce breaking API changes. The current version of GraalVM required for Enso
is `20.2.0`, and it must be the Java 11 build.

Before running the Enso packages, make sure that the `JAVA_HOME` environment
variable points to the correct home location of the Graal distribution.

## Running Enso

The distribution contains two main executables of interest:

1. The project manager. This executable is currently used for testing the IDE,
   though in the future it will rarely be run directly and rather will be
   launched automatically by the IDE. To run the project manager, run the
   `bin/project-manager` script (Linux and MacOS) or the
   `bin/project-manager.bat` script (Windows).
2. The Enso CLI. This allows to create and run Enso projects from the command
   line. To launch the Enso CLI, run the `bin/enso` script (Linux and MacOS) or
   the `bin/enso.bat` script (Windows).

Again, it is necessary for you to set the `JAVA_HOME` variable correctly.

## Troubleshooting

This section lists the most common failures and their probable causes.

1. The exception
   `java.lang.IllegalArgumentException: Could not find option with name enso-runtime-server.enable.`
   It can contain a different option name. This exception signals problems with
   the Graal distribution. Make sure you're running Enso with the correct
   version of GraalVM. You can verify the version of JDK used by running
   `bin/enso --version`. Take note of the version displayed in the `Running on`
   section. It should be similar to:

   ```
   Running on: OpenJDK 64-Bit Server VM, GraalVM Community, JDK 11.0.8+10-jvmci-20.2-b03
               Linux 4.15.0-112-generic (amd64)
   ```

   It could also be caused by not using the launcher scripts and trying to run
   the component `.jar` files via `java -jar` without setting the necessary
   options. Use the launcher scripts.
