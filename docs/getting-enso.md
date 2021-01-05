---
layout: developer-doc
title: Getting Enso
category: summary
tags: [contributing, installation]
order: 5
---

# Getting Enso

Enso packages are best obtained from the [releases](https://github.com/enso-org/enso/releases) page
of the repository. Each release has an "Assets" section at the bottom. You can 
click on this to view the list of artifacts from which you can download the most
appropriate version.

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

The distribution contains the Enso CLI. It allows to create and run Enso
projects from the command line. To launch the Enso CLI, run the `bin/enso`
script (Linux and MacOS) or the `bin/enso.bat` script (Windows).

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
