---
layout: developer-doc
title: Upgrading GraalVM
category: infrastructure
tags: [infrastructure, build, graalvm, graal, jvm]
order: 5
---

# Upgrading GraalVM

After upgrading the project to a newer version of GraalVM, all developers must
take the following actions to be able to continue development after the upgrade:

1. Download the new JVM version and set it as the default for the project. If
   you use IntelliJ, you will also need to update the JVM used for the project
   in the project settings.
2. Re-run `sbt bootstrap` to get the updated Truffle JAR (if there are issues
   updating, removing `engine/runtime/build-cache` directory may help).
3. Do a full clean (it may not _always_ be required, but not doing it often
   leads to problems so it is much safer to do it) by running `enso/clean`.
4. To be able to build or run tests for the `launcher` project, Native Image for
   the new GraalVM version has to be installed, as it is not included by
   default. This can be done with
   `<path-to-graal-home>/bin/gu install native-image`.
   - If there are problems building the Native Image, removing
     `engine/launcher/build-cache` (which contains the downloaded `musl`
     package) may help.
