---
layout: section-summary
title: Infrastructure
category: infrastructure
tags: [infrastructure, readme]
order: 0
---

# Infrastructure

The Enso runtime runs on the GraalVM which is a version of the JVM. This folder
contains all documentation pertaining to Enso's infrastructure, which is broken
up as follows:

- [**sbt:**](sbt.md) The build tools that are used for building the project.
- [**Java 11:**](java-11.md) Description of changes related to the Java 11
  migration.
- [**Native Image:**](native-image.md) Description of the Native Image build
  used for building the launcher native binary.
- [**Rust:**](rust.md) Description of integration of the Scala project with the
  Rust components.
- [**Upgrading GraalVM:**](upgrading-graalvm.md) Description of steps that have
  to be performed by each developer when the project is upgraded to a new
  version of GraalVM.
- [**Logging**:](logging.md) Description of an unified and centralized logging
  infrastructure that should be used by all components.
