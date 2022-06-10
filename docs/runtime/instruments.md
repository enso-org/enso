---
layout: developer-doc
title: Instruments
category: runtime
tags: [runtime, instruments]
order: 9
---

# Instruments

Instruments are used to track runtime events to allow for profiling, debugging
and other kinds of behavior analysis at runtime.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Naming Conventions](#naming-conventions)
- [Fixing Compilation](#fixing-compilation)

<!-- /MarkdownTOC -->

## Naming Conventions

Every Instrument must be implemented in Java and have name that ends with
`Instrument`.

## Fixing Compilation

Annotations are used to register the implemented instruments with Graal. The
annotation processor is triggered when recompiling the Java files.
Unfortunately, when doing an incremental compilation, only the changed files are
recompiled and the annotation processor 'forgets' about other instruments that
haven't been recompiled, leading to runtime errors about missing instruments.

To fix that, individual services have to be placed in separate subprojects
depending on `runtime` and aggregated under `runtime-with-instruments`. Later
the META-INF registrations are concatenated in the final uber jar.
