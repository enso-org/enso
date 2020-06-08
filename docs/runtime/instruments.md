---
layout: developer-doc
title: Instruments
category: runtime
tags: [runtime, instruments]
order: 6
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
`Instrument`. This requirement is to ensure that the [fix](#fixing-compilation)
described below works.

## Fixing Compilation
Annotations are used to register the implemented instruments with Graal. The
annotation processor is triggered when recompiling the Java files.
Unfortunately, when doing an incremental compilation, only the changed files are
recompiled and the annotation processor 'forgets' about other instruments that
haven't been recompiled, leading to runtime errors about missing instruments.

To fix that, we add the
[`FixInstrumentsGeneration.scala`](../../project/FixInstrumentsGeneration.scala)
task which detects changes to any of the instruments and forces recompilation of
all instruments in the project by removing their classfiles.