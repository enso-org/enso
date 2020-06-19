---
layout: developer-doc
title: Build Tools
category: infrastructure
tags: [infrastructure, build]
order: 1
---

# Build Tools
The project is built using the Scala Build Tool which manages dependencies
between the projects as well as external dependencies and allows for incremental
compilation. The build configuration is defined in
[`build.sbt`](../../build.sbt).

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Incremental compilation](#incremental-compilation)
- [Helper tasks](#helper-tasks)
  - [Benchmarks](#benchmarks)
  - [Build information](#build-information)
  - [Instruments generation](#instruments-generation)
  - [Flatbuffers generation](#flatbuffers-generation)
  - [Debugging command](#debugging-command)

<!-- /MarkdownTOC -->

## Incremental compilation
To help wit build times, we do not want to rebuild the whole project with every
change, but to only recompile the files that have been affected by the change.
This is handled by sbt which under the hood uses
[zinc](https://github.com/sbt/zinc) (the incremental compiler for Scala). zinc
analyses the compiled files and detects dependencies between them to determine
which files have to be recompiled when something has been changed.

## Helper tasks
There are additional tasks defined in the [`project`](../../project) directory.
They are used by [`build.sbt`](../../build.sbt) to provide some additional
functionality.

### Benchmarks
[`BenchTasks`](../../project/BenchTasks.scala) defines configuration keys for
benchmarking.

### Build information
[`BenchTasks`](../../project/BuildInfo.scala) records version information
including what git commit has been used for compiling the project. This
information is used by `enso --version`.

### Instruments generation
Truffle annotation processor generates a file that registers instruments
provided by the runtime. Unfortunately, with incremental compilation, only the
changed instruments are recompiled and the annotation processor does not detect
this, so un-changed instruments get un-registered.

To fix this, the task defined in
[`FixInstrumentsGeneration`](../../project/FixInstrumentsGeneration.scala)
detects changes to instruments and if only one of them should be recompiled, it
forces recompilation of all of them, to ensure consistency.

Sometimes it is unable to detect the need for recompilation before it takes
place. As it also cannot restart compilation, to preserve consistency it stops
the compilation and asks the user to restart it, to allow it to force
recompilation of instruments.

### Flatbuffers generation
[`GenerateFlatbuffers`](../../project/GenerateFlatbuffers.scala) defines the
task that runs the Flatbuffer compiler `flatc` whenever the flatbuffer
definitions have been changed. It also makes sure that `flatc` is available on
PATH and that its version matches the version of the library. It reports any
errors.

### Debugging command
[`WithDebugCommand`](../../project/WithDebugCommand.scala) defines a command
that allows to run a task with additional JVM-level flags.