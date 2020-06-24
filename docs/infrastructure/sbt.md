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

- [Incremental Compilation](#incremental-compilation)
- [Bootstrapping](#bootstrapping)
- [Compile Hooks](#compile-hooks)
- [Helper Tasks](#helper-tasks)
  - [Benchmarks](#benchmarks)
  - [Build information](#build-information)
  - [Instruments generation](#instruments-generation)
  - [Flatbuffers generation](#flatbuffers-generation)
  - [Debugging command](#debugging-command)

<!-- /MarkdownTOC -->

## Incremental Compilation
To help wit build times, we do not want to rebuild the whole project with every
change, but to only recompile the files that have been affected by the change.
This is handled by sbt which under the hood uses
[zinc](https://github.com/sbt/zinc) (the incremental compiler for Scala). zinc
analyses the compiled files and detects dependencies between them to determine
which files have to be recompiled when something has been changed.

## Bootstrapping
As described in [Java 11 Migration](./java-11.md#illegalaccesserror) to
successfully compile the `runtime` project, the JVM running sbt must use the
overridden JAR for Truffle API. This JAR has to be present during startup of the
JVM, but it has to be downloaded from the Maven repository.

To fix this chicken-and-egg problem, we have a special `bootstrap` task, that
has to be ran when setting-up the project (and after a version update of Graal).
It makes sure the JAR is downloaded and copied to our directory and terminates
the sbt process to ensure that the user restarts it. Without the full restart,
the JAR would not be seen by the JVM. So when setting up the project or after
changing the version of Graal, the before launching the sbt shell, you should
first run `sbt bootstrap`, to make sure the environment is correctly prepared.

The logic for copying the JAR is implemented in
[`CopyTruffleJAR`](../../project/CopyTruffleJAR.scala). If the compilation
proceeds without the bootstrapped JAR it may lead to inconsistent state with
some dependencies being undetected. So there is a pre-compile task that is
executed before compiling the `runtime` project which makes sure that the
Truffle JAR is up-to-date. This is to ensure that the developer did not forget
about bootstrapping. This pre-compile task will also update the Truffle JAR and
terminate sbt, to make sure it is restarted for the changes to be applied.

## Compile Hooks
There are some invariants that are specific to our project, so they are not
tracked by sbt, but we want to ensure that they hold to avoid cryptic errors at
compilation or runtime.

To check some state before compilation, we add our tasks as dependencies of
`Compile / compile / compileInputs` by adding the following to the settings of a
particular project.

```
Compile / compile / compileInputs := (Compile / compile / compileInputs)
        .dependsOn(preCompileHookTask)
        .value
```

Tasks that should be run before compilation, should be attached to the
`compileInputs` task. That is because the actual compilation process is ran in
the task `compileIncremental`. `Compile / compile` depends on
`compileIncremental` but if we add our dependency to `Compile / compile`, it is
considered as independent with `compileIncremental`, so sbt may schedule it to
run in parallel with the actual compilation process. To guarantee that our
pre-flight checks complete *before* the actual compilation, we add them as a
dependency of `compileInputs` which runs *strictly before* actual compilation.

To check some invariants *after* compilation, we can replace the original
`Compile / compile` task with a custom one which does its post-compile checks
and returns the result of `(Compile / compile).value`. An example of such a
'patched' compile task is implemented in
[`FixInstrumentsGeneration`](../../project/FixInstrumentsGeneration.scala).

## Helper Tasks
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