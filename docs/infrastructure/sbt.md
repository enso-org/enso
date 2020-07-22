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
  - [Graal and Flatc Version Check](#graal-and-flatc-version-check)
  - [Benchmarks](#benchmarks)
  - [Build Information](#build-information)
  - [Instruments Generation](#instruments-generation)
  - [Flatbuffers Generation](#flatbuffers-generation)
  - [Ensuring JARs Were Loaded](#ensuring-jars-were-loaded)
  - [Debugging Command](#debugging-command)

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
has to be ran when setting-up the project (and after a version change of Graal).
It makes sure the JAR is downloaded and copied to our directory and terminates
the sbt process to ensure that the user restarts it. Without the full restart,
the JAR would not be seen by the JVM. So when setting up the project or after
changing the version of Graal, before launching the sbt shell, you should first
run `sbt bootstrap`, to make sure the environment is properly prepared.

The logic for copying the JAR is implemented in the `bootstrapJARs` task in
[`CopyTruffleJAR`](../../project/CopyTruffleJAR.scala).

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
pre-flight checks complete _before_ the actual compilation, we add them as a
dependency of `compileInputs` which runs _strictly before_ actual compilation.

To check some invariants _after_ compilation, we can replace the original
`Compile / compile` task with a custom one which does its post-compile checks
and returns the result of `(Compile / compile).value`. An example of such a
'patched' compile task is implemented in
[`FixInstrumentsGeneration`](../../project/FixInstrumentsGeneration.scala).

## Helper Tasks

There are additional tasks defined in the [`project`](../../project) directory.
They are used by [`build.sbt`](../../build.sbt) to provide some additional
functionality.

### Graal and Flatc Version Check

[`EnvironmentCheck`](../../project/EnvironmentCheck.scala) defines a helper
function that can be attached to the default `Global / onLoad` state transition
to run a version check when loading the sbt project. This helper function
compares the version of JVM running sbt with GraalVM version defined in
[`build.sbt`](../../build.sbt) and the version of `flatc` installed in the
system with the Flatbuffers library version defined in
[`build.sbt`](../../build.sbt). If the versions do not match it reports an error
telling the user to change to the correct version.

### Benchmarks

[`BenchTasks`](../../project/BenchTasks.scala) defines configuration keys for
benchmarking.

### Build Information

[`BenchTasks`](../../project/BuildInfo.scala) records version information
including what git commit has been used for compiling the project. This
information is used by `enso --version`.

### Instruments Generation

Truffle annotation processor generates a file that registers instruments
provided by the runtime. Unfortunately, with incremental compilation, only the
changed instruments are recompiled and the annotation processor does not detect
this, so un-changed instruments get un-registered.

To fix this, the pre-compile task defined in
[`FixInstrumentsGeneration`](../../project/FixInstrumentsGeneration.scala)
detects changes to instruments and if only one of them should be recompiled, it
forces recompilation of all of them, to ensure consistency.

For unclear reasons, if this task is attached to
`Compile / compile / compileInputs`, while it runs strictly _before_
compilation, the deleted class files are not always all recompiled. So instead,
it is attached directly to `Compile / compile`. This technically could allow for
a data race between this task and the actual compilation that happens in
`compileIncremental`, but in practice it seems to be a stable solution.

Sometimes it is unable to detect the need for recompilation before it takes
place. To help that, there is another task that replaces the default `compile`
task, which executes the default compilation task and after it, verifies the
consistency of instruments files. As it cannot restart compilation, to preserve
consistency it ensures the instruments will be recompiled the next time and
stops the current compilation task, asking the user to restart it.

### Flatbuffers Generation

[`GenerateFlatbuffers`](../../project/GenerateFlatbuffers.scala) defines the
task that runs the Flatbuffer compiler `flatc` whenever the flatbuffer
definitions have been changed. It also makes sure that `flatc` is available on
PATH and that its version matches the version of the library. It reports any
errors.

### Ensuring JARs Were Loaded

As described in [Bootstrapping](#bootstrapping), to successfully compile the
`runtime` subproject, the JVM running sbt must load some JARs at startup. The
user should run `sbt bootstrap` to ensure that.

If the compilation proceeds without the bootstrapped JARs it may lead to
inconsistent state with some dependencies being undetected and weird errors. To
avoid such situations, [`CopyTruffleJAR`](../../project/CopyTruffleJAR.scala)
defines is a pre-compile task that is executed before compiling the `runtime`
subproject which makes sure that the Truffle JAR is up-to-date. Even if the
developer forgets about `bootstrap`, this pre-compile task will update the
Truffle JARs. However, as the JARs are loaded at startup, the JVM has to be
restarted. To force the user to restart it, the current sbt process is
terminated.

This pre-compile task runs before compiling `runtime` sources, but some
dependencies may have started compiling in the meantime (as sbt schedules
independent tasks in parallel). The fact that these dependencies are up-to-date
may not be registered by sbt due to the abrupt termination. This does not affect
consistency of the build state though - as a result these dependencies may be
recompiled again when the compilation is restarted. This is acceptable as this
restart is just an additional check to ensure correctness and improve user
experience. In normal operation, the restart should never be triggered, as the
user should remember to run `bootstrap` when necessary.

### Debugging Command

[`WithDebugCommand`](../../project/WithDebugCommand.scala) defines a command
that allows to run a task with additional JVM-level flags.

### Recompile Parser

[`RecompileParser`](../../project/RecompileParser.scala) defines a task that can
be attached to the `compile` task in configurations of the `syntax` project.
This task ensures that the `syntax` project is recompiled whenever
`syntax-definition` changes.

## Native Image

[`NativeImage`](../../project/NativeImage.scala) defines a task that can compile
a project into a native binary using Graal's Native Image. It compiles the
project and runs the Native Image tool which builds the image. To be able to use
it, the Native Image component has to be installed within the used GraalVM
distribution. It can be installed by running `gu install native-image`.

The task is parametrized with `staticOnLinux` parameter which if set to `true`,
will statically link the built binary, to ensure portability between Linux
distributions. For Windows and MacOS, the binaries should generally be portable,
as described in [Launcher Portability](../distribution/launcher.md#portability).

As Native Image does not support cross-compilation, the native binaries can only
be built for the platform and architecture that the build is running on.
