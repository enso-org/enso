---
layout: developer-doc
title: Native Image
category: infrastructure
tags: [infrastructure, build, native, native-image]
order: 3
---

# Native Image

[`NativeImage`](../../project/NativeImage.scala) defines a task that is used for
compiling a project into a native binary using Graal's Native Image. It compiles
the project and runs the Native Image tool which builds the image. Currently,
Native Image is used for building the Launcher.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Requirements](#requirements)
  - [Native Image Component](#native-image-component)
  - [Additional Linux Dependencies](#additional-linux-dependencies)
- [Static Builds](#static-builds)
- [No Cross-Compilation](#no-cross-compilation)

<!-- /MarkdownTOC -->

## Requirements

### Native Image Component

The Native Image component has to be installed within the used GraalVM
distribution. It can be installed by running
`<path-to-graal-home>/bin/gu install native-image`.

### Additional Linux Dependencies

To be able to [link statically](#static-builds) on Linux, we need to link
against a `libc` implementation. The default `glibc` contains
[a bug](https://sourceware.org/bugzilla/show_bug.cgi?id=10652) that would cause
crashes when downloading files form the internet, which is a crucial Launcher
functionality. Instead, [`musl`](https://musl.libc.org/) implementation is
suggested by Graal as an alternative. The sbt task automatically downloads a
bundle containing all requirements for a static build with `musl`. It only
requires a `tar` command to be available to extract the bundle.

Currently, to use `musl`, the `--libc=musl` option has to be added to the build
and `gcc-musl` must be available in the system PATH for the native-image. In the
future it is possible that a different option will be used or that the bundle
will not be required anymore if it became prepackaged. This task may thus need
an update when moving to a newer version of Graal. More information may be found
in
[the Native Image documentation](https://github.com/oracle/graal/blob/master/substratevm/STATIC-IMAGES.md).

To make the bundle work correctly with GraalVM 20.2, a shell script called
`gcc-musl` which loads the bundle's configuration is created by the task and the
paths starting with `/build/bundle` in `musl-gcc.specs` are replaced with
absolute paths to the bundle location.

## Static Builds

The task is parametrized with `staticOnLinux` parameter which if set to `true`,
will statically link the built binary, to ensure portability between Linux
distributions. For Windows and MacOS, the binaries should generally be portable,
as described in [Launcher Portability](../distribution/launcher.md#portability).

## No Cross-Compilation

As Native Image does not support cross-compilation, the native binaries can only
be built for the platform and architecture that the build is running on.

## Configuration

As the Native Image builds a native binary, certain capabilities, like
[reflection](https://github.com/oracle/graal/blob/master/substratevm/REFLECTION.md),
may be limited. The build system tries to automatically detect some reflective
accesses, but it cannot detect everything. It is possible for the built binary
to fail with the following error:

```
java.lang.InstantiationException: Type `XYZ` can not be instantiated reflectively as it does not have a no-parameter constructor or the no-parameter constructor has not been added explicitly to the native image.`
```

To avoid such issues, additional configuration has to be added to the Native
Image build so that it can include the missing constructors.

This can be done manually by creating a file `reflect-config.json`. The build
task looks for the configuration files in a directory called
`native-image-config` inside the root of the compiled sub-project.

Creating the configuration manually may be tedious and error-prone, so GraalVM
includes
[a tool for assisted configuration](https://github.com/oracle/graal/blob/master/substratevm/CONFIGURE.md).
The link describes in detail how the tool can be used. The gist is, the JVM
version of the application can be run with a special agentlib in order to trace
reflective accesses and save the generated configuration to the provided
directory. To run the tool it is easiest to assemble the application into a JAR
and run it with the following command:

```bash
java \
  -agentlib:native-image-agent=config-merge-dir=/path/to/native-image-config \
  -jar /path/to/application.jar \
  <application arguments>
```

For example, to update settings for the Launcher:

```bash
java -agentlib:native-image-agent=config-merge-dir=engine/launcher/native-image-config -jar launcher.jar <arguments>
```

The command may need to be re-run with different arguments to ensure that all
execution paths that use reflection are covered. The configuration files between
consecutive runs will be merged (a warning may be issued for the first run if
the configuration files did not exist, this is not a problem).

It is possible that different classes are reflectively accessed on different
platforms. In that case it may be necessary to run the agent on multiple
platforms and merge the configs. If the conflicts were conflicting (i.e. some
reflectively accessed classes existed only on one platform), it may be necessary
to maintain separate configs for each platform. Currently in the Launcher this
is not the case - the reflective accesses seem to be platform independent, as
the launcher built with a config created on Linux runs successfully on other
platforms.
