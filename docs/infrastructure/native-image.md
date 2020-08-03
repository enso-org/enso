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
distribution. It can be installed by running `gu install native-image`.

### Additional Linux Dependencies

To be able to [link statically](#static-builds) on Linux, we need to link
against a `libc` implementation. The default `glibc` contains
[a bug](https://sourceware.org/bugzilla/show_bug.cgi?id=10652) that would cause
crashes when downloading files form the internet, which is a crucial Launcher
functionality. Instead, [`musl`](https://musl.libc.org/) implementation is
suggested by Graal as an alternative. The sbt task automatically downloads a
bundle containing all requirements for a static build with `musl`. It only
requires a `tar` command to be available to extract the bundle.

## Static Builds

The task is parametrized with `staticOnLinux` parameter which if set to `true`,
will statically link the built binary, to ensure portability between Linux
distributions. For Windows and MacOS, the binaries should generally be portable,
as described in [Launcher Portability](../distribution/launcher.md#portability).

## No Cross-Compilation

As Native Image does not support cross-compilation, the native binaries can only
be built for the platform and architecture that the build is running on.

## Configuration

// TODO [RW] explain reflection config

```bash
java \
  -agentlib:native-image-agent=config-merge-dir=/path/to/native-image-config \
  -jar launcher.jar \
  <program arguments>
```