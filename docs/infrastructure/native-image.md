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
- [Configuration](#configuration)
  - [Launcher Configuration](#launcher-configuration)
  - [Project Manager Configuration](#project-manager-configuration)

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
and `x86_64-linux-musl-gcc` must be available in the system PATH for the
native-image. In the future it is possible that a different option will be used
or that the bundle will not be required anymore if it became prepackaged. This
task may thus need an update when moving to a newer version of Graal. More
information may be found in
[the Native Image documentation](https://github.com/oracle/graal/blob/master/substratevm/STATIC-IMAGES.md).

To make the bundle work correctly with GraalVM 20.2, a shell script called
`x86_64-linux-musl-gcc` which loads the bundle's configuration is created by the
task and the paths starting with `/build/bundle` in `musl-gcc.specs` are
replaced with absolute paths to the bundle location.

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
to fail with `java.lang.ClassNotFoundException` or the following error:

```
java.lang.InstantiationException: Type `XYZ` can not be instantiated reflectively as it does not have a no-parameter constructor or the no-parameter constructor has not been added explicitly to the native image.`
```

To avoid such issues, additional configuration has to be added to the Native
Image build so that it can include the missing constructors.

This can be done manually by creating a file `reflect-config.json`. The build
task looks for the configuration files in every subdirectory of
`META-INF/native-image` on the project classpath.

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
java -agentlib:native-image-agent=config-merge-dir=engine/launcher/src/main/resources/META-INF/native-image/org/enso/launcher -jar launcher.jar <arguments>
```

Note that for convenience, you can run the launcher/engine runner via
`bin/enso`, e.g.

```bash
env JAVA_OPTS="-agentlib:native-image-agent=config-merge-dir=./engine/runner-native/src/main/resources/META-INF/native-image/org/enso/runner" ./built-distribution/enso-engine-0.0.0-dev-linux-amd64/enso-0.0.0-dev/bin/enso --run tmp.enso
```

The command may need to be re-run with different arguments to ensure that all
execution paths that use reflection are covered. The configuration files between
consecutive runs will be merged (a warning may be issued for the first run if
the configuration files did not exist, this is not a problem).

It is possible that different classes are reflectively accessed on different
platforms. In that case it may be necessary to run the agent on multiple
platforms and merge the configs. If the conflicts were conflicting (i.e. some
reflectively accessed classes existed only on one platform), it may be necessary
to maintain separate configs for each platform. Currently, the
`native-image-agent` is not available on Windows, so Windows-specific reflective
accesses may have to be gathered manually. For some types of accesses it may be
possible to force the Windows-specific code paths to run on Linux and gather
these accesses semi-automatically.

After updating the Native Image configuration, make sure to clean it by running

```
cd tools/native-image-config-cleanup && npm install && npm start
```

### Launcher Configuration

In case of the launcher, to gather the relevant reflective accesses one wants to
test as many execution paths as possible, especially the ones that are likely to
use reflection. One of these areas is HTTP support and archive extraction.

To trace this accesses, it is good to run at least
`... launcher.jar install engine` which will trigger HTTP downloads and archive
extraction.

Currently, archive-related accesses are platform dependent - Linux launcher only
uses `.tar.gz` and Windows uses `.zip`. While the Linux launcher never unpacks
ZIP files, we can manually force it to do so, to register the reflection
configuration that will than be used on Windows to enable ZIP extraction.

To force the launcher to extract a ZIP on Linux, one can add the following code
snippet (with the necessary imports) to `org.enso.launcher.cli.Main.main`:

```
Archive.extractArchive(Path.of("enso-engine-windows.zip"), Path.of("somewhere"), None)
```

With this snippet, `launcher.jar` should be built using the
`launcher / assembly` task, and the tracing tool should be re-run as shown
above.

Moreover, some reflective accesses may not be detected by the tool
automatically, so they may need to be added manually. One of them is an access
to the class `[B` when using Akka, so it would require manually adding it to the
`reflect-config.json`. This strange looking access is most likely reflective
access to an array of bytes. To make it easier, a package `akka-native` has been
created that gathers workarounds required to be able to build native images
using Akka, so it is enough to just add it as a dependency. It does not handle
other reflective accesses that are related to Akka, because the ones that are
needed are gathered automatically using the tool described above.

### Project Manager Configuration

Configuring the Native Image for the Project Manager goes similarly as with the
launcher. You need to build the JAR with `project-manager/assembly` and execute
the test scenarios by starting it with:

```
java -agentlib:native-image-agent=config-merge-dir=lib/scala/project-manager/src/main/resources/META-INF/native-image/org/enso/projectmanager -jar project-manager.jar
```

To trace relevant reflection paths, the primary scenario is to start the Project
Manager and connect an IDE to it. Since the Project Manager is able to install
engine versions, similar steps should be taken to force it to extract a zip
archive, as described in [Launcher Configuration](#launcher-configuration)
above. If necessary, other scenarios, like project renaming may be covered.

Remember to run the cleanup script as described above, as tracing the Project
Manager seems to find recursive accesses of some ephemeral-like classes named
`Foo/0x00001234...`. This classes are not accessible when building the Native
Image and they lead to warnings. For now no clues have been found that ignoring
these classes would impact the native build, it seems that they can be ignored
safely.

### Engine runner Configuration

The Native Image generation for the Engine Runner is currently in a preview
state. To generate the Native Image for runner simply execute

```bash
sbt> engine-runner/buildNativeImage
```

and execute the binary on a sample factorial test program

```bash
> runner --run engine/runner-native/src/test/resources/Factorial.enso 6
```

The task that generates the Native Image, along with all the necessary
configuration, reside in a separate project due to a bug in the currently used
GraalVM version. As September 2023 it can execute all Enso code, but cannot
invoke `IO.println` or other library functions that require
[polyglot java import](../../docs/polyglot/java.md), but read on...

### Engine with Espresso

Since [PR-6966](https://github.com/enso-org/enso/pull/6966) there is an
experimental support for including
[Espresso Java interpreter](https://www.graalvm.org/jdk17/reference-manual/java-on-truffle/)
to allow use of some library functions (like `IO.println`) in the _Native Image_
built runner.

The support can be enabled by setting environment variable `ENSO_JAVA=espresso`
and making sure Espresso is installed in the Enso engine `component` directory:

```bash
enso$ built-distribution/enso-engine-*/enso-*/component/
```

e.g. next to `js-language-*.jar` and other JARs. Download following these two
JARs (tested for version 23.1.1) and copy them into the directory:

```bash
enso$ ls built-distribution/enso-engine-*/enso-*/component/espresso-*
built-distribution/enso-engine-0.0.0-dev-linux-amd64/enso-0.0.0-dev/component/espresso-language-23.1.1.jar
built-distribution/enso-engine-0.0.0-dev-linux-amd64/enso-0.0.0-dev/component/espresso-libs-resources-linux-amd64-23.1.1.jar
built-distribution/enso-engine-0.0.0-dev-linux-amd64/enso-0.0.0-dev/component/espresso-runtime-resources-linux-amd64-23.1.1.jar
```

the libraries can be found at
[Maven Central](https://repo1.maven.org/maven2/org/graalvm/espresso/). Version
`23.1.1` is known to work.

Alternatively just build the Enso code with `ENSO_JAVA=espresso` specified

```bash
enso$ ENSO_JAVA=espresso sbt --java-home /graalvm buildEngineDistribution
```

Then you can verify the support works:

```bash
$ cat >hello.enso
import Standard.Base.IO

main = IO.println <| "Hello World!"

$ ENSO_JAVA=espresso ./built-distribution/enso-engine-*/enso-*/bin/enso --run hello.enso
```

Unless you see a warning containing _"No language for id java found."_ your code
has just successfully been executed by
[Espresso](https://www.graalvm.org/jdk17/reference-manual/java-on-truffle/)! To
debug just add `JAVA_OPTS` environment variable set to your IDE favorite value:

```bash
$ JAVA_OPTS=-agentlib:jdwp=transport=dt_socket,address=5005 ENSO_JAVA=espresso enso --run hello.enso
```

Espresso support works also with
[native image support](#engine-runner-configuration). Just make sure
`ENSO_JAVA=espresso` is specified when building the `runner` executable:

```bash
enso$ rm runner
enso$ ENSO_JAVA=espresso sbt --java-home /graalvm
sbt> engine-runner/buildNativeImage
```

as suggested in the [native image support](#engine-runner-configuration). The
build script detects presence of Espresso and automatically adds
`--language:java` when creating the image. Then you can use

```bash
$ ENSO_JAVA=espresso ./runner --run hello.enso
```

to execute native image `runner` build of Enso together with Espresso.
