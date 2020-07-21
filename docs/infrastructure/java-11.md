---
layout: developer-doc
title: Java 11
category: infrastructure
tags: [infrastructure, build, java11]
order: 2
---

# Migrating to Java 11

JDK 11 will be supported longer than JDK 8 that we currently use and it adds new
features that could improve performance. Moreover, we want to be compliant to
the Java Platform Module System, as all future versions of the JDK will rely on
it. Thus, we have moved to using Graal builds for Java 11.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Migration Progress](#migration-progress)
  - [Build Configuration](#build-configuration)
  - [Testing](#testing)
  - [Benchmarks](#benchmarks)
- [Problems](#problems)
  - [IllegalAccessError](#illegalaccesserror)

<!-- /MarkdownTOC -->

## Migration Progress

The overall steps of the migration and their status are outlined in this
section.

### Build Configuration

The option `-XX:-UseJVMCIClassLoader` is deprecated in Java 11 and has been
removed from the test configuration.

The JVM running sbt must have `--upgrade-module-path=lib/truffle-api.jar` added
as an option and the build tool must ensure that the `truffle-api.jar` is copied
from the Maven repository to the `lib/` directory before the `runtime` project
is compiled. Section [IllegalAccessError](#illegalaccesserror) explains why this
is necessary and [Bootstrapping](./sbt.md#bootstrapping) explains the tasks that
help with this process.

### Testing

All tests are passing.

To make sure `runtime` tests can be run both with `test` and `testOnly`,
`--upgrade-module-path=<path-to-truffle-api.jar>` has to be added to
`javaOptions`. It is important to note that the tests may be ran in a working
directory different than the project root, so an absolute path should be used.

### Benchmarks

Initially there were some regressions found in the benchmarks, but further
investigation revealed this was caused by some issues in the methodology of how
the JMH benchmarks were implemented. There are plans to rewrite these
benchmarks.

Benchmarks in pure Enso are currently more meaningful. They yield comparable
results with Java 11 being slightly faster.

## Problems

The problems that were encountered when doing the migration.

### IllegalAccessError

As described in [Build tools](sbt.md#incremental-compilation), to allow for
incremental compilation, zinc has to analyse dependencies between various files.
As our project uses both Scala and Java code, dependencies between files in both
languages have to be detected.

To achieve that, after compiling Java classes with `javac`, zinc loads the
resulting classes and analyses their APIs to find what other classes they depend
on. Based on this information it can figure out what files need to be recompiled
on any changes.

The sbt process running zinc code tries to load our classes, but it fails for
some of them with errors like:

```
java.lang.IllegalAccessError: superinterface check failed: class com.oracle.truffle.sl.runtime.SLFunction (in unnamed module @0x7590b48c) cannot access class com.oracle.truffle.api.interop.TruffleObject (in module org.graalvm.truffle) because module org.graalvm.truffle does not export com.oracle.truffle.api.interop to unnamed module @0x7590b48c
```

The Truffle API does not export its packages for security reasons and it uses
some custom mechanisms when loading the language runtime. However zinc is not
aware of these mechanisms and just directly loads the `.class` files, resulting
in errors.

Some of these errors are caught and instead of failing, simply a warning is
printed. Others are not detected where zinc expects them, but they fail later,
crashing the compilation process. All of them are problematic though, because
they mean that zinc is not able to read dependencies of the affected files. This
harms incremental compilation (as some dependencies are not detected it might be
necessary to do a clean and full recompilation when changing these files).

#### Solution

We want to make the ClassLoader read these class files without errors. For that
we need to ensure it has permissions to load the Truffle modules.

Truffle API is distributed in two ways. The distribution included in the Graal
runtime (the one that is picked up by sbt by default) exports the required APIs
only to its own modules, so they are not available for us (thus the errors).
This is to ensure better security (to disallow language users introspecting the
VM internals). However, there is a second Truffle distribution on Maven that is
to be used for development only and that version exports the necessary APIs to
all packages.

We need to ensure that the sbt process doing the compilation uses the Maven
version of Truffle, so that it does not complain about the illegal accesses. To
achieve that, we need to add the option
`--upgrade-module-path=lib/truffle-api.jar` to the JVM running sbt and ensure
that the Truffle JAR from maven is copied to the `lib/` directory.
