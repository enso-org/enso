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
it. Thus, we want to move to using Graal builds for Java 11.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Migration progress](#migration-progress)
  - [Build configuration](#build-configuration)
  - [Testing](#testing)
  - [Benchmarks](#benchmarks)
- [Problems](#problems)
  - [IllegalAccessError](#illegalaccesserror)

<!-- /MarkdownTOC -->

## Migration progress
The overall steps of the migration and their status are outlined in this
section. The task is tracked as issue
[#671](https://github.com/enso-org/enso/issues/671).

### Build configuration
Currently, the only modification was the removal of the option
`-XX:-UseJVMCIClassLoader` that is deprecated in Java 11. Some other
modifications may be necessary however, to fix the issues described in
[Problems](#problems).

### Testing
After making the build succeed, all runtime tests are passing.
This will have to be revisited after fixing a final build configuration.

### Benchmarks
Benchmarks have not yet been compared.

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
aware of these mechanisms and just directly loads the classfiles, resulting in
errors.

Some of these errors are expected and instead of failing, simply a warning is
printed. Others are not detected where zinc expects them, but they fail later,
crashing the compilation process.

It may be possible to detect and catch those latter by modifying zinc, but this
would likely negatively impact its ability to detect dependencies for
incremental compilation.

We are currently looking into what can be done to avoid these errors.
