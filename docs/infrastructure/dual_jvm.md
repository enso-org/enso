---
layout: developer-doc
title: Dual JVM mode
category: infrastructure
tags: [infrastructure, build, native, native-image, dual-jvm]
order: 4
---

# Dual JVM Mode

_Dual JVM mode_ allows Enso to run both native-compiled code and JVM bytecode in
the same process. This enables fast startup and efficient execution of libraries
that require the JVM.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Terminology](#terminology)
- [How It Works](#how-it-works)
- [Motivation](#motivation)
- [When Is Dual JVM Mode Used?](#when-is-dual-jvm-mode-used)
- [References](#references)
- [Related Work](#related-work)

<!-- /MarkdownTOC -->

## Terminology

- **Native Image (NI):** A compiled native executable.
- **Substrate VM (SVM):** The runtime for NI, an alternative to the standard
  HotSpot VM.
- **HotSpot VM:** The standard Java Virtual Machine.

## How It Works

- The core of Enso is compiled to a native executable (NI).
- Some standard, non-essential, libraries are only compiled to JVM bytecode.
- NI cannot run JVM bytecode directly.
  - SVM is not JVM bytecode interpreter.
- When JVM bytecode is needed, SVM loads the HotSpot VM as a shared library.
- Both VMs run in the same process and communicate efficiently.

## Motivation

- Native compilation can make executables very large and slow to build.
- Maintaining NI configuration is complex, especially for third-party libraries.
- Fast startup is important, so NI is the entry point.
- Some components will only be compiled to the JVM bytecode.
- Executing JVM bytecode in a separate process is not efficient.

## When Is Dual JVM Mode Used?

- When the `--jvm` option is passed to the engine runner, NI immediately
  delegates execution to the HotSpot VM.
  - This is called **JVM mode**.
- When a library that is not NI-ready (contains JAR files) is loaded.
- A library can require JVM mode by setting `jvm: true` in its descriptor.

## References

- [GraalVM Isolates](https://medium.com/graalvm/isolates-and-compressed-references-more-flexible-and-efficient-memory-management-for-graalvm-a044cc50b67e)
- [Launching Enso programs instantly](https://github.com/orgs/enso-org/discussions/10121)
- [Make native enso launcher smaller](https://github.com/orgs/enso-org/discussions/12446)

## Related Work

- [Emit a warning on non-AOT ready libraries](https://github.com/enso-org/enso/pull/12468)
- [Handle --jvm flag in process with JNI instantiated JVM](https://github.com/enso-org/enso/pull/12843)
- [Callback to native executable from HotSpot JVM](https://github.com/enso-org/enso/pull/13076)
- [Channel connecting two JVM instances](https://github.com/enso-org/enso/pull/13206)
- [Using Channel to load Java classes](https://github.com/enso-org/enso/pull/13238)
- [Moving polyglot java import handling into EpbLanguage](https://github.com/enso-org/enso/pull/13483)
- [Use Dual JVM mode to run StdLib Native tests](https://github.com/enso-org/enso/pull/13570)
- [Benchmark and speed dual JVM mode up](https://github.com/enso-org/enso/pull/13780)
- [Distributed GC between the dual JVMs](https://github.com/enso-org/enso/pull/13855)
- [No need for jvm:true when running Generic_JDBC_Tests](https://github.com/enso-org/enso/pull/13900)
- [Exchanging direct ByteBuffers between the dual JVMs](https://github.com/enso-org/enso/pull/13904)
