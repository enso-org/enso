---
layout: developer-doc
title: Rust
category: infrastructure
tags: [infrastructure, build]
order: 1
---

# Rust

The Rust project is built using Cargo which manages dependencies between the
projects as well as external dependencies and allows for incremental
compilation. The build configuration is defined in
[`Cargo.toml`](../../Cargo.toml).

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Shared Libraries](#shared-libraries)

<!-- /MarkdownTOC -->

# Java Native Interface

Although the entry point of the Enso project is a Java archive, it can still
make use of native libraries built in Rust trough the JVM foreign function
interface (FFI) named
[Java Native Interface](https://en.wikipedia.org/wiki/Java_Native_Interface)
(JNI).

In order to generate a shared library, the `Cargo.toml` needs to enable
compilation into a dynamic system library:

```
crate-type = ["cdylib"]
```

Invoking `cargo build` will then output the library into `target/rust/debug`
with the extension `.so` on Linux, `.dll` on Windows and `.dylib` on macOS.

Then, to be able to load the library by `System.loadLibrary("lib_name")`, the
Java application should be started with the option
`-Djava.library.path=path/to/lib_folder`.
