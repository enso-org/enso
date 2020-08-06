---
layout: developer-doc
title: JVM Object Generation
category: parser
tags: [parser, jvm, object-generation]
order: 10
---

# JVM Object Generation

The JVM object generation phase is responsible for creating JVM-native objects
representing the parser AST from the rust-native AST. This is required to allow
the compiler and runtime to work with the AST.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Overall Architecture](#overall-architecture)

<!-- /MarkdownTOC -->

# Overall Architecture

The JVM foreign function interface (FFI) named [Java Native Interface](
https://en.wikipedia.org/wiki/Java_Native_Interface) (JNI) enables the 
Rust parser library to call and be called by the parser library
implemented on top of Scala. Specifically, the Scala application uses
JNI to call functions exposed by Rust parser, while the Rust parser
itself uses the JNI to call Scala AST constructors while consuming
the input. 
