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

<!-- /MarkdownTOC -->

> The actionables for this section are:
>
> - Work out how on earth this is going to work.
> - Produce a detailed design for this functionality.
