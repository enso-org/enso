---
layout: developer-doc
title: Diagnostics
category: semantics
tags: [semantics, diagnostics, runtime]
order: 2
---

# Diagnostics

Due to the highly interactive and always-online nature of a running Enso
program, it is very important that compile diagnostics (such as lints, errors,
and warnings) are surfaced in the _running_ language. This is particularly
important for visual mode.

To this end, Enso provides mechanisms by which diagnostics are scoped to the
smallest possible program component, and are even translated into executable
nodes in the interpreter so that they can be interacted with at runtime.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Warnings at Runtime](#warnings-at-runtime)
- [Errors at Runtime](#errors-at-runtime)

<!-- /MarkdownTOC -->

## Warnings at Runtime

> The actionables for this section are:
>
> - Specify how warnings behave at runtime.

## Errors at Runtime

> The actionables for this section are:
>
> - Specify how errors behave at runtime.
> - Talk about their limited scope and how they can be interacted with at
>   runtime.
