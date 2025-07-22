---
layout: developer-doc
title: Errors
category: semantics
tags: [semantics, errors, runtime]
order: 4
---

# Errors

Due to its hybrid nature, Enso supports two main exception systems that help
users to deal with errors at runtime in the language.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Asynchronous Exceptions](#asynchronous-exceptions)
- [Broken Values](#broken-values)

<!-- /MarkdownTOC -->

## Asynchronous Exceptions

> [!WARNING] The actionables for this section are:
>
> - Why do we call it asynchronous, when they are synchronous!?
> - Specify the semantics of Enso's async exceptions.

## Broken Values

There is a special [dynamic dispatch](../types/dynamic-dispatch.md) for `Error`
values. A dataflow error dispatch first checks if it may call a method on
`Error` type.

> [!WARNING] The actionables for this section are:
>
> - Specify the semantics of Enso's broken values.

## Warnings

> [!WARNING] TODO
>
> - Values in Enso may have warnings attached

There is a special [dynamic dispatch](../types/dynamic-dispatch.md) for _values
with warnings_. Before we pass the dispatch to the underlying value, we check if
warning has a method 'overridden' - this is used for `remove_warnings`, etc.
