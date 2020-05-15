---
layout: developer-doc
title: Evaluation Semantics in Enso
category: semantics
tags: [semantics, evaluation, suspension]
order: 5
---

# Evaluation Semantics in Enso
Enso's evaluation semantics can be succinctly described as 'strict, but with
optional laziness'. By default, expressions in Enso are evaluated strictly, but
the programmer may choose to 'suspend' computations, and instead evaluate them
at the point they are needed.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Strict Evaluation](#strict-evaluation)
- [Optional Suspension](#optional-suspension)

<!-- /MarkdownTOC -->

## Strict Evaluation
Though Enso shares many syntactic similarities with Haskell, the most famous
example of a lazily evaluated language, Enso is not lazy. Instead, Enso is a
language that is strict.

- Statements in Enso are evaluated as soon as they are bound to a name.
- This means that arguments to a function are always evaluated before the
  function is applied.
- Statements are _only_ evaluated when they contain fully-applied function
  applications. Otherwise they return curried functions.

> The actionables for this section are:
>
> - Make this far better specified.

## Optional Suspension
Laziness, however, can often be quite useful for defining certain kinds of API.
To that end, Enso provides support for optional laziness, more specifically
optional _suspension_, through the built-in `Suspended` type.

- When a type `a` is wrapped in a `Suspended`, it is turned into a thunk.
- A value of type `Suspended a` may be forced to execute the suspended
  computation and thereby obtain an `a`.

This forcing can take place in two ways:

- The user calls the standard library function `force : Suspended a -> a` on the
  value.
- Automatically, at a site where its evaluation is demanded. The algorithm for
  this is simple. If a value of type `Suspended a` is provided in a location
  that expects a value of type `a`, the compiler will insert an implicit call to
  `force` to produce the `a`.

> The actionables for this section are:
>
> - Make this far better specified.
