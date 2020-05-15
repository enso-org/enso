---
layout: developer-doc
title: Evaluation and Typing
category: types
tags: [types, evaluation]
order: 9
---

# Evaluation and Typing
Enso is a language that has strict semantics by default, but it can still be
very useful to be able to opt-in to suspended computations (thunks) for the
design of certain APIs.

To that end, Enso provides a mechanism for this through the type system. The
standard library defines a `Suspend a` type which, when used in explicit type
signatures, will cause the corresponding expression to be suspended.

- The explicit calls to `Suspend` and `force` are inserted automatically by the
  compiler doing demand analysis.
- This demand analysis process will also ensure that there are not polynomial
  chains of suspend and force being inserted to ensure performance.

> The actionables for this section are as follows:
>
> - Specify this much better.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Specifying Suspension in the Type System](#specifying-suspension-in-the-type-system)

<!-- /MarkdownTOC -->

## Specifying Suspension in the Type System

> The actionables for this section are:
>
> - Actually specify how the type system interacts with eager and lazy
>   evaluation.
