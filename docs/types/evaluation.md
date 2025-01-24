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

To that end, Enso provides a mechanism for this through the type system.

> [!WARNING] Enso is using `~` and `...` to defer computations and perform them
> lazily when needed.
>
> The standard library defines a `Suspend a` type which, when used in explicit
> type signatures, will cause the corresponding expression to be suspended.
>
> - The explicit calls to `Suspend` and `force` are inserted automatically by
>   the compiler doing demand analysis.
> - This demand analysis process will also ensure that there are not polynomial
>   chains of suspend and force being inserted to ensure performance.
>
> The actionables for this section are as follows:
>
> - Specify this much better.

A function argument can be prefixed by `~`. That instructs the Enso runtime
system to provide such an arguments a thunk - a function to be evaluated later.
Such a thunk function is they evaluated when the value of the argument is
accessed/used. Such an evaluation is performed every time the argument value is
used.

An atom argument can be prefixed by `~`. That instructs the Enso runtime system
to defer evaluation of the argument until it is first accessed. Then its thunk
is evaluated and when the evaluation is over, the atom argument value is
replaced by the computed value. Subsequent access to the atom argument will then
use the already evaluated value.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Specifying Suspension in the Type System](#specifying-suspension-in-the-type-system)

<!-- /MarkdownTOC -->

## Specifying Suspension in the Type System

> [!WARNING] The actionables for this section are:
>
> - Actually specify how the type system interacts with eager and lazy
>   evaluation.

Just use `~` to mark lazily computed function or atom arguments. The rest is
handled by the Enso runtime system.
