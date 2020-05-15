---
layout: developer-doc
title: Monadic Contexts
category: types
tags: [types, context, monad, effect]
order: 8
---

# Monadic Contexts
Coming from a Haskell background, we have found that Monads provide a great
abstraction with which to reason about program behaviour, but they have some
severe usability issues. The main one of these is the lack of automatic lifting,
requiring users to explicitly lift computations through their monad transformer
stack.

For a language as focused on usability as Enso is this really isn't feasible. To
that end, we have created the notion of a 'Monadic Context', which is a monad
transformer based on Supermonads (see
[references](./references.md#monadic-contexts)). These have special support in
the compiler, and hence can be automatically lifted to aid usability.

> The actionables for this section are:
>
> - Think about subsumption for contexts.
> - Contexts (e.g. IO) are represented using `T in IO`. Multiple contexts are
>   combined as standard `(IO | State Int)`, and it is written the same in arg
>   position.
> - Do we definitely want to use monads, or can we use arrows or other
>   interpreter-based effects systems? These may aid with parallelism analysis.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Context Syntax](#context-syntax)
- [Monadic Bind](#monadic-bind)
- [Context Definitions](#context-definitions)
- [Context Lifting](#context-lifting)

<!-- /MarkdownTOC -->

## Context Syntax
There are three main notes about the syntax of contexts:

1. Monadic contexts are defined using the `in` keyword (e.g. `Int in IO`).
2. We have a symbol `!`, which is short-hand for putting something into the
   `Exception` monadic context. This is related to broken values.
3. Contexts can be combined by using the standard typeset operators, or nested
   through repeated uses of `in`.

## Monadic Bind
It is also important to note that Enso has no equivalent to `<-` in Haskell.
Instead, pure computations are implicitly placed in the `Pure` monadic context,
and `=` acts to 'peel off' the outermost layer of contexts. As such, this means
that `=` _always_ acts as `bind`, greatly simplifying how the type-checker has
to work.

## Context Definitions
Contexts can be defined by users.

> The actionables for this section are:
>
> - How, what, when and why?

## Context Lifting
> The actionables for this section are:
>
> - Specify and explain how automated lifting of monadic contexts works.
