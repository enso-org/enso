---
layout: developer-doc
title: Monadic Contexts
category: types
tags: [types, context, monad, effect]
order: 8
---

# Monadic Contexts

> [!WARNING] Reword for people without Haskell background who don't know what
> _lifting_ is.
>
> Coming from a Haskell background, we have found that Monads provide a great
> abstraction with which to reason about program behaviour, but they have some
> severe usability issues. The main one of these is the lack of automatic
> lifting, requiring users to explicitly lift computations through their monad
> transformer stack.

For a language as focused on usability as Enso is importing all the _complexity
of Haskell monads_ really isn't feasible. To that end, we have created the
notion of a 'Monadic Context', which is a monad transformer based on Supermonads
(see [references](./references.md#monadic-contexts)). These have special support
in the compiler, and hence can be automatically lifted to aid usability.

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
- [Inbuilt Contexts](#inbuilt-contexts)
  - [IO](#io)
  - [State](#state)

<!-- /MarkdownTOC -->

## Context Syntax

> [!WARNING] There used to be three main notes about the syntax of contexts:
>
> 1. Monadic contexts are defined using the `in` keyword (e.g. `Int in IO`).
> 2. We have a symbol `!`, which is short-hand for putting something into the
>    `Exception` monadic context. This is related to broken values.
> 3. Contexts can be combined by using the standard typeset operators, or nested
>    through repeated uses of `in`.

There is no special syntax for contexts anymore. Since
[#3828](https://github.com/enso-org/enso/pull/3828) Enso is no longer relaying
on a haskelly solution. Rather than that _contexts_ are being manupulated by
_standard library_ functions grouped around `Standard.Base.Runtime.Context` &
co.

```ruby
Runtime.Context.Output.with_enabled <|
    File.new "c:\trash.txt" . delete
```

There is still the `!` symbol signaling [presence of errors](./errors.md)

- e.g. _broken values_. However the runtime can handle _broken values_ even
  without presence of these _exception type signatures_. Thus the compiler only
  verifies the referenced types are valid.

## Monadic Bind

> [!WARNING] Who knows what `<-` means in Haskell?
>
> It is also important to note that Enso has no equivalent to `<-` in Haskell.
> Instead, pure computations are implicitly placed in the `Pure` monadic
> context, and `=` acts to 'peel off' the outermost layer of contexts. As such,
> this means that `=` _always_ acts as `bind`, greatly simplifying how the
> type-checker has to work.

## Inbuilt Contexts

Enso standard library defines `Input`, `Output` and `Dataflow_Stack_Trace`
contects as of Enso 2024.5.1 version. Users cannot define their own.

### State

The _state_ concept is implement by standard libraries with _no support in the
type system_.

State acts as a
[thread local](https://en.wikipedia.org/wiki/Thread-local_storage) variable of
operating system:

<!-- (well, it will when #7117 gets fixed)  -->

- an _initializing code_ can set `State` up
- execute some code
- a code somewhere deep the stack (while _initializing code_ is still on the
  stack)
- may pick the state up
- once the _initializing code_ finishes execution
- the state is gone

It is an example of _tunnelling a value_ from one side (e.g. code) of the
"tunnel" to another, without the "tunnel" (e.g. thee code in between) knowing
about it.

<!--
> There has to be some Haskell monad concept for it all... but we don't need it
> as Enso is dynamicly typed language.
>
> - Describe how dependently-typed maps allow us to provide more flexible
>   interfaces in future.
-->

See `Standard.Base.Runtime.State` for more details.
