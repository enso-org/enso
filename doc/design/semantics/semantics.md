# Enso: The Semantics
Much like we have specifications for the [syntax](../syntax/syntax.md) and the
[type system](../types/types.md), we also need a document where we can specify
the executable semantics of portions of Enso.

This is that document, and it contains descriptions of key semantic portions of
Enso.

> The actionables for this section are:
>
> - As we make more semantic determinations about the language these should be
>   written down here.

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

### Optional Suspension
Laziness, however, can often be quite useful for defining certain kinds of API.
To that end, Enso provides support for optional laziness, more specifically
optional _suspension_, through the built-in `Suspended` type.

- When a type `a` is wrapped in a `Suspended`, it is turned into a thunk.
- A value of type `Suspended a` may be forced to execute the suspended
  computation and thereby obtain an `a`.

> The actionables for this section are:
>
> - Make this far better specified.
