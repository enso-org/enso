---
layout: developer-doc
title: Access Modifiers
category: types
tags: [types, access-modifiers]
order: 4
---

# Access Modifiers
While we don't usually like making things private in a programming language, it
sometimes the case that it is necessary to indicate that certain fields should
not be touched (as this might break invariants and such like). To this end, Enso
provides an explicit mechanism for access modification.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Access Modification](#access-modification)
- [Private](#private)
- [Unsafe](#unsafe)

<!-- /MarkdownTOC -->

## Access Modification
Access modifiers in Enso work as follows:

- We provide explicit access modifiers that, at the definition site, start an
  indented block.
- All members in the block have the access modifier attributed to them.
- By default, accessing any member under an access modifier will be an error.
- To use members under an access modifier, you use the syntax `use <mod>`, where
  `<mod>` is a modifier. This syntax 'takes' an expression, including blocks,
  within which the user may access members qualified by the modifier `<mod>`.

> The actionables for this section are:
>
> - How do we type this?

## Private
The `private` modifier acts to hide implementation details from clients of the
API. It is:

- Available by default in the `Base` library.
- Able to be overridden using the above-described mechanism.

## Unsafe
While `private` works as you might expect, coming from other languages, the
`unsafe` annotation has additional restrictions:

- It must be explicitly imported from `Std.Unsafe`.
- When you use `unsafe`, you must write a documentation comment on its usage
  that contains a section `Safety` that describes why this usage of unsafe is
  valid.

> The actionables for this section are:
>
> - Specify `unsafe` properly.
