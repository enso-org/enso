---
layout: developer-doc
title: Access Modifiers
category: types
tags: [types, access-modifiers]
order: 4
---

# Access Modifiers

> [!WARNING] Everybody who ever maintained a large system knows
> [encapsulation is essential](../semantics/encapsulation.md).
>
> While we don't usually like making things private in a programming language,
> it sometimes the case that it is necessary to indicate that certain fields
> should not be touched (as this might break invariants and such like). To this
> end, Enso provides an explicit mechanism for access modification.

Enso targets large user base of _non-programmers_. They are mostly focused on
getting their job done and [encapsulation](../semantics/encapsulation.md) of
their own code is the last thing that comes to their mind.

On the other hand, Enso supports and encourages creation of _sharable
libraries_. Maintainers of such libraries are likely to treat API design and its
backward compatibility seriously. As such they need a way to
[encapsulate](../semantics/encapsulation.md) internals of their libraries and
clearly _differentiate public API_ and implementations details.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Access Modification](#access-modification)
- [Private](#private)

<!-- /MarkdownTOC -->

## Access Modification

_By default_ Enso elements (functions, types, methods) are _public_. One has to
use an access modifier to hide and [encapsulate](../semantics/encapsulation.md)
them. The reasoning is: those who don't care can access everything they create
without any restriction. Those who care can make things `private` with an
additional effort.

Accessing any member under an access modifier is an error when performed from
another project. Such a check is enforced during runtime.

There is a command line switch to _disable access modifier check_. It maybe be
useful for experimentation. However the general suggestion is: Avoid using it in
production.

## Private

Encapsulation is an effective _communication mechanism_ among _distributed
groups_ of developers. The `private` modifier hides implementation details from
clients of the API. The primary groups in the Enso case are those who _publish a
library_ and those who _consume such a library_.

As such Enso supports _library private_ encapsulation. To hide any element
(module, type, constructor, function) away from _library consumers_ prefix such
an element with `private` keyword.
