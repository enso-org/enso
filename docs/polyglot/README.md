---
layout: section-summary
title: Enso Polyglot Support
category: polyglot
tags: [polyglot, readme]
order: 0
---

# Enso Polyglot Support

Enso supports robust polyglot interoperability with other programming languages.

### Java Interop

Unlike many other programming languages the _system language_ of Enso (e.g. the
language that is used to do low-level operating system tasks) is **Java**. As
such a lot of attention has been dedicated to interoperability with **Java**:

- [**Java:**](./java.md) Detailed info about the Java polyglot bindings.

Many `Standard` libraries are using these `polyglot java import` statements.
Custom projects and libraries are encouraged to do the same. Interoperability
with Java is first class citizen in the Enso programming language.

### Python, JavaScript & co. Interop

Enso greatly benefits from the
[polyglot ecosystem of GraalVM](http://graalvm.org) and easily interops with any
language from that ecosystem. Including **JavaScript**, **Python**, **R**, etc.

- [**Python:**](./python.md) Specifics of the Python polyglot bindings.

Accessing other languages follows the same pattern. All of them are supported
via `foreign` function definitions - enough include the language support in the
system and the language gets automatically exposed via `foreign` directive to
Enso programs.

## Implementation Decisions

Technically speaking the `EnsoLanguage` doesn't support the other languages
directly, but it delegates to so called `EpbLanguage` to handle the _Enso
polyglot bindings_ during the runtime.

- [**Polyglot Bindings:**](./polyglot-bindings.md) A document providing an
  overview of the mechanisms provided to work with polyglot bindings in Enso.
- [**Typing Polyglot Bindings:**](./typing-polyglot-bindings.md) An exploration
  of how we can provide a modicum of type safety for the polyglot bindings in
  Enso.
