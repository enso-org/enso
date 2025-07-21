---
layout: section-summary
title: Enso Polyglot Support
category: polyglot
tags: [polyglot, readme]
order: 0
---

# Enso Polyglot Support

Enso supports robust interoperability with other programming languages - e.g.
Enso is a _polyglot programming_ languge.

### interoperability with Java

Unlike many other programming languages the _system language_ of Enso (e.g. the
language that is used to do low-level operating system tasks) is **Java**. As
such a lot of attention has been dedicated to make interoperability with
**Java** as smooth as possible:

- [**Java:**](./java.md) Detailed info about the Java polyglot bindings.

Many `Standard` libraries are using these `polyglot java import` statements.
Custom projects and libraries are encouraged to do the same. Interoperability
with Java is a first class citizen in the Enso programming language.

### Interoperability with Python, JavaScript & co.

Enso greatly benefits from the
[polyglot ecosystem of GraalVM](http://graalvm.org) and easily interops with any
language from that ecosystem. Including **JavaScript**, **Python**, **R**, etc.

- [**Python:**](./python.md) Specifics of the Python polyglot bindings.

Interop with these _dynamic languages_ is primarily supported via
[foreign function definitions](./polyglot-bindings.md#foreign-functions). When
accessing Python, as well as other dynamic languages, the same pattern is used.
Enough include the language support in the
[distribution](../distribution/distribution.md) and the language gets
automatically exposed via `foreign` directive to Enso programs.

## Implementation

Additional overview is provided in following documents:

- [**Polyglot Bindings:**](./polyglot-bindings.md) A document providing an
  overview of the mechanisms provided to work with polyglot bindings in Enso.
- [**Typing Polyglot Bindings:**](./typing-polyglot-bindings.md) An exploration
  of how we can provide a modicum of type safety for the polyglot bindings in
  Enso.

Implementation details are described in
[EpbLanguage javadoc](../../engine/runtime-language-epb/src/main/java/org/enso/interpreter/epb/EpbLanguage.java).
