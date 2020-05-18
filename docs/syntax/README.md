---
layout: section-summary
title: Enso's Syntax
category: syntax
tags: [syntax, readme]
order: 0
---

# Enso's Syntax
When working with a programming language, the syntax is the first thing that a
user encounters. This makes it _utterly integral_ to how users experience the
language, and, in the case of Enso, the tool as a whole.

Enso is a truly novel programming language in that it doesn't have _one_ syntax,
but instead has two. These syntaxes are dual: visual and textual. Both are
first-class, and are truly equivalent ways to represent and manipulate the
program. To that end, the design of the language's syntax requires careful
consideration, and this document attempts to explain both the _what_, of Enso's
syntax, but also the _why_.

Furthermore, Enso is novel in the fact that it does not enforce any artificial
restriction between the syntaxes of its type and value levels: they are one and
the same. This enables a staggering level of uniformity when programming in the
language, allowing arbitrary computations on types, because in a
dependently-typed world, they are just values.

The various components of Enso's syntax are described below:

- [**Encoding:**](./encoding.md) The source encoding of Enso files.
- [**Naming:**](./naming.md) The naming of Enso language constructs.
- [**Layout Rules:**](./layout.md) The layout rules for Enso code.
- [**Literals:**](./literals.md) The syntax for Enso literals.
- [**Assignment:**](./assignment.md) The syntax for various forms of assignment
  expression in Enso.
- [**Types and Type Signatures:**](./types.md) The syntax for writing types and
  type signatures in Enso.
- [**Macros:**](./macros.md) The syntax for writing macros in Enso.
- [**Top-Level Syntax:**](./top-level.md) The syntax at the top-level of an Enso
  file.
- [**Functions:**](./functions.md) The syntax for writing functions in Enso.
- [**Function Arguments:**](./function-arguments.md) The syntax for function
  arguments in Enso.
- [**Field Access:**](./projections.md) The syntax for working with fields of
  data types in Enso.
- [**Comments:**](./comments.md) The syntax for writing comments in Enso.
