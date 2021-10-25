---
layout: section-summary
title: Enso's Type System
category: types
tags: [types, readme]
order: 0
---

# Enso's Type System

On the spectrum of programming type systems ranging from dynamically typed to
statically typed, one likes to think that there is a happy medium between the
two. A language that _feels_ dynamic, with high levels of type inference, but
lets the users add more type information as they want more safety and
compile-time checking.

Enso aims to be that language, providing a statically-typed language with a type
system that makes it feel dynamic. It will infer sensible types in many cases,
but as users move from exploratory pipelines to production systems, they are
able to add more and more type information to their programs, proving more and
more properties using the type system. This is based on a novel type-inference
engine, and a fusion of nominal and structural typing, combined with dependent
types.

All in all, the type system should stay out of the users' ways unless they make
a mistake, but give more experienced users the tools to build the programs that
they require.

This document contains discussion and designs for the type-system's behaviour,
as well as formal specifications where necessary. It discusses the impact of
many syntactic language features upon inference and type checking, and is
instrumental for ensuring that we build the right language.

> #### A Note on Syntax
>
> In the aid of precision, this document will use syntax that _may not_ be
> exposed to users. The appearance of a piece of syntax here that is not
> described in the [syntax](../syntax/README.md) document makes no promises as
> to whether said syntax will be exposed in the surface language.

> **Please Note:** The designs in this section are currently very exploratory as
> the type system is not slated from implementation until after 2.0.

Information on the type system is broken up into the following sections:

- [**Goals for the Type System:**](./goals.md) The goals for the Enso type
  system, particularly around usability and user experience.
- [**The Type Hierarchy:**](./hierarchy.md) The type hierarchy in Enso.
- [**Function Types:**](./function-types.md) Function types in Enso.
- [**Access Modification:**](./access-modifiers.md) Access modifiers in Enso
  (e.g. `private` and `unsafe`),
- [**Pattern Matching:**](./pattern-matching.md) The typing of pattern match
  expressions in Enso.
- [**Dynamic Dispatch:**](./dynamic-dispatch.md) How dynamic dispatch interacts
  with the type system in Enso.
- [**Modules:**](./modules.md) A description of the Enso module system.
- [**Monadic Contexts:**](./contexts.md) A description of the typing and
  functionality of Enso's monadic contexts.
- [**Strictness and Suspension:**](./evaluation.md) A description of how the
  type system interacts with Enso's evaluation semantics.
- [**Analysing Parallelism:**](./parallelism.md) A description of how the type
  system interacts with the planned automated parallelism analysis.
- [**Type-Directed Programming:**](./type-directed-programming.md) A description
  of how the type system aids type-directed programming, and the features it has
  to support this approach.
- [**Errors:**](./errors.md) The interaction between Enso's errors and the type
  system.
- [**Type Inference and Checking:**](./inference-and-checking.md) A description
  of Enso's type inference and checking system.
- [**Dependent Typing:**](./dependent-typing.md) A description of the dependent
  nature of Enso's type system.
- [**References:**](./references.md) Useful references for working on the type
  system and its theory.
