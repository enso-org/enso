---
layout: developer-doc
title: Scoping Rules
category: semantics
tags: [semantics, scoping]
order: 7
---

# Scoping Rules

Enso's scoping rules should be fairly familiar to those coming from other
languages that are immutable (or make heavy use of immutability). In essence,
Enso is a lexically-scoped language where bindings may be shadowed in child
scopes.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Scopes](#scopes)
- [Introducing New Scopes](#introducing-new-scopes)
  - [Scoping of Type Signatures](#scoping-of-type-signatures)
- [Implementation Notes](#implementation-notes)
  - [Function Call Arguments](#function-call-arguments)
  - [Collapsing Scopes](#collapsing-scopes)

<!-- /MarkdownTOC -->

## Scopes

A scope is the span in the code within which a set of accessible identifiers
occurs. A nested scope may:

- Reference identifiers defined in parent scopes.
- Shadow identifiers from parent scopes with a new binding.
- Ascribing a type identifier to a binding outside the current scope.

Identifier visibility behaves as follows:

- Identifiers are bound by using a variable name in a pattern context (e.g. the
  LHS of a binding, a function argument, or a case expression pattern).
- Identifiers are accessible only _after_ they have been defined.
- Identifiers introduced into a given scope `s` are accessible in `s` and all
  the children of `s`, _after_ the point at which they are introduced.
- If a scope uses an identifier defined in an outer scope, and then later (in
  the thread of execution) shadows that variable, any usage before the shadowing
  point refers to the occurrence in the outer scope.

The term _accessible_ is defined to mean "can be referred to in the code as a
valid entity," and hence implies "can have its value used."

> The actionables for this section are:
>
> - In the future we may want to relax the forward-definition restriction for
>   pure bindings, allowing a form of recursive pure binding hoisting (like a
>   let block). This would use the monadic context's `fix` function.
> - Once we are capable of supporting `fix` and recursive pure bindings in
>   contexts, we need to revisit the above rules.

## Introducing New Scopes

The following constructs introduce new scopes in Enso:

- **Modules:** Each module (file) introduces a new scope.
- **The Function Arrow `(->)`:** The arrow operator introduces a new scope that
  is shared by both of its operands. This is true both when it is used for a
  lambda (value or type), and when used to denote case branches.
- **Code Blocks:** A code block introduces a new scope.
- **The Type Ascription Operator:** The type ascription operator introduces a
  new scope on its right hand side.

A new scope is _always_ introduced as a child of the scope in which the
introducing construct occurs, unless explicitly noted otherwise.

There are other linguistic constructs that _behave_ as if they introduce a
scope, but this is purely down to the fact that they desugar to one or more of
the above constructs:

- **Method Definitions:** A method definition introduces a new scope. This is
  simply because the method definition desugars to a lambda definition.
- **Function Definitions:** A function definition introduces a new scope. This
  is simply because the method definition desugars to a lambda definition.

> The actionables for this section are:
>
> - Write this out in more detail when we have time. The above is only intended
>   as a brief summary.
> - Decide if we want to support local overloads at all (differing in the type
>   of `this`). Local overloads are those that are not defined at the top level
>   of a module, and are currently unsupported.
> - We need to refine the specification for body-signature mutual scoping.
> - NOTE: The note about case-branch scoping needs to be refined as the
>   implementation of `case` evolves.

### Scoping of Type Signatures

Currently, type signatures in Enso obey a simple set of typing rules:

- The RHS of the type ascription introduces a new scope that is a child of the
  scope in which the LHS of the type ascription exists.

> The actionables for this section are:
>
> In order to enable much of the flexible metaprogramming ability that Enso aims
> for, we have an additional set of scoping rules for type signatures:
>
> - Both operands of the type ascription operator share a scope.
> - If two names are used on the type and term levels to refer to the same
>   entity, both are valid but this issues a warning. Referring to the same
>   entity means that they are two names for the same underlying object.
> - Name clashes are disallowed unless the clashing names refer to the same
>   entity.
> - Do we actually want to support this?
> - What complexities does this introduce wrt typechecking?

## Implementation Notes

This section contains notes on the implementation of the Enso scoping rules in
the interpreter.

### Function Call Arguments

In order to support suspended function arguments in the interpreter in a
performant way, we implicitly wrap _all_ function arguments in a suspension. In
conjunction with making the function itself responsible for when its arguments
are evaluated, this lets us have incredibly performant suspended computations in
Enso.

However, it _does_ require creating a small hack in the Alias Analysis process:

- In order for an expression to be a suspension, it must occur in its own scope
  (the suspended scope).
- Alias analysis must account for this, otherwise the code generator will get
  frame accesses incorrect.

To this end, we account for this implementation detail in alias analysis.

### Collapsing Scopes

Another quirk of the internal alias analysis process is down to the fact that
the Enso IR represents Methods, functions, and blocks as separate constructs.
This means that if you had a method containing a function containing a block, a
naive implementation of alias analysis would allocate three scopes here.

This is incorrect, according to the semantic specification of the language, so
the alias analysis process needs to handle the collapsing of these scopes as it
allocates them. The rules are as follows:

- If you have a method whose body is a function, they share a scope.
- If you have a function whose body is a block, they share a scope.
