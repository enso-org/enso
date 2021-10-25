---
layout: developer-doc
title: Dependent Typing
category: types
tags: [types, inference, checking, dependent, dependency]
order: 14
---

# Dependent Typing

Enso is a [dependently typed](https://en.wikipedia.org/wiki/Dependent_type)
programming language. This means that types are first-class values in the
language, and hence can be manipulated and computed upon just like any other
value. To the same end, there is no distinction between types and kinds, meaning
that Enso obeys the 'Type in Type' axiom of dependent types. While this does
make the language unsound as a logic, the type safety properties do not depend
on this fact.

In essence, values are types and types are values, and all kinds are also types.
This means that, in Enso, you can run _arbitrary_ code to compute with types.
All in all, dependency in Enso is not about being a proof system, but is instead
about enabling practical usage without hampering usability. To that end we
combine a powerful and non-traditional dependently-typed system with the ability
to automate much of the proof burden through SMT solvers.

> The actionables for this section are:
>
> - Do we want the ability to explicitly quantify type variables for visibility,
>   dependency, relevance and requiredness (forall, foreach, etc).
> - How do we infer as much of these above properties as possible?
> - Based on QTT and RAE's thesis.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Proving Program Properties](#proving-program-properties)
- [Automating the Proof Burden](#automating-the-proof-burden)

<!-- /MarkdownTOC -->

## Proving Program Properties

Some notes:

- Dependent types as constructing proofs through combining types. Combining
  types provides evidence which can be discharged to create a proof. A value can
  then only be constructed by discharging a proof.
- To provide more predictable inference in dependent contexts, this system will
  draw on the notion of _matchability polymorphism_ as outlined in the
  higher-order type-level programming paper. The key recognition to make,
  however is that where that paper was required to deal with
  backwards-compatibility concerns, we are not, and hence can generalise all
  definitions to be matchability polymorphic where appropriate.
- Provide some keyword (`prove`) to tell the compiler that a certain property
  should hold when typechecking. It takes an unrestricted expression on types,
  and utilises this when typechecking. It may also take a string description of
  the property to prove, allowing for nicer error messages:

  ```
  append : (v1 : Vector a) -> (v2 : Vector a) -> (v3 : Vector a)
  append = vec1 -> vec2 ->
      prove (v3.size == v1.size + v2.size) "appending augments length"
      ...
  ```

  Sample error:

  ```
  [line, col] Unable to prove that "appending augments length":
      Required Property: v3.size == v1.size + v2.size
      Proof State:       <state>

      <caret diagnostics>
  ```

  This gives rise to the question as to how we determine which properties (or
  data) are able to be reasoned about statically.

- Dependent types in Enso will desugar to an application of Quantitative Type
  Theory.

> The actionables for this section are:
>
> - Specify how we want dependency to behave in a _far more rigorous_ fashion.

## Automating the Proof Burden

Even with as capable and simple a dependently-typed system as that provided by
Enso, there is still a burden of proof imposed on our users that want to use
these features. However, the language [F\*](https://www.fstar-lang.org/) has
pioneered the combination of a dependently-typed system with an SMT solver to
allow the automation of many of the simpler proofs.

- The Enso typechecker will integrate an aggressive proof rewrite engine to
  automate as much of the proof burden as possible.

> The actionables for this section are:
>
> - What is the impact of wanting this on our underlying type theory?
> - Where and how can we draw the boundary between manual and automated proof?
> - How much re-writing can we do (as aggressive as possible) to assist the SMT
>   solver and remove as much proof burden as possible.
