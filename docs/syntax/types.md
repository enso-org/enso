---
layout: developer-doc
title: Types and Type Signatures
category: syntax
tags: [syntax, types]
order: 6
---

# Types and Type Signatures
Enso is a statically typed language, meaning that every variable is associated
with information about the possible values it can take. In Enso, the type
language is the same as the term language, with no artificial separation. For
more information on the type system, please see the [types](../types/README.md)
design document.

This section will refer to terminology that has not been defined in _this_
document. This is as this document is a specification rather than a guide, and
it is expected that you have read the above-linked document on the type-system
design as well.

Additionally, this document will colloquially refer to the left and right hand
sides of the type ascription operator `:` as the 'term' and 'type' levels,
respectively. In reality, there is no separation between the two in Enso, but it
is a useful way of thinking about things when discussing type signatures.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Type Signatures](#type-signatures)
- [Operations on Types](#operations-on-types)
- [Type Definitions](#type-definitions)
  - [Visibility and Access Modifiers](#visibility-and-access-modifiers)

<!-- /MarkdownTOC -->

## Type Signatures
Enso allows users to provide explicit type signatures for values through use of
the type ascription operator `:`. The expression `a : b` says that the value
`a` has the type `b` attributed to it.

```ruby
foo : (m : Monoid) -> m.this
```

Type signatures in Enso have some special syntax:

- The reserved name `in` is used to specify the monadic context in which a value
  resides. The Enso expression `a in IO` is equivalent to the Haskell
  `MonadIO a`.

  ```ruby
  foo : Int -> Int in IO
  ```

- The operator `!` is used to specify the potential for an _error_ value in a
  type. The expression `a ! E` says that the type is either an `a` or an error
  value of type `E`.

  ```ruby
  / : Number -> Number -> Number ! ArithError
  ```

In Enso, a type signature operates to constrain the values that a given variable
can hold. Type signatures are _always_ checked, but Enso may maintain more
specific information in the type inference and checking engines about the type
of a variable. This means that:

- Enso will infer constraints on types that you haven't necessarily written.
- Type signatures can act as a sanity check in that you can encode your
  intention as a type.
- If the value is of a known type (distinguished from a dynamic type),
  constraints will be introduced on that type.
- Where the type of the value is known, ascription can be used to constrain that
  type further.
- It is legal to add constraints to an identifier using `:` in any scope in
  which the identifier is visible.

From a syntactic perspective, the type ascription operator `:` has the following
properties:

- The right hand operand to the operator introduces a pattern context.
- The right hand side may declare fresh variables that occur in that scope.
- It is not possible to ascribe a type to an identifier without also assigning
  to that identifier.
- Introduced identifiers will always shadow other identifiers in scope due to
  the fact that `:` introduces a new scope on its RHS.
- Constraint implication is purely feed-forward. The expression `b:A` only
  introduces constraints to `b`.

> The actionables for this section are:
>
> - How do signatures interact with function bodies in regards to scoping?
> - Does this differ for root and non-root definitions?

## Operations on Types
Enso also provides a set of rich operations on its underlying type-system notion
of typesets. Their syntax is as follows:

- **Union - `|`:** The resultant typeset may contain a value of the union of its
  arguments.
- **Intersection - `&`:** The resultant typeset may contain values that are
  members of _both_ its arguments.
- **Subtraction - `\`:** The resultant typeset may contain values that are in
  the first argument's set but not in the second.

> The actionables for this section are:
>
> - Unify this with the types document at some point. The types document
>   supersedes this section while this actionable exists.

## Type Definitions
Types in Enso are defined by using the `type` reserved name. This works in a
context-dependent manner that is discussed properly in the
[type system design document](../types/README.md), but is summarised
briefly below.

- **Name and Fields:** When you provide the keyword with only a name and some
  field names, this creates an atom.

  ```ruby
  type Just value
  ```

- **Body with Atom Definitions:** If you provide a body with atom definitions,
  this defines a smart constructor that defines the atoms and related functions
  by returning a typeset.

  ```ruby
  type Maybe a
      Nothing
      type Just (value : a)

      isJust = case this of
          Nothing -> False
          Just _ -> True

      nothing = not isJust
  ```

  Please note that the `type Foo (a : t)` is syntax only allowable inside a type
  definition. It defines an atom `Foo`, but constrains the type variable of the
  atom _in this usage_.

- **Body Without Atom Definitions:** If you provide a body and do not define any
  atoms within it, this creates an interface that asserts no atoms as part of
  it.

  ```ruby
  type HasName
  name: String
  ```

In addition, users may write explicit `this` constraints in a type definition,
using the standard type-ascription syntax:

```ruby
type Semigroup
    <> : this -> this

type Monoid
    this : Semigroup
    use Nothing
```

### Visibility and Access Modifiers
While we don't usually like making things private in a programming language, it
sometimes the case that it is necessary to indicate that certain fields should
not be touched (as this might break invariants and such like). To this end, we
propose an explicit mechanism for access modification that works as follows:

- We provide explicit access modifiers that, at the definition site, start an
  indented block. These are `private` and `unsafe`.
- All members in the block have the access modifier attributed to them.
- By default, accessing any member under an access modifier will be an error.
- To use members under an access modifier, you use the syntax `use <mod>`, where
  `<mod>` is a modifier. This syntax 'takes' an expression, including blocks,
  within which the user may access members qualified by the modifier `<mod>`.

While `private` works as you might expect, coming from other languages, the
`unsafe` annotation has additional restrictions:

- It must be explicitly imported from `Std.Unsafe`.
- When you use `unsafe`, you must write a documentation comment on its usage
  that contains a section `Safety` that describes why this usage of unsafe is
  valid.
