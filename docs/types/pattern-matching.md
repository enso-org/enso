---
layout: developer-doc
title: Pattern Matching
category: types
tags: [types, pattern-matching]
order: 5
---

# Pattern Matching

Pattern matching in Enso works similarly to as you would expect in various other
functional languages. Typing information is _always_ refined in the branches of
a case expression, which interacts well with dependent typing and type-term
unification.

> The actionables for this section :
>
> - How do we type pattern matching?
> - Exactly how (and what) evidence is discharged during matching?
> - How can we use bijective applications of the
>   [typeset operators](/hierarchy.md#typeset-operators) to perform pattern
>   matching?
> - How do we extend this to structural matching in general on typesets?

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Positional Matching](#positional-matching)
- [Type Matching](#type-matching)
- [Name Matching on Labels](#name-matching-on-labels)
- [Naming Scrutinees](#naming-scrutinees)

<!-- /MarkdownTOC -->

## Positional Matching

Matching on the scrutinee by structure. This works both for atoms and typesets
(for typesets it is a subsumption judgement).

```ruby
type Vector a
  V2 x:a y:a
  V3 x:a y:a z:a

v = Vector.V3 x y z

case v of
  Vector.V3 x y z -> print x
```

## Type Matching

Matching purely by the types involved, and not matching on structure.

```ruby
case v of
  Vector.V3 -> print v.x
```

## Name Matching on Labels

Matching on the labels defined within a type for both atoms and typesets, with
renaming.

```ruby
case v of
  Vector.V3 {x y} -> print x
  {x}             -> print x
```

## Naming Scrutinees

Ascribing a name of a scrutinee is done using the standard typing judgement.
This works due to the type-term unification present in Enso.

```ruby
case _ of
  v : Vector.V3 -> print v,x
```
