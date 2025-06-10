---
layout: developer-doc
title: Pattern Matching
category: types
tags: [types, pattern-matching]
order: 5
---

# Pattern Matching

Pattern matching in Enso follows typical operations promoted by various other
functional languages. Typing information is _always_ refined in the branches of
a case expression.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Positional Matching](#positional-matching)
- [Type Matching](#type-matching)
- [Name Matching on Labels](#name-matching-on-labels)
- [Naming Scrutinees](#naming-scrutinees)

<!-- /MarkdownTOC -->

## Positional Matching

It is possible to match on the scrutinee by structure for an atom:

```ruby
from Standard.Base.IO import println

type Vector a
  V2 x:a y:a
  V3 x:a y:a z:a

main =
    v = Vector.V3 "a" "b" "c"

    case v of
        Vector.V3 x _ _ -> println x
```

## Type Matching

Matching purely by the types involved, and not matching on structure.

```ruby
case v of
  v3:Vector -> print v3.x
```

> [!WARNING] > **Unsupported:** Name Matching on Labels
>
> Matching on the labels defined within a type for both atoms and typesets, with
> renaming.
>
> ```ruby
> case v of
>   Vector.V3 {x y} -> print x
>   {x}             -> print x
> ```

## Naming Scrutinees

Ascribing a name of a scrutinee is done using the standard typing judgement.
This works due to the type-term unification present in Enso.

```ruby
v = Vector.V3 "a" "b" "c"

f = case _ of
    Vector.V3 x _ _ -> println x

f v
```
