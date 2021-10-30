---
layout: developer-doc
title: Typing the Polyglot Bindings
category: polyglot
tags: [polyglot, types]
order: 2
---

# Typing the Polyglot Bindings

The polyglot bindings inherently provide a problem for the Enso type system.
When many of the languages with which we can interoperate are highly dynamic and
flexible, or have significant mismatches between their type system and Enso's,
we can only make a best effort attempt to maintain type safety across this
boundary.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Enso Values](#enso-values)
- [Polyglot Values](#polyglot-values)
- [Dynamic](#dynamic)
  - [The Enso Boundary](#the-enso-boundary)

<!-- /MarkdownTOC -->

## Enso Values

The underlying nature of our runtime allows us to pass Enso values across the
polyglot boundary while ensuring that they aren't modified. This means that the
typing information known about a value `v` _before_ it is passed to a polyglot
call is valid after the polyglot call, as long as the following properties hold:

- The polyglot call does not modify the entity passed in.
- The polyglot call returns an entity of the same type.

However, there are sometimes cases where we _want_ to let an Enso value be used
freely by the polyglot language. To that end, we have to have some way of
distinguishing _safe_ usages of Enso values from _unsafe_ ones. In the latter
case, the value needs to be treated as `Dynamic` after its use.

> The actionables for this section are:
>
> - Think much more on this.

## Polyglot Values

In the presence of a polyglot value, however, there is very little that we can
determine about a value with which we are working. This means that we need to
have a principled way to assert properties on a polyglot object that can then be
reflected in the Enso type system. This mechanism needs to deal with:

- Concurrent access to polyglot objects.
- Mutation and modification of polyglot objects.
- Potentially taking _ownership_ of polyglot objects.

> The actionables for this section are:
>
> - Reflect more on this problem and think about what principled approaches we
>   could take to it.

## Dynamic

As Enso can seamlessly interoperate with other programming languages, we need a
principled way of handling dynamic types that we don't really know anything
about. This mechanism needs:

- A way to record what properties we _expect_ from the dynamic.
- A way to turn a dynamic into a well-principled type-system member without
  having the dynamics pollute the whole type system. This may involve a 'trust
  me' function, and potentially dynamicness-polymorphic types.
- A way to understand as much as possible about what a dynamic _does_ provide.
- A way to try and refine information about dynamics where possible.

```ruby
obj.model =
    { atom  : Text
    , dict  : Map Text Any
    , info  :
        { doc  : Text
        , name : Text
        , code : Text
        , loc  : Location
        }
    , arg  : # used only when calling like a function
        { doc     : Text
        , default : Maybe Any
        }
    }
```

> The actionables for this section are:
>
> - Work out how to do dynamic properly, keeping in mind that in a dynamic value
>   could self-modify underneath us.

### The Enso Boundary

Fortunately, we can at least avoid foreign languages modifying memory owned by
the Enso interpreter. As part of the interop library, Graal lets us mark memory
as read-only. This means that the majority of data passed out (from a functional
language like Enso) is not at risk. However, if we _do_ allow data to be worked
with mutably, when control is returned to Enso it needs to be treated as a
dynamic as it may have been modified.
