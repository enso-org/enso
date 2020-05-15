---
layout: developer-doc
title: Function Types
category: types
tags: [types, functions]
order: 3
---

# Function Types
As a functional programming language, the type of functions in Enso (`->`) is
key. There are a few things that should be noted about functions in Enso.

> The actionables for this section:
>
> - Work out a full specification for function typing and behaviour.
> - Calling a function with an upper-case letter instantiates all of its type
>   arguments to free type variables.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Scoping](#scoping)
- [Structural Type Shorthand](#structural-type-shorthand)
- [Function Composition](#function-composition)

<!-- /MarkdownTOC -->

## Scoping
Enso's scoping rules should be fairly familiar to most other programming
languages, as it uses standard lexical scoping with shadowing. However, there
are a few unique points to keep in mind:

- There is no separation between the type and term levels. This means that, for
  a given lexical scope, type, function and lambda variables exist in the same
  name space.
- This means that if different names are used on the type and value level to
  refer to the same entity, both names are valid.
- Name clashes between different entities are not allowed.
- Shadowing between different lexical scopes occurs as standard.
- Shadowing in the same lexical scope may only occur in a chain of lambdas, and
  in such a case the shadowed variables must unify.
- Variables from the body are accessible in the type signature.
- Variables from the type signature are accessible in the body.

Scope lookup proceeds from the function body outwards, as is standard for
lexical scoping. For a detailed discussion of scoping, please see
[the scoping documentation](../semantics/scoping.md).

## Structural Type Shorthand
In Enso, we want to be able to write a type-signature that represents types in
terms of the operations that take place on the input values. A classical example
is `add`:

```ruby
add : a -> b -> b + a
add = a -> b -> b + a
```

There are a few things to note about this signature from a design standpoint:

- `a` and `b` are not the same type. This may, at first glance, seem like a
  signature that can't work, but the return type, in combination with our
  integrated `Convertible` mechanism gives us the tools to make it work.
- The return type is `a + b`. This is a shorthand expression for a detailed
  desugaring. The desugaring provided below is what the typechecker would infer
  based on such a signature.

```ruby
add : forall a b c d. ({+ : Convertible b c => a -> c -> d} <: a) => a -> b -> d
```

This may look fairly strange at first, but we can work through the process as
follows:

1. The expression `b + a` is syntactic sugar for a method call on a: `a.+ b`.
2. This means that there must be a `+` method on a that takes both an `a` and a
   `b`, with return-type unspecified as of yet: `+ : a -> b -> ?`
3. However, as `Convertible` is built into the language, we have to consider
   that for `a.+ b` to work, the `+` method can actually take any type to which
   `b` converts. This introduces the constraint `Convertible b c`, and we get
   `+ : a -> c -> ?`
4. The return type from a function need not be determined by its arguments, so
   hence in the general case we have to default to an unrestricted type variable
   giving `+ a -> c -> d`.
5. This method must exist on the type `a`, resulting in the constraint that the
   row `{+ : a -> c -> d} <: a` must conform to that interface.
6. We now know the return type of `a + b`, and can rewrite it in the signature
   as `d`.

Please note that `a <: b` (which can be flipped as `:>`) means that `a` is a row
that is a sub-row contained within the row `b`. The containment relation allows
for the possibility that `a == b`.

The inferred type for any function should, in general, match the given type
signature. Cases where this break down should only exist where the type
signature acts to constrain the function type further than would be inferred.

> The actionables for this section are:
>
> PLEASE NOTE. THIS IS OUT OF DATE.
>
> - Clean it up and work out how it applies in light of the new developments of
>   typesets.

## Function Composition
Enso introduces a function composition operator which composes functions after
all arguments have been applied. This operator is `>>` (and its backwards cousin
`<<`). It takes a function `f` with `n` arguments, and a function `g` with `m`
arguments, and the result consumes `n` arguments, applies them to `f`, and then
applies the result of that plus any additional arguments to `g`.

```ruby
computeCoeff = (+) >> (*5)

doThing = (+) >> (*)
```

In addition, we have the operator `.`, which acts as standard forward function
chaining in Enso, and its backwards chaining cousin `<|`.

> The actionables from this section are:
>
> - Examples for the more advanced use-cases of `>>` to decide if the type
>   complexity is worth it.
> - Otherwise, standardise on using `>>` and `<<` for standard function
>   composition:
>   + `<< : (b -> c) -> (a -> b) -> a -> c` - backwards composition (standard)
>   + `>> : (a -> b) -> (b -> c) -> a -> c` - forwards composition
