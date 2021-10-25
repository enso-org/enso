---
layout: developer-doc
title: Access Modifiers
category: types
tags: [types, access-modifiers]
order: 4
---

# Access Modifiers

While we don't usually like making things private in a programming language, it
sometimes the case that it is necessary to indicate that certain fields should
not be touched (as this might break invariants and such like). To this end, Enso
provides an explicit mechanism for access modification.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Access Modification](#access-modification)
- [Private](#private)
- [Unsafe](#unsafe)

<!-- /MarkdownTOC -->

## Access Modification

Access modifiers in Enso work as follows:

- We have a set of access modifiers, namely `private` and `unsafe`.
- We can place these modifiers before a top-level definition.

  ```ruby
  type MyAtomType
      type MyAtom a

      is_foo : Boolean
      is_foo = ...

      private private_method a b = ...

      unsafe unsafe_method a b = ...
  ```

- By default, accessing any member under an access modifier is an error when
  performed from another module.
- To use members protected by an access modifier, you must _import_ that access
  modifier from the file in which you want to access those elements.

  ```ruby
  import private Base.Data.Vector
  import unsafe Base.Data.Atom
  ```

- These modified imports are available in _all_ scopes, so it is possible to
  limit the scope in which you have access to the modified definitions.

  ```ruby
  function_using_modifiers v x =
      import private Base.Data.Vector
      import unsafe Base.Data.Atom

      v.mutate_at_index 0 (_ -> x)
      x = MyAtom.mutate_field name="sum" (with = x -> x + 20)
      x + 20
  ```

> The actionables for this section are:
>
> - How do we type this?

## Private

The `private` modifier acts to hide implementation details from clients of the
API. It is:

- Available by default in the `Base` library.
- Able to be avoided using the above-described mechanism.

## Unsafe

While `private` works as you might expect, coming from other languages, the
`unsafe` annotation has additional restrictions:

- It must be explicitly imported from `Base.Unsafe`.
- When you use `unsafe`, you must write a documentation comment on its usage
  that contains a section `Safety` that describes why this usage of unsafe is
  valid.

> The actionables for this section are:
>
> - Specify `unsafe` properly.
