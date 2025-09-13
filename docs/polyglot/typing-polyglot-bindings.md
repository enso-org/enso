---
layout: developer-doc
title: Typing the Polyglot Bindings
category: polyglot
tags: [polyglot, types]
order: 2
---

# Typing the Polyglot Bindings

The polyglot bindings inherently provide a problem for the Enso static type
system. Many of the languages with which we can interoperate are highly dynamic
and flexible, or have significant mismatches between their type system and Enso.
The Enso static analysis just gives up when dealing with such
[polyglot bindings](./polyglot-bindings.md).

## Enso Values

Enso values are immutable which allows us to pass Enso values across the
polyglot boundary while ensuring that they aren't modified by foreign languages.
This means that the typing information known about a value `v` _before_ it is
passed to a polyglot call is valid after the polyglot call,

## Polyglot Values

In the presence of a polyglot value, however, there is very little that we can
determine about a value with which we are working. This means that we need to
have a principled way to assert properties on a polyglot object that can then be
reflected in the Enso type system. An object coming from foreign language can
mutate and change its state anytime.

### Array like Structures

GraalVM `InteropLibrary` offers a protocol for recognizing certain types of
objects. For example one can find out whether an object `hasArrayElements`. Such
objects are then recognized as _array like structures_.

Enso distinguishes between its own _array like structures_ and foreign ones.
While Enso `Vector` is immutable, _array like foreign objects_ may potentially
mutate. To address that Enso offers two types:

- `Vector` - guaranteed (by those who create it) to be immutable
- `Array` - an _array like structure_ which may potentially change
- both these types follow the same interface and offer the same methods

There is no way in Enso to modify objects of type `Array` (neither `Vector`). A
**JavaScript** or **Python** allocated `Array` can mutate during execution of
the program. Should one need to guarantee immutability, one can convert `Array`
to `Vector` with `array_like_foreign_object . to Vector` conversion. Such a
conversion creates read-only snapshot of the original _array like object_.
