# Polyglot Java Semantics
This document deals with the Enso-level semantics of polyglot interop with
Java. It talks about the issues with (and solutions for) matching the runtime
semantics of two quite different languages.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Impedance Mismatch](#impedance-mismatch)
- [The Polyglot FFI](#the-polyglot-ffi)
    - [Importing Polyglot Bindings](#importing-polyglot-bindings)
    - [Using Polyglot Bindings](#using-polyglot-bindings)

<!-- /MarkdownTOC -->

## Impedance Mismatch
Polyglot interoperation in Enso has a significant impedance mismatch. In
essence, this means that there is a mismatch between Enso's language semantics
and the semantics of the foreign languages that are being worked with.

While some of this mismatch can be worked around by manually wrapping the
foreign constructs in Enso, there are still concepts that can't easily be
represented by Enso.

> The actionables for this section are:
>
> - Expand on the impedance mismatch and how it leads to the defined semantics.

## The Polyglot FFI
The low-level polyglot FFI mechanism refers to a way to use polyglot objects
directly in Enso code. This can be used to underlie a library implementaion in
Enso, or to interoperate with code running in other languages.

### Importing Polyglot Bindings
When importing a polyglot binding into scope in an Enso file, this introduces a
_polyglot object_ into scope. This object will have appropriate fields and/or
methods defined on it, as described by the foreign language implementation.

> The actionables for this section are:
>
> - Expand greatly on the detail of this as the semantics of the imports become
>   clearer.

### Using Polyglot Bindings
With a polyglot object in scope, the user is free to call methods on it
directly. These polyglot objects are inherently dynamically typed, meaning that
any operation may _fail_ at runtime.

Enso implements a generic variadic syntax for calling polyglot functions using
vectors of arguments. In essence, this is necessary due to the significant
impedance mismatch between Enso's runtime semantics (let alone the type system)
and the runtime semantics of many of the polyglot languages.

We went the way of the variadic call for multiple reasons:

- It allows us to match up with a wide range of language semantics (such as
  subtyping and overloading).
- It is flexible and easy to expand in the future.
- We can easily build a more Enso-feeling interface on top of it.
- It can still be typed due to our plans for dependent vector types.

By way of illustrative example, Java supports method overloading and subtyping,
two things which have no real equivalent in the Enso type system.

> The actionables for this section are:
>
> - Expand greatly on the runtime semantics of working with polyglot bindings.
> - Determine how to make the inherent 'failability' of polyglot objects safer.
