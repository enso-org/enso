---
layout: developer-doc
title: Polyglot Bindings
category: polyglot
tags: [polyglot, bindings, interop]
order: 1
---

# Polyglot Bindings

This document deals with the specification and design for the polyglot interop
system provided in the Enso runtime. This system allows users to connect Enso to
other supported programming languages, to both provide access to a wealth of
libraries, and to integrate Enso into existing systems.

The polyglot support in Enso is best-in class, and it supports this through two
main mechanisms:

1. **Polyglot FFI:** The low-level polyglot support provides a fairly low-level
   syntax sugar system for working with values from foreign languages.
2. **Embedded Syntax:** This system allows users to write code from other
   languages directly in their `.enso` files, and to seamlessly share values
   between Enso and that foreign code.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Impedance Mismatch](#impedance-mismatch)
- [The Polyglot FFI](#the-polyglot-ffi)
  - [Importing Polyglot Bindings](#importing-polyglot-bindings)
  - [Using Polyglot Bindings](#using-polyglot-bindings)
  - [Importing Polyglot Bindings \(Syntax\)](#importing-polyglot-bindings-syntax)
  - [Using Polyglot Bindings \(Syntax\)](#using-polyglot-bindings-syntax)
  - [Finding Polyglot Bindings](#finding-polyglot-bindings)
- [Embedded Syntax](#embedded-syntax)
  - [Embedded Syntax Usage \(Syntax\)](#embedded-syntax-usage-syntax)

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

The mechanism provides users with the facilities to import bindings from other
languages and call them via a generic mechanism.

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

### Importing Polyglot Bindings (Syntax)

Polyglot bindings can be imported using a polyglot import directive. This is
constructed as follows:

- The `polyglot` keyword
- A language identifier (e.g. `java`).
- The keyword `import`.
- Optionally (where the language supports it), an identifier for the type of
  language entity being imported (e.g. `struct` for `c`).
- A path that uniquely identifies the polyglot object to import.
- Optionally, the keyword `as`, followed by a new name.

For example:

```ruby
polyglot java import org.example.MyClass as MyClassJava
polyglot c import struct NetworkPacket as NetworkPacketC
```

### Using Polyglot Bindings (Syntax)

A polyglot binding is a polyglot object that has methods and/or fields defined
on it. Due to an impedance mismatch between languages, Enso implements a
variadic syntax for calling these polyglot bindings using vectors.

In essence, we have a primitive function as follows:

```ruby
Polyglot.method : Polyglot.Object -> [Any] -> Any
```

It works as follows:

- It is a method called `method` defined on the `Polyglot` type. The name
  `method` is, however, a stand-in for the name of the method in question.
- It takes an object instance of the polyglot object.
- It takes a vector of arguments (and is hence variadic).
- And it returns some value.

By way of example, the following is a valid usage of a polyglot binding:

```ruby
polyglot java import com.example.MyClass as MyClassJava

main =
    x = MyClassJava.foo [1, 2, 3]    # a static method
    inst = MyClassJava.new [a, b, c] # a constructor
    bar = inst.metod [x, y]          # a static method
```

### Finding Polyglot Bindings

Polyglot objects for various languages are found in the `polyglot` subdirectory
of an Enso project. This folder is subdivided into directories based on the
polyglot language. The name of each subdirectory must match the language
identifier used in the source code.

Inside each directory is an implementation-defined structure, with the polyglot
implementation for that particular language needing to specify it. Please see
the language-specific documentation for details.

## Embedded Syntax

The term "Embedded Syntax" is our terminology for the ability to use foreign
language syntaxes from directly inside `.enso` files. This system builds upon
the more generic mechanisms used by the [polyglot FFI](#the-polyglot-ffi) to
provide a truly seamless user experience.

### Embedded Syntax Usage (Syntax)

A polyglot block is introduced as follows:

- The `polyglot` keyword starts a block.
- This must be followed by a language identifier (e.g. `java`).
- After the language identifier, the remaining syntax behaves like it is an Enso
  function definition until the `=`.
- After the `=`, the user may write their foreign code.

```ruby
polyglot python concat a b =
  def concat(a, b):
    str(a) + str(b)
```

In the above example, this defines a function `concat` that takes two arguments
`a` and `b`, implemented in Python.

> The actionables for this section are:
>
> - Greatly flesh out the syntax for the high-level polyglot functionality.
