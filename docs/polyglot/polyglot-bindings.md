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

## Impedance Mismatch

Polyglot interoperation in Enso has a significant impedance mismatch. In
essence, this means that there is a mismatch between Enso's language semantics
and the semantics of the foreign languages that are being worked with.

- Enso is designed as a functional programming language
  - relies on referrential transparency
  - relies on minimal side effects of computations
- foreign languages aren't designed that way

Some of thes mismatches can be worked around by manually wrapping the foreign
constructs in Enso, however some just cannot. Care must be taken when dealing
with other languages and especially their side-effects.

## `polyglot import`

Accessing existing objects of foreign languages can be done via
`polyglot xyz import` statements. This primarily works for **Java** classes:

- [**Java:**](./java.md) Detailed info about the Java polyglot bindings.

The _polyglot import directive_ is constructed as follows:

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
polyglot c import struct NetworkPacket
```

Once imported the `MyClassJava` as well as `NetworkPacket` objects behave as
`Any` Enso objects. Such objects have methods and/or fields defined on them. The
following is a valid usage of a polyglot binding:

```ruby
main =
    x = MyClassJava.foo [1, 2, 3]    # a static method
    inst = MyClassJava.new [a, b, c] # a constructor
    bar = inst.method [x, y]         # an instance method
```

### Using Polyglot Bindings

With a polyglot object in scope, the user is free to call methods on it
directly. These polyglot objects are inherently dynamically typed, meaning that
they have `Any` type - e.g. any operation may _fail_ at runtime.

Enso implements a generic variadic syntax for calling polyglot functions using
vectors of arguments. In essence, this is necessary due to the significant
impedance mismatch between Enso's runtime semantics (let alone the type system)
and the runtime semantics of many of the polyglot languages.

We went the way of the variadic call for multiple reasons:

- It allows us to match up with a wide range of language semantics (such as
  subtyping and overloading).
- It is flexible and easy to expand in the future.
- We can easily build a more Enso-feeling interface on top of it.

By way of illustrative example, Java supports method overloading and subtyping,
two things which have no real equivalent in the Enso type system.

### Finding Polyglot Bindings

Polyglot objects for various languages are found in the `polyglot` subdirectory
of an Enso project. This folder is subdivided into directories based on the
polyglot language. The name of each subdirectory must match the language
identifier used in the source code.

Inside each directory is an implementation-defined structure, with the polyglot
implementation for that particular language needing to specify it. Please see
the language-specific documentation for details.

## `foreign` functions

It is possible to define new code snippets of foreign languages directly in
`.enso` source files using _"Embedded Syntax"_. Such a handy support provides a
truly smooth user experience. A `foreign` function block is introduced as
follows:

- The `foreign` keyword starts a block.
- This must be followed by a language identifier (e.g. `python` or `js`).
- After the language identifier, the remaining syntax behaves like it is an Enso
  function definition until the `=`.
- After the `=`, the user may write their foreign code as a string.

```ruby
foreign python concat a b = """
  def join(a, b):
    str(a) + str(b)
  join
```

In the above example, this defines an Enso function `concat` that takes two
arguments `a` and `b`, implemented in Python.

- [**Python:**](./python.md) Details on Python polyglot bindings.

Similar syntax can be used for `js` and other dynamic languages. Certain
languages require/have special support, but in general this mechanism is reusing
polyglot capabilities of GraalVM Truffle framework and works with any language
that implements its `InteropLibrary` and _parse in a context_ protocols. s
