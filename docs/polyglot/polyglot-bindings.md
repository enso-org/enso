---
layout: developer-doc
title: Polyglot Bindings
category: polyglot
tags: [polyglot, bindings, interop]
order: 1
---

# Polyglot Bindings

Enso provides [robust interoperability](./README.md) with other programming
languages. This document describes how users can connect Enso to other supported
programming languages to gain access to a wealth of libraries, as well as to
integrate Enso into existing systems.

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
they have `Any` type. As such there is no _static type checking_ when invoking
methods on such types and potential errors are only detected during runtime and
result in a runtime _failure_ (a typical behavior of Python or JavaScript
programs).

Enso implements a generic variadic syntax for calling polyglot functions using
vectors of arguments. In essence, this is necessary due to the significant
impedance mismatch between Enso's runtime semantics and the runtime semantics of
many of the polyglot languages. Such a solution:

- allows Enso to match up with a wide range of language semantics
  - for example Java's subtyping and overloading
- it is flexible and easy to expand in the future.
- allows building a more Enso-feeling interface on top of it.

Thanks to the generic variadic syntax, it is possible to smoothly invoke Java
overloaded and overriden methods.

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
truly smooth user experience:

```ruby
foreign python concat x y = """
    def join(a, b):
        return str(a) + str(b)
    return join(x, y)

main u="Hello" s=" " w="World!" =
    concat (concat u s) w
```

The previous example defines an Enso function `concat` that takes two arguments
`a` and `b`. The function is implemented in Python. The Python code defines a
local function `join` and uses it to compute and return result of `concat`. Then
the `concat` function is invoked from a `main` Enso function to concatenate
typical _Hello World!_ message.

- [**Python:**](./python.md) Details on Python polyglot bindings.

Similar syntax can be used for `js` and other dynamic languages. Certain
languages require/have special support, but in general this mechanism is reusing
polyglot capabilities of GraalVM Truffle framework and works with any language
that implements its `InteropLibrary` and _"parse in a context"_ protocols.

## Impedance Mismatch

Enso is designed as a functional programming language and as such it assumes
_mininal side effects_ when performing operation. Especially the _live
programming_ environment provided by the Enso visual editor relies on operations
being idempotent and having no side effects. Enso semantic enforces such _no
side effects_ behavior for programs written in Enso.

This is not a typical behavior of other programming languages and certainly it
is not enforced in languages like JavaScript, Python or Java. Polyglot
interoperation in Enso has a significant impedance mismatch. In essence, this
means that there is a mismatch between Enso's language semantics and the
semantics of the foreign languages that are being worked with.

Some of thes mismatches can be worked around by manually wrapping the foreign
constructs in Enso, however some just cannot. Care must be taken when dealing
with other languages and especially their side-effects.
