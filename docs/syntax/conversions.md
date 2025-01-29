---
layout: developer-doc
title: Defining Functions
category: syntax
tags: [syntax, functions]
order: 10
---

# Conversions

Conversions are special [functions](./functions.md) associated with a
[type](../types/hierarchy.md), named `from` and taking single `that` argument.
Following example:

```ruby
type Complex
    Num re:Float im:Float

Complex.from (that:Float) = Complex.Num that 0
```

defines type `Complex` and a **conversion** from `Float` which uses the provided
`Float` value as real part of a complex number while setting the imaginary part
to zero.

## Type Checks

Conversions are integral part of
[type checking](../types/inference-and-checking.md#type-checking-algorithm)
during runtime. Having the right conversion in scope one can write:

```ruby
complex_pi = 3.14:Complex
```

to create a new instance of type `Complex`. The Enso runtime represents the
`3.14` literal as `Float` value and that would fail the `Complex` type check if
there was no conversion method available. However as there is one, the runtime
uses `Complex.from` behind the scene and `complex_pi` then becomes
`Complex.Num 3.14 0` value.

Type checks may perform no, one or multiple conversions (like in case of
[intersection types](../types/intersection-types.md)).
