---
layout: developer-doc
title: Inference and Checking
category: types
tags: [types, inference, checking, compiler]
order: 13
---

# Inference and Checking

In spite of being dynamically-typed language, Enso is built with a sophisticated
type checker capable of reasoning about Enso typed system. However, a type
checker on its own is quite useless. For Enso to truly be usable, it must also
have a powerful type inference engine.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Type Checker Prototype](#type-checker-prototype)
- [Design Goals](#design-goals)
  - [Maximal Inference Power](#maximal-inference-power)
  - [Type Inference Algorithm](#type-inference-algorithm)
  - [Type Checking Algorithm](#type-checking-algorithm)

<!-- /MarkdownTOC -->

## Type Checker Prototype

A prototype of a type checker has been developed that does try to reach all 
the design goals written below, but tries to perform best effort type 
checking to provide lints and warnings to developers to aid in development.

As it is a prototype and may slow down the compilation, it is only enabled 
if an `--enable-static-analysis` flag has been passed to the compiler.

To try out the type checker you may run
```bash
./built-distribution/enso-engine-0.0.0-dev-windows-amd64/enso-0.0.0-dev/bin/enso --compile /path/to/Project --enable-static-analysis
```

### Overall Design

#### "Best-effort" gradual typing

Enso has integrated some form of dynamic type checking by implementing the argument type checks and type assertions which are checked at runtime. However, non-negligible amount of code still depends on more 'dynamic' dispatch. Moreover, Enso allows interoperability with external inherently dynamically-typed languages like Java Script or Python, so there are cases where the types of values cannot really be known 'statically'.

To alleviate in this, the type inference and checking are implemented in a gradual, best-effort, basis.

The type inference algorithm tries to infer the types wherever it is possible, but it is designed to give up in a graceful way. Static type errors are only reported if the type checker can 'prove' that a given operation **will** surely fail at runtime (if the piece of code is reached). If the operation may fail or succeed, no errors are reported.

This makes `Any` a special type. In terms of the subtyping relationship it is a [top type](https://en.wikipedia.org/wiki/Top_type), however in terms of the type checker it is a bit closer to the [bottom type](https://en.wikipedia.org/wiki/Bottom_type) - because a value of type `Any` can be of any particular type, that means no error will be reported because there is no way to guarantee a failure at runtime. Arguments of type `Any` can be passed to methods expecting all kinds of types (then, they can fail at runtime, but there is no way to prove a guaranteed failure statically), and all methods can be called on `Any` (not only those defined on `Any`, as the actual value passed in can have the given method defined). It behaves similarly to [`any` type in TypeScript](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#any) or [`dynamic` type in Kotlin](https://kotlinlang.org/docs/dynamic-type.html). Because of that, the `Any` type can be used to represent unexpected return values from polyglot calls.

#### Local Inference and Type Propagation

The type inference relies on existing type signatures and type assertions. Since function argument types are checked at runtime, the type checker treats them as assertions that an incoming value is of a given type. Similarly, code following a type assertion inside of an expression (`y = x : T`, or `(x:T).method`) relies on the fact that the control flow only proceeds if that assertion was satisfied.

The processing is performed by traversing the IR of each method body bottom-up. First we try to infer the types of the 'leafs' - literals or variables, and then based on their types, the type of more comples 'nodes' (e.g. function application).

### Overall structure of implementation

...

### Future work

TODO


## Design Goals

> [!WARNING]
>
> The actionables for this section are:
>
> - Work out how on earth we do inference and how we maximise inference power.
> - Do we want to provide a way to reason about the _runtime representation_ of
>   types? This is 'Levity Polymorphism' style.
> - We want error messages to be as informative as possible, and are willing to
>   retain significant extra algorithmic state in the typechecker to ensure that
>   they are. This means both _formatting_ and _useful information_.
> - It is going to be important to retain as much information as possible in
>   order to provide informative error messages. This means that the eventual
>   algorithm is likely to combine techniques from both W and M
>   (context-insensitive and context-sensitive respectively).

### Maximal Inference Power

In order to make Enso's type inference as helpful and friendly as possible to
our users, we want the ability to infer the _maximal subset_ of the types that
Enso can express.

> [!WARNING] The actionables for this section are:
>
> - How do we do inference for higher-rank and impredicative instantiations.
> - How do we infer contexts, and how do we make that inference granular (e.g.
>   `IO.Read`, `IO.Write`, rather than just `IO`).
> - How do we propagate inference information as far as possible?
> - If it comes to a tension between typechecker speed and inference capability,
>   Enso will err on the side of inference capability in order to promote ease
>   of use. Speed will be increased by performing incremental type-checking
>   where possible on subsequent changes.
> - Where are we okay requiring annotations? Polymorphic recursion, higher rank
>   function parameters, constrained data and dependency?

### Type Inference Algorithm

> [!WARNING] The actionables for this section are:
>
> - Specify the inference algorithm.

#### Inferring Dependency

> [!WARNING] The actionables for this section are:
>
> - Specify how (if at all) we can infer dependent quantifiers.

### Type Checking Algorithm

> [!WARNING] The actionables for this section are:
>
> - Specify the type checking algorithm.
