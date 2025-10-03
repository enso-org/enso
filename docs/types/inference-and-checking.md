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
  - [Overall Design](#overall-design)
  - [Overall structure of the implementation](#overall-structure-of-the-implementation)
  - [Future work](#future-work)
- [Design Goals](#design-goals)
  - [Maximal Inference Power](#maximal-inference-power)
  - [Type Inference Algorithm](#type-inference-algorithm)
  - [Type Checking Algorithm](#type-checking-algorithm)

<!-- /MarkdownTOC -->

## Type Checker Implementation

A prototype of a type checker has been developed that does not try to reach all
the design goals written below, but tries to perform best effort type checking
to provide lints and warnings to developers to aid in development.

As static type checking isn't needed for Enso execution, the type checker is
only enabled if an `--enable-static-analysis` flag has been passed to the
compiler.

To try out the type checker you may run

```bash
./built-distribution/enso-engine-0.0.0-dev-windows-amd64/enso-0.0.0-dev/bin/enso --compile /path/to/Project --enable-static-analysis
```

### Overall Design

#### What is being checked

Any issues found by the type checker are reported as compilation warnings. They
are not treated as errors for two reasons: firstly, because the type checker is
still a prototype and so it should not block programs in case there is a mistake
in its logic, and secondly, because the program can still run - if the given
problematic block of code that is guaranteed to error is not reached during
execution it may not cause problems.

Currently the type checker reports the following kinds of warnings:

- **type mismatch**, reported whenever during a method call the inferred type of
  an argument passed to the method does not satisfy the type expected by the
  function's signature, e.g. calling function defined as
  `function (x : Integer) = ...` with an argument of type text:
  `function "Txt"`,
- **no such method**, whenever a method is being called on an object with an
  inferred type and the type checker deduces that the inferred type does not
  have a method with the given name, e.g. when calling `x.method_1` and the type
  of `x` is not `Any` and does not define method called `method_1`,
- **not invokable**, whenever a non-function value is being called as a
  function - this often happens if _too many_ arguments have been passed to a
  method,
- **discarded value**, reported whenever a value, whose inferred type is a
  _Function_, is discarded; this usually means that not enough arguments were
  applied to a function.

The type mismatch error is **not** reported for a type-asserting expression,
because it can still be satisfied by a hidden intersection type. See
[the documentation about intersection types for more information](./intersection-types.md#narrowing-type-check).

#### "Best-effort" gradual typing

Enso has integrated some form of dynamic type checking by implementing the
argument type checks and type assertions which are checked at runtime. However,
non-negligible amount of code still depends on more 'dynamic' dispatch.
Moreover, Enso allows interoperability with external inherently
dynamically-typed languages like JavaScript or Python, so there are cases where
the types of values cannot really be known 'statically'.

To alleviate in this, the type inference and checking are implemented in a
gradual, best-effort, basis.

The type inference algorithm tries to infer the types wherever it is possible,
but it is designed to give up in a graceful way. Static type errors are only
reported if the type checker can 'prove' that a given operation **will** surely
fail at runtime (if the piece of code is reached). If the operation may fail or
succeed, no errors are reported.

#### Any is Special

This makes `Any` a special type. In terms of the subtyping relationship it is a
[top type](https://en.wikipedia.org/wiki/Top_type), however in terms of the type
checker it is a bit closer to the
[bottom type](https://en.wikipedia.org/wiki/Bottom_type) - because a value of
type `Any` can be of any particular type, that means no error will be reported
because there is no way to guarantee a failure at runtime. Arguments of type
`Any` can be passed to methods expecting all kinds of types (then, they can fail
at runtime, but there is no way to prove a guaranteed failure statically), and
all methods can be called on `Any` (not only those defined on `Any`, as the
actual value passed in can have the given method defined). It behaves similarly
to
[`any` type in TypeScript](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#any)
or [`dynamic` type in Kotlin](https://kotlinlang.org/docs/dynamic-type.html).
Because of that, the `Any` type can be used to represent unexpected return
values from polyglot calls.

#### Relation to Intersection Types

[Enso approach to intersection types](./intersection-types.md) allows for a
value to have multiple different types, some of which are hidden, adds some
challenges to the type checking. The current state (where the hidden part of the
type can only be uncovered via an explicit cast) provides good balance between
the ability to work with intersection types in the interactive GUI (that is
capable of inserting the necessary casts) and pass them through various checked
methods (which may hide the extra parts, but still being able to uncover them)
and the capabilities of static analysis of types. Good balance is namely
achieved because of:

- allowing `y = x:T`
  [narrowing type check](./intersection-types.md#narrowing-type-check) that
  check the type at run-time and allow to 'uncover' the hidden parts of the
  type. They are treated as an `instanceof` check, so they are not validated by
  static analysis but instead serve as _evidence_ that if the code continues
  execution, then indeed `y` must now have type `T`.
- all other places - _invoking methods_ on a type, passing a _value as an
  argument_, using a _value in binary operators_ rely only on the **visible**
  part of the type and thus can be checked in static analysis.
- uncovering hidden values without a check is only possible when explicitly
  requested by an [open type check](./intersection-types.md#open-type-check).
  When one uses `x : Integer&Any` check, it is no longer possible to report any
  type mismatch warnings. Type inference knows the value must conform to
  `Integer`, but it cannot rule out that `x` was not created as
  `Integer & Text`. Therefore the static analysis must assume that every value
  can have any combination of types _"in its `&Any` part"_.

Any changes to the related logic must be done very carefully as it is very easy
to introduce a change that will introduce a 'collapse' of the type system that
would make it unable to report any useful warnings.

#### Local Inference and Type Propagation

The type inference relies on existing type signatures and type assertions. Since
function argument types are checked at runtime, the type checker treats them as
assertions that an incoming value is of a given type. Similarly, code following
a type assertion inside an expression (`y = x : T`, or `(x:T).method`) relies on
the fact that the control flow only proceeds if that assertion was satisfied.

The processing is performed by traversing the IR of each method body from
terminal IR nodes upwards. First types of the leafs - literals or variables -
are inferred, and then based on their types, the type of more complex 'nodes'
(e.g. function application) is derived based on very basic inference rules that
stem from the
[simply typed lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus)
with some additional extensions for calling methods on objects and other
features of the language like pattern matching. Inside of blocks of code, the
inference is run line-by-line; whenever the assignment operator `=` creates a
new binding, the inferred type of that binding is saved in a mapping that can
then be used by subsequent lines of the block that can reference that binding.

Calling of methods on objects is implemented via special logic for application
on `UnresolvedSymbol`. That is because within the IR, the expression `a.f b c`
is actually translated to `UnresolvedSymbol<f> a b c`.

Similar desugaring is also applied to standalone methods and atom constructors:

- calling a standalone method defined in some module `method arg1 arg2` is
  translated into `UnresolvedSymbol<method> <internal-module-ref> arg1 arg2`,
  where `<internal-module-ref>` is a special variable that resolves to the
  module in which the `method` is defined,
- calling an atom constructor `Atom.Constructor arg1 arg2` becomes
  `UnresolvedSymbol<Constructor> Atom arg1 arg2`.

The most important rules for inference are then:

- a name is resolved using the `NameResolutionAlgorithm` to either a local
  binding (and then the type is read from the current `LocalBindingsTyping`), a
  global type/module reference, or an unresolved symbol - in which case it gets
  assigned a special `UnresolvedSymbol` type that is then used in application,
- an application of multiple arguments is broken into multiple single-argument
  applications and processed one-by-one,
- a single application `f x` is processed by looking up the types of `f` and
  `x`; four possibilities are considered:
  - `f` has a function type `A -> B` - then the type of `x` is checked for
    compatibility with `A` (this may yield **type mismatch** warnings) and the
    type of the application expression becomes `B`,
  - `f` has type `UnresolvedSymbol<f>` - then the method is looked up (using
    `MethodTypeResolver`) in the scope associated with the type of `x`; the type
    of such application expression is the full type of the referenced method,
    - If the method is not found, a **no such method** warning is reported.
  - `f` has type `Any` - then the type of the application expression becomes
    `Any`, as the type of `f` is not known,
  - `f` has some non-functional type - then a **not invokable** warning is
    reported.
- a binding `x = e` is processed by inferring the type of `e` and storing it in
  the current `LocalBindingsTyping` under the name `x`,
- a block of expressions is processed by recursively inferring each expression
  in the block (later expressions see updated bindings from processing earlier
  ones) and finally returning the type of the last one,
  - If any expression in the block apart from the last one is discarded, and it
    represents a function type, a **discarded value** warning is reported.
- a lambda expression `(a:A)-> b-> e` gets its type by first adding the types of
  arguments based on their ascriptions to the `LocalBindingsTyping` (an argument
  with no ascription is treated as unknown - `Any`) and then inferring the type
  of `e` in context that includes these additional bindings; then the type of
  the whole expression is built by combining the types of the arguments with the
  return type,
- a singleton or vector literal gets the type based on its value - it can be one
  of `Text`, `Integer`, `Float` or `Vector`,
- when processing a `case of` expression, similarly to a lambda, the bindings
  introduced by the patterns in each case are added to the `LocalBindingsTyping`
  for each case's expression; the type of the whole expression is built by
  computing the union of types inferred for each of the branches.

These rules are implemented in `TypePropagation::tryInferringType` and its
related helper methods.

### Overall structure of the implementation

The implementation consists of three phases:

1. `TypeInferenceSignatures` analyzes the IR of method definitions and
   associates a type signature with each method based on its type ascriptions.
   If a method has no type ascriptions, the argument types and the return type
   will default to unknown type (`Any`) but the method will still get a
   signature at least indicating the _arity_ of the method.
2. `StaticModuleScopeAnalysis` builds a static counterpart of `ModuleScope` that
   holds method definitions available in scopes of each module. The static
   module scope is then used to resolve methods on types during the static
   analysis, in the same way as `ModuleScope` is used at runtime for method
   dispatch. i. The `BuildScopeFromModuleAlgorithm` and
   `MethodResolutionAlgorithm` encapsulate the core logic, so that the same
   logic is guaranteed to be used both at runtime and compile-time.
3. `TypeInferencePropagation` which analyzes each method, tries to infer types
   of each sub-expression and report any issues found. While a type is inferred
   for every sub-expression inside a method body, the types are stored in
   metadata **only for the named bindings** to conserve memory. The rationale is
   mostly that the types of named bindings are worth storing as in the future
   they could be used for features such as auto-complete.

More information can be found in the documentation of the relevant classes.

### Future work

#### Improving the current prototype

Currently, the algorithm has several places that simply were not yet finished:

- improving the type matching algorithm (responsible for type mismatch warnings)
  to work with sum types and intersection types (currently it bails out),
- improving the handling of method arguments that have default values and
  calling method arguments by name:
  - This requires extending the `TypeRepresentation.ArrowType` to hold the name
    of each argument and a flag indicating if it has a default value; then the
    logic of `CurryNode` and its relatives needs to be ported to the compiler to
    support the argument reordering, deciding when to use default arguments
    during the method call and when not to do it (e.g. not all arguments are
    provided, or the `...` operator is used to explicitly stop the default
    application).
- checking for `Private_Access` violations statically.
  - Currently, the `StaticModuleScopeAnalysis` pass does not record if a given
    method or constructor (or the whole module) is marked as `private`. This
    property should be recorded, and if a private method is called from an
    outside module, a warning about the private access could be reported also
    statically.

#### Integration with the VS Code extension

Integrating the type checker with the VS Code extension for Enso can lead to
vastly improved developer experience. The inferred types of bindings can be used
to offer some form of method autocomplete and warnings could be displayed inline
inside the code editor.

#### Polymorphic types, or a single type: `Vector a`

Enso's type system is designed to allow for polymorphic types. While a full
implementation may be quite complicated to implement efficiently, doing that for
a specific case of `Vector` can be a good first step as it allows to use some
'tricks' to 'cheat' and get the desired result for relatively cheap.

It has been suggested that, upon construction, a `Vector` could compute the most
general type of its elements and store it inside its metadata. Then in runtime,
checking if a value `v` satisfies the type `Vector Integer` would merely require
checking if `v` is a `Vector`, reading its metadata to see the type the elements
satisfy and checking if that type is a subtype of `Integer`. This is much
cheaper than having to iterate over all elements of the vector.

There are still some challenges to overcome:

- what to do with vectors that come from other languages (e.g. arrays coming
  from Java),
- ensuring that the additional step of computing the most general type upon
  construction does not significantly impact the performance.

#### More powerful inference

The current approach for inference is very simple, but can already provide basic
checking helpful during development. If types of method arguments are known and
method definitions on types have checked signatures, then most expressions can
be inferred and the type checker can provide warnings in case the inferred types
do not match the expectations, allowing the developer to find bugs before
running the program.

However, the inference algorithm has been created with simplicity and checking
for program correctness in mind, so the inference does not work with ambiguity.
If function arguments do not have checked types, they are treated as `Any` and
very little information can be derived from them. Currently, the algorithm does
not try to do bidirectional inference that would try to guess what the types of
the function 'should be' based on how they are used.

In the future, one could try to implement more powerful inference that treats
un-annotated function arguments as type-variables and tries to propagate them
through the data flow, recording any constraints induced by method calls. Then
trying to infer what the type of the argument 'should' be to satisfy the
gathered constraints (the simplest form of these constraints stemming from
checking argument types is unification of type variables, but calling methods on
objects may give raise to other kinds of constraints (ones like 'type that has
method `f` defined on it with arity N')). In such case, a distinction may need
to be made between function arguments that are 'checked' and ones that have an
inferred type, but it is not checked at runtime. It is also unclear how the
unorthodox approaches like Enso's approach to intersection types would play with
solving these kinds of constraints.

Yet another big area for improvement is allowing for type polymorphism in
methods - being able to change the signature of `map` from
`Vector -> (Any -> Any) -> Vector` into `Vector a -> (a -> b) -> Vector b` would
allow for much more powerful type inference.

Such improvements in type inference may require rather fundamental changes from
the current relatively simple 'propagation' algorithm.

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
