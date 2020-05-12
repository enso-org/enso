# Enso: The Semantics
Much like we have specifications for the
[syntax](../../syntax/specification/syntax.md) and the
[type system](../../types/design/types.md), we also need a document where we can
specify the executable semantics of portions of Enso.

This is that document, and it contains descriptions of key semantic portions of
Enso.

> The actionables for this section are:
>
> - As we make more semantic determinations about the language these should be
>   written down here.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Scoping](#scoping)
  - [Scoping Rules](#scoping-rules)
  - [Scoping of Type Signatures](#scoping-of-type-signatures)
- [Strict Evaluation](#strict-evaluation)
  - [Optional Suspension](#optional-suspension)
- [Bindings](#bindings)

<!-- /MarkdownTOC -->

## Scoping
Enso's scoping rules should be fairly familiar to those coming from other
languages that are immutable (or make heavy use of immutability). In essence,
Enso is a lexically-scoped language where bindings may be shadowed in child
scopes.

A scope is the span in the code within which a set of accessible identifiers
occurs. A nested scope may:

- Reference identifiers defined in parent scopes.
- Shadow identifiers from parent scopes with a new binding.
- Ascribing a type identifier to a binding outside the current scope.

Identifier visibility behaves as follows:

- Identifiers are bound by using a variable name in a pattern context (e.g. the
  LHS of a binding, a function argument, or a case expression pattern).
- Identifiers are accessible only _after_ they have been defined.
- Identifiers introduced into a given scope `s` are accessible in `s` and all
  the children of `s`, _after_ the point at which they are introduced.
- If a scope uses an identifier defined in an outer scope, and then later (in
  the thread of execution) shadows that variable, any usage before the shadowing
  point refers to the occurrence in the outer scope.

The term _accessible_ is defined to mean "can be referred to in the code as a
valid entity," and hence implies "can have its value used."

> The actionables for this section are:
>
> - In the future we may want to relax the forward-definition restriction for
>   pure bindings, allowing a form of recursive pure binding hoisting (like a
>   let block). This would use the monadic context's `fix` function.
> - Once we are capable of supporting `fix` and recursive pure bindings in
>   contexts, we need to revisit the above rules.

### Scoping Rules
The following constructs introduce new scopes in Enso:

- **Modules:** Each module (file) introduces a new scope.
- **The Function Arrow `(->)`:** The arrow operator introduces a new scope that
  is shared by both of its operands. This is true both when it is used for a
  lambda (value or type), and when used to denote case branches.
- **Code Blocks:** A code block introduces a new scope.
- **The Type Ascription Operator:** The type ascription operator introduces a
  new scope on its right hand side.

A new scope is _always_ introduced as a child of the scope in which the
introducing construct occurs, unless explicitly noted otherwise.

There are other linguistic constructs that _behave_ as if they introduce a
scope, but this is purely down to the fact that they desugar to one or more of
the above constructs:

- **Method Definitions:** A method definition introduces a new scope. This is
  simply because the method definition desugars to a lambda definition.
- **Function Definitions:** A function definition introduces a new scope. This
  is simply because the method definition desugars to a lambda definition.

> The actionables for this section are:
>
> - Write this out in more detail when we have time. The above is only intended
>   as a brief summary.
> - Decide if we want to support local overloads at all (differing in the type
>   of `this`). Local overloads are those that are not defined at the top level
>   of a module, and are currently unsupported.
> - We need to refine the specification for body-signature mutual scoping.
> - NOTE: The note about case-branch scoping needs to be refined as the
>   implementation of `case` evolves.

### Scoping of Type Signatures
Currently, type signatures in Enso obey a simple set of typing rules:

- The RHS of the type ascription introduces a new scope that is a child of the
  scope in which the LHS of the type ascription exists.

> The actionables for this section are:
>
> In order to enable much of the flexible metaprogramming ability that Enso aims
> for, we have an additional set of scoping rules for type signatures:
> - Both operands of the type ascription operator share a scope.
> - If two names are used on the type and term levels to refer to the same entity,
>   both are valid but this issues a warning. Referring to the same entity means
>   that they are two names for the same underlying object.
> - Name clashes are disallowed unless the clashing names refer to the same
>   entity.
> - Do we actually want to support this?
> - What complexities does this introduce wrt typechecking?

## Strict Evaluation
Though Enso shares many syntactic similarities with Haskell, the most famous
example of a lazily evaluated language, Enso is not lazy. Instead, Enso is a
language that is strict.

- Statements in Enso are evaluated as soon as they are bound to a name.
- This means that arguments to a function are always evaluated before the
  function is applied.
- Statements are _only_ evaluated when they contain fully-applied function
  applications. Otherwise they return curried functions.

> The actionables for this section are:
>
> - Make this far better specified.

### Optional Suspension
Laziness, however, can often be quite useful for defining certain kinds of API.
To that end, Enso provides support for optional laziness, more specifically
optional _suspension_, through the built-in `Suspended` type.

- When a type `a` is wrapped in a `Suspended`, it is turned into a thunk.
- A value of type `Suspended a` may be forced to execute the suspended
  computation and thereby obtain an `a`.

This forcing can take place in two ways:

- The user calls the standard library function `force : Suspended a -> a` on the
  value.
- Automatically, at a site where its evaluation is demanded. The algorithm for
  this is simple. If a value of type `Suspended a` is provided in a location
  that expects a value of type `a`, the compiler will insert an implicit call to
  `force` to produce the `a`.

> The actionables for this section are:
>
> - Make this far better specified.

## Bindings
While some expression-based languages with bindings have the binding return the
value assigned to the binding, we feel that this is far too error prone.
Consider the following code as a demonstration:

```ruby
if x = someExprEvaluatingToBool then foo else bar
```

This is the perennially-discussed C++ bug where you fail to type `==` in an
if-statement.

Enso, instead, takes the approach where a binding expression returns the
singleton value of the type `Nothing`, making the above-written code a type
error.
