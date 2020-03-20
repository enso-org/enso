# Enso: The Semantics
Much like we have specifications for the [syntax](../syntax/syntax.md) and the
[type system](../types/types.md), we also need a document where we can specify
the executable semantics of portions of Enso.

This is that document, and it contains descriptions of key semantic portions of
Enso.

> The actionables for this section are:
>
> - As we make more semantic determinations about the language these should be
>   written down here.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Scoping](#scoping)
    - [Scoping Rules](#scoping-rules)
- [Strict Evaluation](#strict-evaluation)
    - [Optional Suspension](#optional-suspension)
- [Bindings](#bindings)

<!-- /MarkdownTOC -->

## Scoping
Enso's scoping rules should be fairly familiar to those coming from other
languages that are immutable (or make heavy use of immutability). In essence,
Enso is a lexically-scoped language where bindings may be shadowed in child
scopes.

- A name may not be redefined in the same scope that it was defined in.
- A name may be redefined in any child of that scope. This redefinition shadows
  the binding higher up the tree of scopes.

### Scoping Rules
The following constructs introduce new scopes in Enso:

- **Method Definitions:** A method definition introduces a new scope. These
  scopes are considered to be 'top-level' and hence have no parent other than
  the module scope. If the body of the method is a block or a function, the
  scope of the method definition should be _reused_ as the scope of the
  function or block.
- **Function Definitions:** A function definition introduces a new scope. This
  scope is either a child of the scope in which the function is defined, or is
  the scope of the method being defined. If the body of the function is a block,
  the function scope should be _reused_ as the block scope.
- **Blocks:** A block introduces a new scope. This scope is a child of the scope
  in which the block is defined, or is the scope of the method being defined.
- **Case Branches:** Each case branch introduces a new scope.
- **Function Call Arguments:** In order to handle suspension of arguments
  correctly, each argument used to call a function is evaluated in its own
  scope. This scope is a direct child of the scope in which the function call is
  taking place.

> The actionables for this section are:
>
> - Write this out in more detail when we have time. The above is only intended
>   as a brief summary.
> - NOTE: The note about case-branch scoping needs to be refined as the
>   implementation of `case` evolves.

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
