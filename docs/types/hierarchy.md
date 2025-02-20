---
layout: developer-doc
title: The Enso Type Hierarchy
category: types
tags: [types, hierarchy, typeset, atom]
order: 2
---

# The Enso Type Hierarchy

Enso is a dynamic language, yet _every object_ in the running system _has a
type_. Type defines the set of operations that can be performed on a value. The
most generic type is `Any`. If a value has no better (more specific) type, it
has the type `Any`. All operations defined on type `Any` can be performed on any
value in the system.

> [!WARNING] _Typeset theory is far from current state of affairs_:
>
> Enso is a statically typed language based upon a theory of set-based typing,
> what we call `typesets`. This is a novel approach, and it is key to our intent
> for Enso to _feel_ like a dynamic language while still bringing enhanced
> safety.
>
> ## Typeset Theory
>
> - All types are denoted by a set of constructors, which represent the atomic
>   values of that type. We call these 'atoms'. For example, the typeset `Nat`
>   is made up of the atoms `1, 2, 3, ...` and so on.
> - Constructors are grouped into typesets.
> - These typesets are arranged into a modular lattice:
> - The type `Any` is the typeset of all typesets.
> - The type `Void` is the empty typeset.
> - All atoms are typesets, but not all typesets are atoms.
> - This lattice is ordered using the `<:` subsumption judgement. For more
>   information please see [typeset subsumption](#typeset-subsumption).

A value in Enso can have multiple different types attributed to it. It is
possible to query/inspect these types during runtime and thus decide what
operations are available for a particular value at hand.

> [!WARNING] > _Probably not true in current system at all_
>
> A brief note on naming (for more, please see the
> [naming syntax](../syntax/naming.md)):
>
> - Naming in Enso is case-insensitive.
> - In contexts where it is ambiguous as to whether the user would be referring
>   to a fresh name or a name already in scope, capitalisation is used to
>   determine which is meant.
> - An uncapitalised identifier is assumed to be fresh, while a capitalised
>   identifier is assumed to be in scope.

## Atoms

Atoms are the fundamental building blocks of types in Enso, so named because
they are small units of 'type', but nonetheless may be separated further. When
defining a `type` in Enso, one can associate to it multiple _atom constructors_.
Such constructors are then used to create instances of the `type`.

- Atoms can be thought of as the 'values' of Enso's type system. Formally an
  atom is a product type with named fields, where the fields are polymorphic.
- Atoms have _unique identity_. This means an atom has a particular type,
  however an atom can never be a type of another atom.
- An atom, thus, can only unify with a site expecting that atom, or its type or
  the general type `Any`.

The following defines a type `Option` with two atoms `None` and `Some`:

```ruby
type Maybe a
    Nothing
    Just (value : a)

    isJust = case this of
        Nothing -> False
        Just _ -> True

    nothing = not isJust
v = Maybe.Just "Hi"

# value `v` has type `Maybe`:
v:Maybe

# `v.value` has type `Text`:
v.value:Text
```

> [!WARNING] There are no _Typesets_ in Enso anymore
>
> Typesets in Enso are an entity unique to Enso's type system. They are a
> fundamental recognition of types as 'sets of values' in Enso, and while they
> share some similarities with records they are a far more general notion.
>
> - A typeset is an entity that contains one or more labels.
> - Each label has a type, which _may_ be explicitly ascribed to it.
> - Each label may have a value provided for it.
>
> The other key notion of typesets is that typesets are matched _structurally_,
> subject to the rules for nominal typing of atoms discussed above.
>
> - Typeset members are themselves typesets.
> - A typeset member _must_ have a label, but may also have a type and a value
>   (`label : Type := value`)
> - An unspecified type is considered to be a free type variable.
> - The label and the type become part of the typing judgement where present,
>   and will be used for unification and subsumption.
>
> Typesets themselves can be declared in two ways:
>
> 1.  **Anonymous:** An anonymous typeset can be declared using the curly-braces
>     syntax `{}`. Such a definition must contain zero or more typeset fields
>     (see above). Please note that `{}` is necessary as it needs to delimit a
>     pattern context.
> 2.  **Atoms:** An atom definition declares a typeset with a discrete identity,
>     using atom definition syntax. Atom fields must only be names.
> 3.  **Concatenation:** Combining two typeset values using `;`.

Types can be combined using the [typeset operators](#typeset-operators) defined
below.

### Typeset Operators

Enso defines a set of operations on types that can be used to combine and
manipulate them. Any use of these operators introduces typing evidence which may
later be discharged through pattern matching.

They are as follows:

- **Type Ascription - `:`:** This operator ascribes the type given by its right
  operand to the expression of its left operand.
- **Error Ascription - `!`:** This operator combines the type of its left
  operand with the error type of its right. This is _effectively_ an `Either`,
  but with `Left` and `Right` reversed, and integrates with the inbuilt
  mechanism for [broken values](../semantics/errors.md#broken-values).
- **Context Ascription - `in`:** This operator ascribes the monadic context
  given by its right operand to the expression of its left operand.
- **Function - `->`:** This operator defines a mapping from one expression to
  another.
- **Union - `|`:** This operator creates a typeset that contains the members in
  the union of its operands.
- **Intersection - `&`:** This operator creates a typeset that contains the
  members in the [intersection of its operands](./intersection-types.md).

> [!WARNING] These operators _don't seem to be supported_. There is no plan to
> support following operators now:
>
> - **Subsumption - `<:`:** This operator asserts that the left hand operand is
>   _structurally subsumed_ by the right-hand operand. For more details on this
>   relationship, see [typeset subsumption](#typeset-subsumption) below.
> - **Equality - `~`:** This operator asserts that the left and right operands
>   are structurally equal.
> - **Concatenation - `;`:** This operator combines multiple typesets,
>   expressing product types.

For information on the syntactic usage of these operators, please see the
section on [type operators](#../syntax/types.md#type-operators) in the syntax
design documentation.

> [!NOTE] The actionables for this section are:
>
> - When necessary, we need to _explicitly formalise_ the semantics of all of
>   these operators.
> - When do applications of these constructors create matchable (injective and
>   generative) types?
> - Are `<:` and `:` equivalent in the surface syntax?

> [!WARNING] > _Typeset Subsumption_ isn't relevant
>
> For two typesets `a` and `b`, `a` is said to be subsumed by `b` (written using
> the notation `a <: b`) if the following hold recursively. This can be thought
> of as a 'can behave as' relation.
>
> 1.  `a` contains a subset of the labels in `b`. It should be noted that `a` is
>     not _limited to_ being a subset of the labels in `b`.
> 2.  For each label in `a`, the type of that label `t` is subsumed by the type
>     `q` of the corresponding label in `b`. That is, `t <: q`, defined as
>     follows:
>
>     1.  If both `t` and `q` are atoms, then it holds only if `t` and `q` are
>         the same atom (have the same identity).
>     2.  If `t` is an atom, then it holds only if the fields in `t` are
>         subsumed by `q`.
>     3.  If either `t` or `q` is a function type but not _both_ `t` and q are
>         function types, then the relation does not hold.
>     4.  If both `t` and `q` are function types, then the relation holds if:
>
>         - If `t` contains defaulted arguments, not present in `q`, then these
>           can be ignored for the purposes of determining whether `t <: q`. For
>           example, `f : a -> b = x -> c` is subsumed by `f : a -> c`.
>         - For the _argument_ position of both `t` and `q`, `t.arg <: q.arg`
>           (the argument position is covariant).
>         - For the _return_ position of both `t` and `q`, if it is not a
>           function type, then `t.ret <: q.ret` (the return position is
>           covariant). If it is a function type then recurse.
>
>     5.  If the types have constraints then the constraints must match. A
>         constraint is simply an application of the `<:` relation.
>     6.  The types both have the same relevance and visibility (in the
>         dependent sense).
>
> 3.  For any typeset `a`, the relation `a <: Any` always holds, and the
>     converse `Any <: a` never holds.
> 4.  For any typeset `a`, the relation `a <: Void` never holds, and the
>     converse `Void <: a` always holds.
>
> Two typesets `A` and `B` are defined to be structurally equal if `A <: B` and
> `B <: A`.

> The actionables for this section are as follows:
>
> - Just _delete it_!?
> - Fix the above. It isn't 100% correct, but should convey a general gist. Use
>   examples including all the operators.
> - Ensure that co- and contra-variance are handled properly. They are a bit odd
>   under this theory.
> - Do we need explicit variance annotations?
> - How do constraints factor in?
> - We want users not to have to think about the difference between `~`, `:` and
>   `<:` so we need to work out if we can elide them from the surface language.
>   This requires considering polymorphic function arguments, partial data,
>   qualified function types and variable definitions.
> - Reformulate this in terms of row polymorphism. We really want to avoid a
>   _real_ subtyping relationship as it doesn't play at all well with global
>   inference.
> - To that end, it is an open question as to whether we can have type unions
>   without subtyping. Conventionally we wouldn't be able to, but with our
>   theory we may.

### Field Mutation

For performance it is sometimes necessary to have the ability to _directly_
_mutate_ the field of an atom. This is possible using `Meta.atom_with_hole`.
Such a mutation can happen _only once_.

## Interfaces

Historically interfaces used to be defined by _duck typing_. As Enso is a
dynamic language, having two types with the same operations means they can be
used interchangingly.

> [!NOTE] A work on
> [type classes](https://github.com/orgs/enso-org/discussions/11366) support is
> under way

> [!WARNING] _Doesn't match reality:_
>
> Because typesets can be matched _structurally_, all typesets implicitly define
> interfaces. A type `t` conforming to an interface `i` in Enso is as simple as
> the relation `i <: t` (as in [typeset subsumption](#typeset-subsumption))
> holding.
>
> This means that types need not _explicitly_ implement interfaces, which can be
> thought of as a form of static duck typing. However, when defining a new type,
> users may choose to explicitly state that it defines an interface. This has
> two main benefits:
>
> - We can include default implementations from the interface definition.
> - We can provide better diagnostics in the compiler as we can point to the
>   definition site instead of the use site.
>
> ```ruby
> type HasName
>     name: String
>     name = "unnamed"
>
> type Vector a
>     this: HasName
>     V2 x:a y:a
>     V3 x:a y:a z:a
>
> name (this:Int) = "IntName"
>
> greet (t:HasName) = print 'Hi, my name is `t.name`'
>
> main =
>     greet (V3 1 2 3)
>     greet 8
> ```
>
> As an aside, it should be noted that the nature of Enso's typesets means that
> it is easy to express far more general interfaces than Haskell's typeclasses
> can.

<!-- There are no Special Interfaces

In order to aid usability we include a few special interfaces in the standard
library that have special support in the compiler.

#### Destruct

While it is a common idiom in functional languages to implement the `bracket`
-->

To control lifecycle of a value use `Managed_Resource` and its support for
_finalizers_.

## Type Ascription

An expression `a : b` says that the expression denoted by `a` has the type
denoted by the expression `b`.

> [!WARNING] No support for Scoping in Type Ascription
>
> Enso intends to support some form of mutual scoping between the left and right
> sides of the type ascription operator. This introduces some complexity into
> the typechecker but brings some significant benefits.
>
> - It is common in dependently-typed languages for the signature to balloon
>   significantly as you add more constraints to your program.
> - To this end, Enso wants to support a sharing of scopes between the top-level
>   scope of the type signature's right-hand-side, and the top-level scope of
>   the signature's left hand side.
> - This will allow users to move complexity _out_ of the type signature and
>   into the body of the expression, aiding code clarity.
> - It does, however, introduce some significant complexity around recursive
>   bindings in groups, and the desugaring needs to depend on combinations of
>   `>>=` and `fix`.

> [!WARNING] There are _no projections_ right now and none are planned
>
> In order to work efficiently with typesets, we need the ability to seamlessly
> access and modify (immutably) their properties. In the context of our type
> theory, this functionality is known as a _projection_, in that it projects a
> value from (or into) a typeset.
>
> Coming from Haskell, we are well-versed with the flexibility of lenses, and
> more generally _optics_. To that end, we base our projection operations on
> standard theories of optics. While we _do_ need to formalise this, for now we
> provide examples of the expected basic usage. This only covers lenses, while
> in the future we will likely want prisms and other more-advanced optics.
>
> A projection is generated for each field of a typeset.
>
> Actionables for this section:
>
> - Work out whether standard optics theory with custom types is sufficient for
>   us. We may want to support side effects.
> - Determine how much of the above we can support without a type-checker. There
>   are likely to be a lot of edge-cases, so it's important that we make sure we
>   know how to get as much of it working as possible.
> - How (if at all) do lenses differ for atoms and typesets?
>
> ### Special Fields
>
> We also define special projections from typesets:
>
> - `index`: The expression `t.n`, where `n` is of type `Number` is translated
>   to `t.index n`.
> - `field`: The expression `t.s` where `s` is of type `Text` is translated to
>   `t.fieldByName s`.
