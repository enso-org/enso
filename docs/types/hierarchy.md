---
layout: developer-doc
title: The Enso Type Hierarchy
category: types
tags: [types, hierarchy, typeset, atom]
order: 2
---

# The Enso Type Hierarchy

Enso is a statically typed language based upon a theory of set-based typing,
what we call `typesets`. This is a novel approach, and it is key to our intent
for Enso to _feel_ like a dynamic language while still bringing enhanced safety.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Typeset Theory](#typeset-theory)
- [Atoms](#atoms)
- [Typesets](#typesets)
  - [Typeset Operators](#typeset-operators)
  - [Typeset Subsumption](#typeset-subsumption)
  - [Unsafe Typeset Field Mutation](#unsafe-typeset-field-mutation)
- [Dependent Lists](#dependent-lists)
- [Interfaces](#interfaces)
  - [Special Interfaces](#special-interfaces)
- [Type Ascription](#type-ascription)
  - [Scoping in Type Ascription](#scoping-in-type-ascription)
- [Projections](#projections)
  - [Special Fields](#special-fields)

<!-- /MarkdownTOC -->

## Typeset Theory

- All types are denoted by a set of constructors, which represent the atomic
  values of that type. We call these 'atoms'. For example, the typeset `Nat` is
  made up of the atoms `1, 2, 3, ...` and so on.
- Constructors are grouped into typesets.
- These typesets are arranged into a modular lattice:
  - The type `Any` is the typeset of all typesets.
  - The type `Void` is the empty typeset.
  - All atoms are typesets, but not all typesets are atoms.
  - This lattice is ordered using the `<:` subsumption judgement. For more
    information please see [typeset subsumption](#typeset-subsumption).

All in all, this means that a value in Enso can have myriad different types
attributed to it, even though these may vary greatly in the level of
specificity.

```ruby
7 : 7 : Natural : Integer : Number : Any : Any : ...
```

A brief note on naming (for more, please see the
[naming syntax](../syntax/naming.md)):

- Naming in Enso is case-insensitive.
- In contexts where it is ambiguous as to whether the user would be referring to
  a fresh name or a name already in scope, capitalisation is used to determine
  which is meant.
- An uncapitalised identifier is assumed to be fresh, while a capitalised
  identifier is assumed to be in scope.

## Atoms

Atoms are the fundamental building blocks of types in Enso, so named because
they are small units of 'type', but nonetheless may be separated further. In
Enso's type hierarchy, atoms are the typesets that are unified _nominally_,
meaning that every atom has a discrete identity.

- Atoms can be thought of as the 'values' of Enso's type system. Formally an
  atom is a product type with named fields, where the fields are polymorphic.
- Atoms have _unique identity_. This means that while a typeset may subsume an
  atom, an atom can never be subsumed by another atom, even if it has the same
  number of fields with the same names.
- An atom, thus, can only unify with a site expecting that atom, or some
  anonymous typeset.

The following are all examples of atoms with a usage example:

```ruby
type Nothing
type Just value
atom Vec3 x y z
atom Vec2 x y

v = V3 1 2 3 : V3 1 2 3 : V3 Int Int Int : V3 Any Any Any : Any
```

## Typesets

Typesets in Enso are an entity unique to Enso's type system. They are a
fundamental recognition of types as 'sets of values' in Enso, and while they
share some similarities with records they are a far more general notion.

- A typeset is an entity that contains one or more labels.
- Each label has a type, which _may_ be explicitly ascribed to it.
- Each label may have a value provided for it.

The other key notion of typesets is that typesets are matched _structurally_,
subject to the rules for nominal typing of atoms discussed above.

- Typeset members are themselves typesets.
- A typeset member _must_ have a label, but may also have a type and a value
  (`label : Type := value`)
- An unspecified type is considered to be a free type variable.
- The label and the type become part of the typing judgement where present, and
  will be used for unification and subsumption.

Typesets themselves can be declared in two ways:

1.  **Anonymous:** An anonymous typeset can be declared using the curly-braces
    syntax `{}`. Such a definition must contain zero or more typeset fields (see
    above). Please note that `{}` is necessary as it needs to delimit a pattern
    context.
2.  **Atoms:** An atom definition declares a typeset with a discrete identity,
    using atom definition syntax. Atom fields must only be names.
3.  **Concatenation:** Combining two typeset values using `;`.

Typesets can be combined using the [typeset operators](#typeset-operators)
defined below.

In addition, we provide syntactic sugar for defining typesets as described in
the syntax document. This syntax has a special desugaring that obeys the
following rules:

```ruby
type Maybe a
    Nothing
    type Just (value : a)

    isJust = case this of
        Nothing -> False
        Just _ -> True

    nothing = not isJust
```

Becomes

```ruby
atom Just value
maybe a =
  { (Nothing | Just a), isJust: IsJust = isJust, nothing : Nothing = nothing }

Nothing.isJust : Maybe a -> Bool
Nothing.isJust this = case this of
  Nothing -> False
  Just _ -> True

Just.isJust : Maybe a -> Bool
Just.isJust this = case this of
  Nothing -> False
  Just _ -> True

Nothing.nothing : Maybe a -> Bool
Nothing.nothing = not isJust

Just.nothing : Maybe a -> Bool
Just.nothing = not isJust
```

> The actionables for this section are as follows:
>
> - Simplify this definition if we decide _not_ to support multiple dispatch.
> - Determine how duplicate labels should work, and if it should work.
> - Note that because things desugar to functions we can place arbitrary
>   constraints on initialisation (partial type constructors style).

### Typeset Operators

Enso defines a set of operations on typesets that can be used to combine and
manipulate them. Any use of these operators introduces typing evidence which may
later be discharged through pattern matching.

They are as follows:

- **Type Ascription - `:`:** This operator ascribes the type given by its right
  operand to the expression of its left operand.
- **Context Ascription - `in`:** This operator ascribes the monadic context
  given by its right operand to the expression of its left operand.
- **Error Ascription - `!`:** This operator combines the type of its left
  operand with the error type of its right. This is _effectively_ an `Either`,
  but with `Left` and `Right` reversed, and integrates with the inbuilt
  mechanism for [broken values](../semantics/errors.md#broken-values).
- **Function - `->`:** This operator defines a mapping from one expression to
  another.
- **Subsumption - `<:`:** This operator asserts that the left hand operand is
  _structurally subsumed_ by the right-hand operand. For more details on this
  relationship, see [typeset subsumption](#typeset-subsumption) below.
- **Equality - `~`:** This operator asserts that the left and right operands are
  structurally equal.
- **Concatenation - `;`:** This operator combines multiple typesets, expressing
  product types.
- **Union - `|`:** This operator creates a typeset that contains the members in
  the union of its operands.
- **Intersection - `|`:** This operator creates a typeset that contains the
  members in the intersection of its operands.
- **Subtraction - `\`:** This operator creates a typeset that contains all of
  the members in the left operand that do not occur in the right operand.

For information on the syntactic usage of these operators, please see the
section on [type operators](#../syntax/types.md#type-operators) in the syntax
design documentation.

> The actionables for this section are:
>
> - When necessary, we need to _explicitly formalise_ the semantics of all of
>   these operators.
> - When do applications of these constructors create matchable (injective and
>   generative) types?
> - Are `<:` and `:` equivalent in the surface syntax?

### Typeset Subsumption

For two typesets `a` and `b`, `a` is said to be subsumed by `b` (written using
the notation `a <: b`) if the following hold recursively. This can be thought of
as a 'can behave as' relation.

1.  `a` contains a subset of the labels in `b`. It should be noted that `a` is
    not _limited to_ being a subset of the labels in `b`.
2.  For each label in `a`, the type of that label `t` is subsumed by the type
    `q` of the corresponding label in `b`. That is, `t <: q`, defined as
    follows:

    1.  If both `t` and `q` are atoms, then it holds only if `t` and `q` are the
        same atom (have the same identity).
    2.  If `t` is an atom, then it holds only if the fields in `t` are subsumed
        by `q`.
    3.  If either `t` or `q` is a function type but not _both_ `t` and q are
        function types, then the relation does not hold.
    4.  If both `t` and `q` are function types, then the relation holds if:

        - If `t` contains defaulted arguments, not present in `q`, then these
          can be ignored for the purposes of determining whether `t <: q`. For
          example, `f : a -> b = x -> c` is subsumed by `f : a -> c`.
        - For the _argument_ position of both `t` and `q`, `t.arg <: q.arg` (the
          argument position is covariant).
        - For the _return_ position of both `t` and `q`, if it is not a function
          type, then `t.ret <: q.ret` (the return position is covariant). If it
          is a function type then recurse.

    5.  If the types have constraints then the constraints must match. A
        constraint is simply an application of the `<:` relation.
    6.  The types both have the same relevance and visibility (in the dependent
        sense).

3.  For any typeset `a`, the relation `a <: Any` always holds, and the converse
    `Any <: a` never holds.
4.  For any typeset `a`, the relation `a <: Void` never holds, and the converse
    `Void <: a` always holds.

Two typesets `A` and `B` are defined to be structurally equal if `A <: B` and
`B <: A`.

> The actionables for this section are as follows:
>
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

### Unsafe Typeset Field Mutation

For performance it is sometimes necessary to have the ability to _directly_
_mutate_ the field of a typeset. This is most often necessary for atoms
themselves, but as atoms are typesets this also applies.

- We define a method `setField : (field : Name) -> (value : Any) -> Nothing`
  that performs in-place field mutation of the field `field` to set its value to
  `any`.
- In order to prevent this from being used flippantly, this functionality is
  marked `unsafe` (see [access modifiers](./access-modifiers.md) for more).

## Dependent Lists

The most-specific type of an Enso list is the list of the types of the list's
elements. By way of example, the following are true:

```ruby
[1, 1.2, "String"] : List [Integer, Number, String] : List [Number, Number, String] : ...
[1, 1.2, 3.0f] : List [Integer, Number, Double] : List [Number, Real, Real] : List Number : ...
```

This means that Enso's lists _fully subsume_ the use cases for standard tuples.

## Interfaces

Because typesets can be matched _structurally_, all typesets implicitly define
interfaces. A type `t` conforming to an interface `i` in Enso is as simple as
the relation `i <: t` (as in [typeset subsumption](#typeset-subsumption))
holding.

This means that types need not _explicitly_ implement interfaces, which can be
thought of as a form of static duck typing. However, when defining a new type,
users may choose to explicitly state that it defines an interface. This has two
main benefits:

- We can include default implementations from the interface definition.
- We can provide better diagnostics in the compiler as we can point to the
  definition site instead of the use site.

```ruby
type HasName
    name: String
    name = "unnamed"

type Vector a
    this: HasName
    V2 x:a y:a
    V3 x:a y:a z:a

name (this:Int) = "IntName"

greet (t:HasName) = print 'Hi, my name is `t.name`'

main =
    greet (V3 1 2 3)
    greet 8
```

As an aside, it should be noted that the nature of Enso's typesets means that it
is easy to express far more general interfaces than Haskell's typeclasses can.

### Special Interfaces

In order to aid usability we include a few special interfaces in the standard
library that have special support in the compiler.

#### Wrapper

In a language where composition is queen and inheritance doesn't exist there
needs to be an easy way for users to compose typesets without having to define
wrappers for the contained types. This is a big usability bonus for Enso.

```ruby
type Wrapper
    wrapped   : (lens s t a b) this.unwrapped
    unwrapped : t
    unwrapped = t # Default implementation based on inferred type.
```

`Wrapper` is an interface implemented implicitly for all typesets, and boils
down to delegating to the contained members if a given label is not found on the
top typeset. This delegation only occurs on the this type.

A usage example is as follows:

```ruby
type HasName a
    this:Wrapper # The field 'unwrapped' uses default implementation.
    type Cons
        name    : String
        wrapped : a

test i:Int = i + 1

main =
    p1 = HasName.Cons "Zenek" 7
    p2 = p1 + 1     # OK, uses `wrapped` lens.
    print $ p2.name # OK
    print $ test p1 # OK, uses `wrapped` lens.
```

#### Convertible

Also useful in a language for data science is the ability to have the compiler
help you by automatically converting between types that have sensible coercions
between them. This interface is known as `Convertible`, and defines a one-way
conversion between a type `a` and a type `b`.

```ruby
Convertible t
  to : t -> t
```

There are a few key points of this design that must be considered carefully:

- This interface _only_ applies when implemented explicitly by the type. The
  compiler will not automatically generate implementations for `Convertible t`.
- It is very important for the conversions to be inserted automatically in the
  correct place. If a conversion is required in the body of a block, the point
  at which the conversion takes place should propagate outwards as far as
  possible. This is very important for proper definition of controls in the GUI.
- `Convertible t` can also be implemented by a function that accepts arguments
  _iff_ all of the arguments have default values associated with them. In such a
  case, the GUI should display conversion controls with a checkbox that, when
  checked, can be converted to an explicit conversion call.
- We will need some limited mechanism for doing this even without type inference
  as it forms the backbone of good API design for the graphical interface. This
  is because polymorphic functions are much harder to support with graphical
  controls.

An example use-case is as follows:

```ruby
type Vector a
    type V3 x:a y:a z:a

    this : Convertible String
    to = 'V3 `this.x` `this.y` `this.z`'

    this : Convertible (a: Semigroup)
    to = this.x + this.y + this.z

test: Int -> String
test i = print 'I got the magic value `i`!'

main =
    test 7    # OK!
    test 'hi' # FAIL: type mismatch, no definition `Convertible Int` for String.
    test (Vector.V3 1 2 3) # OK, prints 'I got the magic value 6'.
```

> The actionables for this section are:
>
> - Work out the semantics for working with this only for `this` in the dynamic
>   case.
> - Work out how best the type system can expose the conversion so that you can
>   see the additional defaulted parameters and conversion types in the UI.

#### Destruct

While it is a common idiom in functional languages to implement the `bracket`
pattern for acquiring and releasing resources, but this isn't such a good fit
for a language where many users aren't going to be used to thinking about
resources.

Instead, we have the final of our special traits, called `Destruct`, defined as
follows:

```ruby
type Destruct
  destroy : This -> Nothing
```

For those coming from Rust, C++, or another language which uses the RAII
principle, this should be a familiar sight. It works as follows:

1.  All types automatically provide a trivial implementation for `destroy`. This
    will recursively call the destructors of the type's members.
2.  A type definition may provide a non-default implementation for `destroy`,
    such that it implements more complex behaviour.
3.  When a type goes out of scope, its `destroy` method is called, allowing it
    to clean up any resources that it owns.

Initially, going out of scope will be defined as the point at which the instance
is garbage collected, while later, once we are able to perform more
sophisticated analysis, it will instead be defined as the point at which the
instance's lexical lifetime ends.

It should be noted, however, that a type that implements an explicit `destroy`
method should still implement explicit methods for resource handling as lexical
lifetimes are not always sufficient (e.g. a socket that you may want to close
and re-open in the same block).

> The actionables for this section are:
>
> - Determine how this interacts with copying and moving.

## Type Ascription

Like all statically-typed programming languages, Enso provides the means for the
user to ascribe a type to a value. In Enso, this is done using the `:` operator.
An expression `a : b` says that the expression denoted by `a` has the type
denoted by the expression `b`. However, unlike in many programming languages,
the types ascribed to values in Enso are not the be-all and end-all.

- The Enso compiler is free to infer a _more specific_ type than the one
  provided in a type signature. If this is the case, then the actual type of the
  value is the inferred type rather than the provided type.

  ```ruby
  a = 17 : Int | Text
  b = a + 1 # This is not an error
  ```

- If the Enso compiler would infer a _more general_ type than the one provided
  in a type signature, then the signature _constrains_ the allowable type of the
  value.
- Type signatures must be subsumed by the inferred type of the value, otherwise
  the compiler will raise an error. This includes

### Scoping in Type Ascription

Enso intends to support some form of mutual scoping between the left and right
sides of the type ascription operator. This introduces some complexity into the
typechecker but brings some significant benefits.

- It is common in dependently-typed languages for the signature to balloon
  significantly as you add more constraints to your program.
- To this end, Enso wants to support a sharing of scopes between the top-level
  scope of the type signature's right-hand-side, and the top-level scope of the
  signature's left hand side.
- This will allow users to move complexity _out_ of the type signature and into
  the body of the expression, aiding code clarity.
- It does, however, introduce some significant complexity around recursive
  bindings in groups, and the desugaring needs to depend on combinations of
  `>>=` and `fix`.

## Projections

In order to work efficiently with typesets, we need the ability to seamlessly
access and modify (immutably) their properties. In the context of our type
theory, this functionality is known as a _projection_, in that it projects a
value from (or into) a typeset.

Coming from Haskell, we are well-versed with the flexibility of lenses, and more
generally _optics_. To that end, we base our projection operations on standard
theories of optics. While we _do_ need to formalise this, for now we provide
examples of the expected basic usage. This only covers lenses, while in the
future we will likely want prisms and other more-advanced optics.

A projection is generated for each field of a typeset.

> Actionables for this section:
>
> - Work out whether standard optics theory with custom types is sufficient for
>   us. We may want to support side effects.
> - Determine how much of the above we can support without a type-checker. There
>   are likely to be a lot of edge-cases, so it's important that we make sure we
>   know how to get as much of it working as possible.
> - How (if at all) do lenses differ for atoms and typesets?

### Special Fields

We also define special projections from typesets:

- `index`: The expression `t.n`, where `n` is of type `Number` is translated to
  `t.index n`.
- `field`: The expression `t.s` where `s` is of type `Text` is translated to
  `t.fieldByName s`.
