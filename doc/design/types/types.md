# Enso: The Type System
On the spectrum of programming type systems ranging from dynamically typed to
statically typed, one likes to think that there is a happy medium between the
two. A language that _feels_ dynamic, with high levels of type inference, but
lets the users add more type information as they want more safety and
compile-time checking.

Enso aims to be that language, providing a statically-typed language with a type
system that makes it feel dynamic. It will infer sensible types in many cases,
but as users move from exploratory pipelines to production systems, they are
able to add more and more type information to their programs, proving more and
more properties using the type system. This is based on a novel type-inference
engine, and a fusion of nominal and structural typing, combined with dependent
types.

All in all, the type system should stay out of the users' ways unless they make
a mistake, but give more experienced users the tools to build the programs that
they require.

This document contains discussion and designs for the type-system's behaviour,
as well as formal specifications where necessary. It discusses the impact of
many syntactic language features upon inference and type checking, and is
instrumental for ensuring that we build the right language.

#### A Note About This Document
In the aid of precision, this document will use syntax that _may not_ be exposed
to users. The appearance of a piece of syntax here that is not described in the
[syntax](../syntax/syntax.md) document makes no promises as to whether said
syntax will be accessible.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Goals for the Type System](#goals-for-the-type-system)
- [The Type Hierarchy](#the-type-hierarchy)
  - [Atoms](#atoms)
  - [Typesets](#typesets)
  - [Typeset Operators](#typeset-operators)
  - [Interfaces](#interfaces)
  - [Type Ascription](#type-ascription)
  - [Projections](#projections)
- [Function Types](#function-types)
  - [Scoping](#scoping)
  - [Structural Type Shorthand](#structural-type-shorthand)
  - [Function Composition](#function-composition)
- [Access Modificatiom](#access-modificatiom)
- [Pattern Matching](#pattern-matching)
- [Dynamic Dispatch](#dynamic-dispatch)
  - [Specificity](#specificity)
  - [Multiple Dispatch](#multiple-dispatch)
- [Modules](#modules)
  - [Scoping and Imports](#scoping-and-imports)
- [Monadic Contexts](#monadic-contexts)
  - [Context Definitions](#context-definitions)
  - [Context Lifting](#context-lifting)
- [Strictness and Suspension](#strictness-and-suspension)
- [Analysing Parallelism](#analysing-parallelism)
- [Typed Holes](#typed-holes)
- [Errors](#errors)
  - [Async Exceptions](#async-exceptions)
  - [Broken Values](#broken-values)
- [Dynamic](#dynamic)
  - [The Enso Boundary](#the-enso-boundary)
- [Type Checking and Inference](#type-checking-and-inference)
  - [Maximal Inference Power](#maximal-inference-power)
- [Dependency and Enso](#dependency-and-enso)
  - [Proving Program Properties](#proving-program-properties)
  - [Automating the Proof Burden](#automating-the-proof-burden)
- [References](#references)

<!-- /MarkdownTOC -->

## Goals for the Type System
In our design for Enso, we firmly believe that the type system should be able to
aid the user in writing correct programs, far and above anything else. However,
with so much of our targeted user-base being significantly non-technical, it
needs to be as unobtrusive as possible.

- Inference should have maximal power. We want users to be _forced_ to write
  type annotations in as few situations as possible. This means that, ideally,
  we are able to infer higher-rank types and make impredicative instantiations
  without annotations.
- Error messages must be informative. This is usually down to the details of the
  implementation, but we'd rather not employ an algorithm that discards
  contextual information that would be useful for crafting useful errors.
- Dependent types are a big boon for safety in programming languages, allowing
  the users that _want to_ to express additional properties of their programs
  in their types. We would like to introduce dependent types in future, but
  would welcome insight on whether it is perhaps easier to do so from the get
  go. If doing so, we would prefer to go with `Type : Type`.
- Our aim is to create a powerful type system to support development, rather
  than turn Enso into a research language. We want users to be able to add
  safety gradually.

## The Type Hierarchy
Enso is a statically typed language based upon a theory of set-based typing,
what we call `typesets`. This is a novel approach, and it is key to our intent
for Enso to _feel_ like a dynamic language while still bringing enhanced safety.

- All types are denoted by a set of constructors, which represent the atomic
  values of that type. We call these 'atoms'. For example, the typeset `Nat` is
  made up of the atoms `1, 2, 3, ...` and so on.
- Constructors are grouped into typesets.
- These typesets are arranged into a modular lattice:
  + The type `Any` is the typeset of all typesets.
  + The type `Void` is the empty typeset.
  + All atoms are typesets, but not all typesets are atoms.
  + This lattice is ordered using the `<:` subsumption judgement. For more
    information please see [typeset subsumption](#typeset-subsumption).

All in all, this means that a value in Enso can have myriad different types
attributed to it, even though these may vary greatly in the level of
specificity.

```ruby
7 : 7 : Natural : Integer : Number : Any : Any : ...
```

A brief note on naming:

- Naming in Enso is case-insensitive.
- In contexts where it is ambiguous as to whether the user would be referring to
  a fresh name or a name already in scope, capitalisation is used to determine
  which is meant.
- An uncapitalised identifier is assumed to be fresh, while a capitalised
  identifier is assumed to be in scope.

### Atoms
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

### Typesets
Typesets in Enso are an entity unique to Enso's type system. They are a
fundamental recognition of types as 'sets of values' in Enso, and while they
share some similarities with records they are a far more general notion.

- A typeset is an entity that contains one or more labels.
- Each label has a type, which _may_ be explicitly ascribed to it.
- Each label may have a default value provided for it.
- A label may be _duplicated_, as long as the types of the duplicate labels are
  different.

The other key notion of typesets is that typesets are matched _structurally_,
subject to the rules for nominal typing of atoms discussed above.

The definition for a typeset member uses the syntax `label : type = val`, where
the following rules apply:

- If a default is explicitly requested it becomes part of the subsumption
  judgement (see [below](#typeset-subsumption)).
- If the type of a label is omitted it is inferred from a default if present,
  and is otherwise inferred to be `Any`.
- If only the type is present, auto-generated labels are provided using the
  index of the member in the typeset (e.g `{ Int, String }.1` has type `Int`).
  These labels act specially for unification.
- Labels are syntactically _names_, but internally may be other entities (e.g.
  types).
- A typeset member is itself a typeset.

Typesets themselves can be declared in two ways:

1. **Anonymous:** An anonymous typeset can be declared using the curly-braces
   syntax `{}`. Such a definition must contain zero or more typeset fields (see
   above).
2. **Atoms:** An atom definition declares a typeset with a discrete identity,
   using atom definition syntax. Atom fields must only be names.

Typesets can be combined using the [typeset operators](#typeset-operators)
defined below.

In addition, we provide syntactic sugar for defining typesets as described in
the syntax document. This syntax has a special desugaring that obeys the 
following rules:

> Actually fill in these rules.
> Note that because things desugar to functions we can place arbitrary
  constraints on initialisation (partial type constructors style).

```ruby
type Maybe a
    Nothing
    type Just (value : a)

    isJust = case self of
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
Nothing.isJust self = case self of
  Nothing -> False
  Just _ -> True

Just.isJust : Maybe a -> Bool
Just.isJust self = case self of
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

### Typeset Operators
Enso defines a set of operations on typesets that can be used to combine and
manipulate them:

- **Subsumption - `<:`:** This operator expresses the relation between typesets
  defined below in the section on [typeset subsumption](#typeset-subsumption).
- **Equality - `~`:** This operator expresses the relation that two typesets `a`
  and `b` are equal (such that `a <: b && b <: a`).
- **Concatenation - `,`:** This operator combines multiple typesets, and is most
  often used to combine typeset fields. It expresses product types.
- **Union - `|`:** This operator creates a typeset that contains all the types
  in the union of its operand typesets.
- **Intersection - `&`:** This operator creates a typeset that contains all the
  types in the intersection of its operand typesets.
- **Subtraction - `\`:** This operator creates a typeset that contains all the
  types in its first operand that are _not_ also contained in its second
  operand.
- **Function - `->`:** This operator creates a mapping from one typeset to
  another, and its result is a typeset containing all the possible types of that
  mapping.

Any use of these operators introduces typing evidence which may later be
discharged through pattern matching.

> The actionables for this section are:
>
> - When necessary, we need to _explicitly formalise_ the semantics of all of
>   these operators.
> - When do applications of these constructors create matchable (injective and
>   generative) types?

#### Typeset Subsumption
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
        - For the _argument_ position of both `t` and `q`, `t.arg <: q.arg`
          (the argument position is covariant).
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
> - Ensure that co- and contra-variance are handled properly. They are a bit
>   odd under this theory.
> - Do we need explicit variance annotations?
> - How do constraints factor in?
> - We want users not to have to think about the difference between `~`, `:` and
>   `<:` so we need to work out if we can elide them from the surface language.
>   This requires considering polymorphic function arguments, partial data,
>   qualified function types and variable definitions.

#### Unsafe Typeset Field Mutation
For performance it is sometimes necessary to have the ability to _directly_
_mutate_ the field of a typeset. This is most often necessary for atoms
themselves, but as atoms are typesets this also applies.

- We define a method `setField : (field : Name) -> (value : Any) -> Nothing`
  that performs in-place field mutation of the field `field` to set its value to
  `any`.
- In order to prevent this from being used flippantly, this functionality is
  marked `unsafe` (see [access modifiers](#access-modification) for more).

### Interfaces
Because typesets can be matched _structurally_, all typesets implicitly define
interfaces. A type `t` conforming to an interface `i` in Enso is as simple as
the relation `i <: t` (as in [typeset subsumption](#typeset-subsumption))
holding.

This means that types need not _explicitly_ implement interfaces, which can be
thought of as a form of static duck typing. However, when defining a new type,
users may choose to explicitly state that it defines an interface. This has
two main benefits:

- We can include default implementations from the interface definition.
- We can provide better diagnostics in the compiler as we can point to the
  definition site instead of the use site.

```ruby
type HasName
    name: String
    name = "unnamed"

type Vector a
    self: HasName
    V2 x:a y:a
    V3 x:a y:a z:a

name (self:Int) = "IntName"

greet (t:HasName) = print 'Hi, my name is `t.name`'

main =
    greet (V3 1 2 3)
    greet 8
```

As an aside, it should be noted that the nature of Enso's typesets means that it
is easy to express far more general interfaces than Haskell's typeclasses can.

#### Special Interfaces
In order to aid usability we include a few special interfaces in the standard
library that have special support in the compiler.

##### Wrapper
In a language where composition is queen and inheritance doesn't exist there
needs to be an easy way for users to compose typesets without having to define
wrappers for the contained types. This is a big usability bonus for Enso.

```ruby
type Wrapper
    wrapped   : (lens s t a b) self.unwrapped
    unwrapped : t
    unwrapped = t # Default implementation based on inferred type.
```

`Wrapper` is an interface implemented implicitly for all typesets, and boils
down to delegating to the contained members if a given label is not found on
the top typeset. This delegation only occurs on the self type.

A usage example is as follows:

```ruby
type HasName a
    self:Wrapper # The field 'unwrapped' uses default implementation.
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

##### Convertible
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
  _iff_ all of the arguments have default values associated with them. In such
  a case, the GUI should display conversion controls with a checkbox that, when
  checked, can be converted to an explicit conversion call.
- We will need some limited mechanism for doing this even without type inference
  as it forms the backbone of good API design for the graphical interface. This
  is because polymorphic functions are much harder to support with graphical
  controls.

An example use-case is as follows:

```ruby
type Vector a
    type V3 x:a y:a z:a

    self : Convertible String
    to = 'V3 `self.x` `self.y` `self.z`'

    self : Convertible (a: Semigroup)
    to = self.x + self.y + self.z

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

##### Destruct
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

Initially, going out of scope will be defined as the point at which the
instance is garbage collected, while later, once we are able to perform more
sophisticated analysis, it will instead be defined as the point at which the
instance's lexical lifetime ends.

It should be noted, however, that a type that implements an explicit `destroy`
method should still implement explicit methods for resource handling as lexical
lifetimes are not always sufficient (e.g. a socket that you may want to close
and re-open in the same block).

> The actionables for this section are:
>
> - Determine how this interacts with copying and moving.

### Type Ascription
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

Additionally, as Enso is a dependently-typed language, the expression `b` may
contain arbitrary Enso expressions. The type-checking of such signatures is
discussed further in the section on [dependency](#dependency-and-enso).

### Projections
In order to work efficiently with typesets, we need the ability to seamlessly
access and modify (immutably) their properties. In the context of our type
theory, this functionality is known as a _projection_, in that it projects a
value from (or into) a typeset.

Coming from Haskell, we are well-versed with the flexibility of lenses, and
more generally _optics_. To that end, we base our projection operations on
standard theories of optics. While we _do_ need to formalise this, for now we
provide examples of the expected basic usage. This only covers lenses, while in
the future we will likely want prisms and other more-advanced optics.

A projection is generated for each field of a typeset.

> Actionables for this section:
>
> - Work out whether standard optics theory with custom types is sufficient for
>   us. We may want to support side effects.
> - Determine how much of the above we can support without a type-checker. There
>   are likely to be a lot of edge-cases, so it's important that we make sure we
>   know how to get as much of it working as possible.
> - How (if at all) do lenses differ for atoms and typesets?

#### Special Fields
We also define special projections from typesets:

- `index`: The expression `t.n`, where `n` is of type `Number` is translated to
  `t.index n`.
- `field`: The expression `t.s` where `s` is of type `Text` is translated to
  `t.fieldByName s`.

## Function Types
As a functional programming language, the type of functions in Enso (`->`) is
key. There are a few things that should be noted about functions in Enso.

> The actionables for this section:
>
> - Work out a full specification for function typing and behaviour.
> - Calling a function with an upper-case letter instantiates all of its type
>   arguments to free type variables.

### Scoping
Enso's scoping rules should be fairly familiar to most other programming
languages, as it uses standard lexical scoping with shadowing. However, there
are a few unique points to keep in mind:

- There is no separation between the type and term levels. This means that, for
  a given lexical scope, type, function and lambda variables exist in the same
  name space.
- This means that if different names are used on the type and value level to
  refer to the same entity, both names are valid.
- Name clashes between different entities are not allowed.
- Shadowing between different lexical scopes occurs as standard.
- Shadowing in the same lexical scope may only occur in a chain of lambdas, and
  in such a case the shadowed variables must unify.
- Variables from the body are accessible in the type signature.
- Variables from the type signature are accessible in the body.

Scope lookup proceeds from the function body outwards, as is standard for
lexical scoping.

### Structural Type Shorthand
In Enso, we want to be able to write a type-signature that represents types in
terms of the operations that take place on the input values. A classical example
is `add`:

```ruby
add : a -> b -> b + a
add = a -> b -> b + a
```

There are a few things to note about this signature from a design standpoint:

- `a` and `b` are not the same type. This may, at first glance, seem like a
  signature that can't work, but the return type, in combination with our
  integrated `Convertible` mechanism gives us the tools to make it work.
- The return type is `a + b`. This is a shorthand expression for a detailed
  desugaring. The desugaring provided below is what the typechecker would infer
  based on such a signature.

```ruby
add : forall a b c d. ({+ : Convertible b c => a -> c -> d} <: a) => a -> b -> d
```

This may look fairly strange at first, but we can work through the process as
follows:

1. The expression `b + a` is syntactic sugar for a method call on a: `a.+ b`.
2. This means that there must be a `+` method on a that takes both an `a` and a
   `b`, with return-type unspecified as of yet: `+ : a -> b -> ?`
3. However, as `Convertible` is built into the language, we have to consider
   that for `a.+ b` to work, the `+` method can actually take any type to which
   `b` converts. This introduces the constraint `Convertible b c`, and we get
   `+ : a -> c -> ?`
4. The return type from a function need not be determined by its arguments, so
   hence in the general case we have to default to an unrestricted type variable
   giving `+ a -> c -> d`.
5. This method must exist on the type `a`, resulting in the constraint that the
   row `{+ : a -> c -> d} <: a` must conform to that interface.
6. We now know the return type of `a + b`, and can rewrite it in the signature
   as `d`.

Please note that `a <: b` (which can be flipped as `:>`) means that `a` is a row
that is a sub-row contained within the row `b`. The containment relation allows
for the possibility that `a == b`.

The inferred type for any function should, in general, match the given type
signature. Cases where this break down should only exist where the type
signature acts to constrain the function type further than would be inferred.

> The actionables for this section are:
>
> PLEASE NOTE. THIS IS OUT OF DATE.
> 
> - Clean it up and work out how it applies in light of the new developments of
>   typesets.

### Function Composition
Enso introduces a function composition operator which composes functions after
all arguments have been applied. This operator is `>>` (and its backwards cousin
`<<`). It takes a function `f` with `n` arguments, and a function `g` with `m`
arguments, and the result consumes `n` arguments, applies them to `f`, and then
applies the result of that plus any additional arguments to `g`.

```ruby
computeCoeff = (+) >> (*5)

doThing = (+) >> (*)
```

In addition, we have the standard function composition operator `.`, and its
backwards chaining cousin `<|`.

> The actionables from this section are:
>
> - Examples for the more advanced use-cases of `>>` to decide if the type
>   complexity is worth it.

## Access Modificatiom
While we don't usually like making things private in a programming language, it
sometimes the case that it is necessary to indicate that certain fields should
not be touched (as this might break invariants and such like). To this end, we
propose an explicit mechanism for access modification that works as follows:

- We provide explicit access modifiers that, at the definition site, start an
  indented block. These are `private` and `unsafe`.
- All members in the block have the access modifier attributed to them.
- By default, accessing any member under an access modifier will be an error.
- To use members under an access modifier, you use the syntax `use <mod>`, where
  `<mod>` is a modifier. This syntax 'takes' an expression, including blocks,
  within which the user may access members qualified by the modifier `<mod>`.

While `private` works as you might expect, coming from other languages, the
`unsafe` annotation has additional restrictions:

- It must be explicitly imported from `Std.Unsafe`.
- When you use `unsafe`, you must write a documentation comment on its usage
  that contains a section `Safety` that describes why this usage of unsafe is
  valid.

> The actionables for this section are:
>
> - How do we type this?

## Pattern Matching
Pattern matching in Enso works similarly to as you would expect in various other
functional languages. Typing information is _always_ refined in the branches of
a case expression, which interacts well with dependent typing and type-term
unification. There are a few main ways you can pattern match:

1.  **Positional Matching:** Matching on the scrutinee by structure. This works
    both for atoms and typesets (for typesets it is a subsumption judgement).

    ```ruby
    type Vector a
      V2 x:a y:a
      V3 x:a y:a z:a

    v = Vector.V3 x y z

    case v of
      Vector.V3 x y z -> print x
    ```

2.  **Type Matching:** Matching purely by the types involved, and not matching
    on structure.

    ```ruby
    case v of
      Vector.V3 -> print v.x
    ```

3.  **Name Matching on Labels:** Matching on the labels defined within a type
    for both atoms and typesets, with renaming.

    ```ruby
    case v of
      Vector.V3 {x y} -> print x
      {x}             -> print x
    ```

4.  **Naming Scrutinees in Branches:** Ascribing a name of a scrutinee is done
    using the standard typing judgement. This works due to the type-term
    unification present in Enso.

    ```ruby
    case _ of
      v : Vector.V3 -> print v,x
    ```

> The actionables for this section :
>
> - How do we type pattern matching?
> - Exactly how (and what) evidence is discharged during matching?
> - How can we use bujective applications of the
>   [typeset operators](#typeset-operators) to perform pattern matching?
> - How do we extend this to structural matching in general on typesets?

## Dynamic Dispatch
Enso is a language that supports pervasive dynamic dispatch. This is a big boon
for usability, as users can write very flexible code that still plays nicely
with the GUI.

The current implementation of Enso supports single dispatch (dispatch purely on
the type of `self`), but there are broader visions afoot for the final
implementation of dynamic dispatch in Enso.

> The actionables for this section include:
>
> - Determining whether we want to support proper multiple dispatch in the
>   future. This is important to know as it has implications for the type
>   system, and the design of the dispatch algorithm.

### Specificity
In order to determine which of the potential dispatch candidates is the correct
one to select, the compiler needs to have a notion of _specificity_, which is
effectively an algorithm for determining which candidate is more specific than
another.

- Always prefer a member function for both `x.f y` and `f y x` notations.
- Only member functions, current module's functions, and imported functions are
  considered to be in scope. Local variable `f` could not be used in the `x.f y`
  syntax.
- Selecting the matching function:
  1. Look up the member function. If it exists, select it.
  2. If not, find all functions with the matching name in the current module and
     all directly imported modules. These functions are the _candidates_.
  3. Eliminate any candidate `X` for which there is another candidate `Y` whose
     `this` argument type is strictly more specific. That is, `Y` self type is a
     substitution of `X` self type but not vice versa.
  4. If not all of the remaining candidates have the same self type, the search
     fails.
  5. Eliminate any candidate `X` for which there is another candidate `Y` which
     type signature is strictly more specific. That is, `Y` type signature is a
     substitution of `X` type signature.
  6. If exactly one candidate remains, select it. Otherwise, the search fails.

> The actionables for this section are as follows:
> 
> - THE ABOVE VERSION IS OLD. NEEDS UPDATING.
> - The definition of specificity for dispatch candidates (including how it
>   interacts with the subsumption relationship on typesets and the ordering of
>   arguments).

### Multiple Dispatch
It is an open question as to whether we want to support proper multiple dispatch
in Enso. Multiple dispatch refers to the dynamic dispatch target being
determined based not only on the type of the `self` argument, but the types of
the other arguments to the function.

To do multiple dispatch properly, it is very important to get a rigorous
specification of the specificity algorithm. It must account for:

- The typeset subsumption relationship.
- The ordering of arguments.
- How to handle defaulted and lazy arguments.
- Constraints in types. This means that for two candidates `f` and `g`, being
  dispatched on a type `t` with constraint `c`, the more specific candidate is
  the one that explicitly matches the constraints. An example follows:

  ```ruby
  type HasName
    name : String

  greet : t -> Nothing in IO
  greet _ = print "I have no name!"

  greet : (t : HasName) -> Nothing in IO
  greet t = print 'Hi, my name is `t.name`!'

  type Person
    Pers (name : String)

  main =
    p1 = Person.Pers "Joe"
    greet p1 # Hi, my name is Joe!
    greet 7  # I have no name
  ```

  Here, because `Person` conforms to the `HasName` interface, the second `greet`
  implementation is chosen because the constraints make it more specific.

## Modules
With such a flexible type system in Enso, the need for making modules
first-class is obviated. Instead, a module is very much its own entity, being
simply a container for bindings (whether they be functions, methods, atoms, or
more generic typesets).

- Where the module name clashes with a member contained in the module, the
  member is preferred. If you need the module you must import it qualified under
  another name.
- We provide the alias `here` as a way to access the name of the current module.

> The actionables for this section are:
>
> - Characterise modules in more depth as we need them.

### Scoping and Imports
To use the contents of a module we need a way to bring them into scope. Like
most languages, Enso provides an _import_ mechanism for this. Enso has four
different kinds of imports that may be combined freely, all of which take a
module path as their first argument.

1. **Unqualified Imports:** These import all symbols from the module into the
   current scope (`import M`).
2. **Qualified Imports:** These import all symbols from the module into the
   current scope with symbols qualified under a name _different_ from the
   module name (`import M as T`).
3. **Restricted Imports:** These import only the specific symbols from the
   module into the current scope (`import M only sym1 sym2`).
4. **Hiding Imports:** These are the inverse of restricted imports, and import
   _all_ symbols other than the named ones into the current scope
   (`import M hiding sym1 sym2`),

Imports may introduce ambiguous symbols, but this is not an error until one of
the ambiguous symbols is used in user code.

When importing a module `X` into the current module `Y`, the bindings in `X`
become available in `Y` (modified by the import type). However, these bindings
are _not_ available in `Y` externally. This means that we need a re-export
mechanism. Similarly to imports, this has four kinds, all of which take a module
path as their first argument, and all of which _may_ introduce the module it
exports into scope (if it is not already imported).

1. **Unqualified Exports:** These export all symbols from the module as if they
   were defined in the exporting module (`export X`).
2. **Qualified Exports:** These export all symbols from the module as if they
   were defined in another module accessible in the exporting module
   (`export X as Y`).
3. **Restricted Exports:** These export only the specified symbols from the
   module as if they were defined in the exporting module (`export X only sym`)
4. **Hiding Exports:** These export all symbols from the module except those
   explicitly specified (`export X hiding sym1 sym2`).

Exports effectively act to 'paste' the contents of the exported module into the
module declaring the export. This means that exports that create name clashes
must be resolved at the source.

> The actionables for this section are:
>
> - Are we _really, really_ sure we want unqualified by default?
> - Think about how to handle imports properly in the type checker. What, if
>   they have any, are the impacts of imports on inference and checking?

## Monadic Contexts
Coming from a Haskell background, we have found that Monads provide a great 
abstraction with which to reason about program behaviour, but they have some
severe usability issues. The main one of these is the lack of automatic lifting,
requiring users to explicitly lift computations through their monad transformer
stack.

For a language as focused on usability as Enso is this really isn't feasible. To
that end, we have created the notion of a 'Monadic Context', which is a monad
transformer based on Supermonads (see [references](#references)). These have
special support in the compiler, and hence can be automatically lifted to aid
usability. There are three main notes about the syntax of contexts:

1. Monadic contexts are defined using the `in` keyword (e.g. `Int in IO`).
2. We have a symbol `!`, which is short-hand for putting something into the 
   `Exception` monadic context. This is related to broken values.
3. Contexts can be combined by using the standard typeset operators, or nested
   through repeated uses of `in`.

It is also important to note that Enso has no equivalent to `<-` in Haskell.
Instead, pure computations are implicitly placed in the `Pure` monadic context,
and `=` acts to 'peel off' the outermost layer of contexts. As such, this means
that `=` _always_ acts as `bind`, greatly simplifying how the type-checker has
to work.

> The actionables for this section are:
>
> - Think about subsumption for contexts.
> - Contexts (e.g. IO) are represented using `T in IO`. Multiple contexts are
>   combined as standard `(IO | State Int)`, and it is written the same in arg
>   position.

### Context Definitions
Contexts can be defined by users.

> The actionables for this section are:
> 
> - How, what, when and why?

### Context Lifting
> The actionables for this section are:
> 
> - Specify and explain how automated lifting of monadic contexts works.

## Strictness and Suspension
Enso is a language that has strict semantics by default, but it can still be
very useful to be able to opt-in to suspended computations (thunks) for the
design of certain APIs. 

To that end, Enso provides a mechanism for this through the type system. The
standard library defines a `Suspend a` type which, when used in explicit type
signatures, will cause the corresponding expression to be suspended. 

- The explicit calls to `Suspend` and `force` are inserted automatically by the
  compiler doing demand analysis.
- This demand analysis process will also ensure that there are not polynomial
  chains of suspend and force being inserted to ensure performance.

> The actionables for this section are as follows:
> 
> - Specify this much better.

## Analysing Parallelism

> The actionables for this section are:
> 
> - Work out how the type checker can support parallelism analysis.

## Typed Holes

> The actionables for this section are:
> 
> - Determine how we want to support typed holes.
> - Determine the syntax for typed holes.

## Errors
Enso supports two notions of errors. One is the standard asynchronous exceptions
model, while the other is a theory of 'broken values' that propagate through
computations.

> The actionables for this section are:
> 
> - Greatly expand on the reasoning and theory behind the two exception models.
> - Explain why broken values serve the GUI well.

### Async Exceptions

> The actionables for this section are:
> 
> - Formalise the model of async exceptions as implemented.

### Broken Values
In Enso we have the notion of a 'broken' value: one which is in an invalid state
but not an asynchronous error. While these may initially seem a touch useless,
they are actually key for the display of errors in the GUI.

Broken values can be thought of like checked monadic exceptions in Haskell, but
with an automatic propagation mechanism:

- Broken values that aren't handled explicitly are automatically promoted
  through the parent scope. This is trivial inference as no evidence discharge
  will have occurred on the value.

  ```ruby
  open : String -> String in IO ! IO.Exception
  open = ...

  test =
    print 'Opening the gates!'
    txt = open 'gates.txt'
    print 'Gates were opened!'
    7
  ```

  In the above example, the type of test is inferred to
  `test : Int in IO ! IO.Exception`, because no evidence discharge has taken
  place as the potential broken value hasn't been handled.
- This allows for very natural error handling in the GUI.

> The actionables for this section are:
>
> - Determine what kinds of APIs we want to use async exceptions for, and which
>   broken values are more suited for.
> - Ensure that we are okay with initially designing everything around async
>   exceptions as broken values are very hard to support without a type checker.
> - Initially not supported for APIs.

## Dynamic
As Enso can seamlessly interoperate with other programming languages, we need a
principled way of handling dynamic types that we don't really know anything
about. This mechanism needs:

- A way to record what properties we _expect_ from the dynamic.
- A way to turn a dynamic into a well-principled type-system member without
  having the dynamics pollute the whole type system. This may involve a 'trust
  me' function, and potentially dynamicness-polymorphic types.
- A way to understand as much as possible about what a dynamic _does_ provide.
- A way to try and refine information about dynamics where possible.

```ruby
obj.model =
    { atom  : Text
    , dict  : Map Text Any
    , info  :
        { doc  : Text
        , name : Text
        , code : Text
        , loc  : Location
        }
    , arg  : # used only when calling like a function
        { doc     : Text
        , default : Maybe Any
        }
    }
```

> The actionables for this section are:
>
> - Work out how to do dynamic properly, keeping in mind that in a dynamic value
>   could self-modify underneath us.

### The Enso Boundary
Fortunately, we can at least avoid foreign languages modifying memory owned by
the Enso interpreter. As part of the interop library, Graal lets us mark memory
as read-only. This means that the majority of data passed out (from a functional
language like Enso) is not at risk. However, if we _do_ allow data to be worked
with mutably, when control is returned to Enso it needs to be treated as a
dynamic as it may have been modified.

## Type Checking and Inference
As a statically-typed language, Enso is built with a sophisticated type checker
capable of reasoning about a fully dependently-typed system. However, a type
checker on its own is quite useless. For Enso to truly be usable, it must also
have a powerful type inference engine.

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

> The actionables for this section are:
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

## Dependency and Enso
Enso is a [dependently typed](https://en.wikipedia.org/wiki/Dependent_type)
programming language. This means that types are first-class values in the
language, and hence can be manipulated and computed upon just like any other
value. To the same end, there is no distinction between types and kinds, meaning
that Enso obeys the 'Type in Type' axiom of dependent types. While this does
make the language unsound as a logic, the type safety properties do not depend
on this fact.

In essence, values are types and types are values, and all kinds are also types.
This means that, in Enso, you can run _arbitrary_ code to compute with types.
All in all, dependency in Enso is not about being a proof system, but is instead
about enabling practical usage without hampering usability. To that end we
combine a powerful and non-traditional dependently-typed system with the ability
to automate much of the proof burden through SMT solvers.

> The actionables for this section are:
>
> - Do we want the ability to explicitly quantify type variables for visibility,
>   dependency, relevance and requiredness (forall, foreach, etc).
> - How do we infer as much of these above properties as possible?
> - Based on QTT and RAE's thesis.

### Proving Program Properties
Some notes:

- Dependent types as constructing proofs through combining types. Combining
  types provides evidence which can be discharged to create a proof. A value can
  then only be constructed by discharging a proof.
- To provide more predictable inference in dependent contexts, this system will
  draw on the notion of _matchability polymorphism_ as outlined in the
  higher-order type-level programming paper. The key recognition to make,
  however is that where that paper was required to deal with
  backwards-compatibility concerns, we are not, and hence can generalise all
  definitions to be matchability polymorphic where appropriate.
- Provide some keyword (`prove`) to tell the compiler that a certain property
  should hold when typechecking. It takes an unrestricted expression on types,
  and utilises this when typechecking. It may also take a string description of
  the property to prove, allowing for nicer error messages:

    ```
    append : (v1 : Vector a) -> (v2 : Vector a) -> (v3 : Vector a)
    append = vec1 -> vec2 ->
        prove (v3.size == v1.size + v2.size) "appending augments length"
        ...
    ````

    Sample error:

    ```
    [line, col] Unable to prove that "appending augments length":
        Required Property: v3.size == v1.size + v2.size
        Proof State:       <state>

        <caret diagnostics>
    ```

  This gives rise to the question as to how we determine which properties (or
  data) are able to be reasoned about statically.
- Dependent types in Enso will desugar to an application of Quantitative Type
  Theory.

> The actionables for this section are:
> 
> - Specify how we want dependency to behave in a _far more rigorous_ fashion.

### Automating the Proof Burden
Even with as capable and simple a dependently-typed system as that provided by
Enso, there is still a burden of proof imposed on our users that want to use
these features. However, the language [F*](https://www.fstar-lang.org/) has
pioneered the combination of a dependently-typed system with an SMT solver to
allow the automation of many of the simpler proofs. 

- The Enso typechecker will integrate an aggressive proof rewrite engine to
  automate as much of the proof burden as possible.

> The actionables for this section are:
> 
> - What is the impact of wanting this on our underlying type theory?
> - Where and how can we draw the boundary between manual and automated proof?
> - How much re-writing can we do (as aggressive as possible) to assist the SMT
>   solver and remove as much proof burden as possible.

## References
The design of the type-system described in this document is based on prior work
by others in the PL design community. The (probably) complete list of references
is as below.

#### Rows
- [Abstracting Extensible Data Types](http://ittc.ku.edu/~garrett/pubs/morris-popl2019-rows.pdf)

#### Maximum Inference Power
- [A Theory of Qualified Types](https://github.com/sdiehl/papers/blob/master/A_Theory_Of_Qualified_Types.pdf)
- [Boxy Type-Inference for Higher-Rank Types and Impredicativity](https://www.microsoft.com/en-us/research/publication/boxy-type-inference-for-higher-rank-types-and-impredicativity/)
- [Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism](https://www.cl.cam.ac.uk/~nk480/bidir.pdf)
- [Flexible Types: Robust Type Inference for First-class Polymorphism](https://www.microsoft.com/en-us/research/publication/flexible-types-robust-type-inference-for-first-class-polymorphism/)
- [FPH: First-Class Polymorphism for Haskell](https://www.microsoft.com/en-us/research/publication/fph-first-class-polymorphism-for-haskell/)
- [MLF: Raising ML to the Power of System-F](http://gallium.inria.fr/~remy/work/mlf/icfp.pdf)
- [Practical Type Inference for Arbitrary-Rank Types](https://www.microsoft.com/en-us/research/publication/practical-type-inference-for-arbitrary-rank-types/)
- [QML: Explicit, First-Class Polymorphism for ML](https://www.microsoft.com/en-us/research/wp-content/uploads/2009/09/QML-Explicit-First-Class-Polymorphism-for-ML.pdf)
- [Wobbly Types: Type Inference for GADTs](https://www.microsoft.com/en-us/research/publication/wobbly-types-type-inference-for-generalised-algebraic-data-types/)

#### Dependent Types
- [Dependent Types in Haskell: Theory and Practice](https://cs.brynmawr.edu/~rae/papers/2016/thesis/eisenberg-thesis.pdf)
- [Higher-Order Type-Level Programming in Haskell](https://www.microsoft.com/en-us/research/uploads/prod/2019/03/ho-haskell-5c8bb4918a4de.pdf)
- [Practical Erasure in Dependently-Typed Languages](https://eb.host.cs.st-andrews.ac.uk/drafts/dtp-erasure-draft.pdf)
- [Syntax and Semantics of Quantitative Type Theory](https://bentnib.org/quantitative-type-theory.pdf)

#### Monadic Contexts
- [Supermonads](http://eprints.nottingham.ac.uk/36156/1/paper.pdf)

#### Types and Performance
- [Levity Polymorphism](https://cs.brynmawr.edu/~rae/papers/2017/levity/levity-extended.pdf)
- [Partial Type-Constructors](https://cs.brynmawr.edu/~rae/papers/2019/partialdata/partialdata.pdf)
