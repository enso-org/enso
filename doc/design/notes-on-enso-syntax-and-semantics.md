# Notes on Enso's Syntax and Semantics
As we get closer to the development of the more sophisticated features of the
language, as well as have a more fully-featured interpreter, we really need to
clarify exactly how certain syntactic and semantic elements of the language
behave.

This document aims to clarify the behaviour of many language constructs, as
well as expose any open questions that we have about them. It is not intended
to be a design document directly itself, but the information contained here
will be _used_ to later contribute to various, more-specialised design
documents.

<!-- MarkdownTOC levels="2,3,4" autolink="true" -->

- [Variable Naming](#variable-naming)
- [Top-Level Evaluation](#top-level-evaluation)
- [High-Level Syntax and Semantic Notes](#high-level-syntax-and-semantic-notes)
- [Annotations](#annotations)
- [Types](#types)
    - [Atoms](#atoms)
        - [Anonymous Atoms](#anonymous-atoms)
    - [Typesets](#typesets)
        - [The Type Hierarchy](#the-type-hierarchy)
        - [Typesets and Smart Constructors](#typesets-and-smart-constructors)
        - [Type and Interface Definitions](#type-and-interface-definitions)
        - [Anonymous Typesets](#anonymous-typesets)
        - [Typeset Projections \(Lenses\)](#typeset-projections-lenses)
    - [Implementing Interfaces](#implementing-interfaces)
        - [Special Interfaces](#special-interfaces)
    - [Pattern Matching](#pattern-matching)
- [Dynamic Dispatch](#dynamic-dispatch)
    - [Multiple Dispatch](#multiple-dispatch)
        - [Overlappable Functions](#overlappable-functions)
    - [First-Class Modules](#first-class-modules)
        - [Self-Initialisation and Qualified Access](#self-initialisation-and-qualified-access)
- [Broken Values](#broken-values)
- [Function Composition](#function-composition)
- [Dynamic](#dynamic)
    - [The Enso Boundary](#the-enso-boundary)
    - [An Insufficient Design For Dynamic](#an-insufficient-design-for-dynamic)
- [The Main Function](#the-main-function)

<!-- /MarkdownTOC -->

## Variable Naming
One of the key features of Enso as a language is a total lack of separation
between the value and type level syntaxes. This enables a staggering uniformity
of programming in the language, allowing arbitrary computations in types as
well as in values. This means that arbitrary names can refer to both types and
values (as they are one and the same).

However, this means that naming becomes a bit of a concern. Without a syntactic
disambiguator, it becomes much harder to keep a minimal syntax for things like
pattern matches. To this end, Enso itself enforces naming conventions:

- Everything is named in `camelCase`.
- When you want to refer to a name in a pattern, you can instead use the same
  name, but in `UpperCamelCase`. For example, `fooBar` becomes `FooBar`. This
  allows us a uniformity and syntactic marker for disambiguation.

For much of the history of the language's development, we have been happy with
using `camelCase` and `UpperCamelCase` naming conventions to mark this
distinction, but recently it has been raised that we might instead prefer to
use `snake_case` to refer to variables. A few thoughts on this follow:

- Snake case tends to be far more readable than camel case. This is primarily
  down to the fact that the `_` is far more readily readable as a space.
- However, with `snake_case`, we have to still have some syntactic identifier
  for type names in patterns, which would be `SnakeCase`. Unlike the
  distinction with camel case, this creates a much larger visual disparity
  with snake case.
- In all cases, mixed style (e.g. `foo_Bar`) would be disallowed to allow the
  language source to be uniform.
- If we go with `snake_case`, we should come up with another syntax for the
  definition of mixfix functions, and we may want to do this anyway. The
  current proposal for this is `if check _then ok _else fail`, which may be
  something that we want to adopt regardless of the decision on this section.

Please note that this file sticks to the pre-determined naming convention for
Enso, as no final decision has been made on whether or not it should be
changed.

> No final decision has been made on this point yet, so the actionables are as
> follows:
>
> - Wojciech wants to think about it more.
> - The decision needs to be made by the next design meeting on 2019-12-10.
> - Support in the parser is fairly simple, though some consideration is needed
>   for how to support the mixfix definition syntax as methods.

## Top-Level Evaluation
An ongoing discussion for the language design has been whether or not to allow
for the top-level evaluation of statements in Enso. In order to help make a
decision, we listed the following use-cases for top-level evaluation. These are
annotated using the following key:

|  Label  | Meaning |
| --------| ------- |
| `[?,_]` | We don't know how to implement it, but it may be possible.
| `[-,_]` | Not possible to implement using purely syntactic macros.
| `[M,_]` | Possible to implement using purely syntactic macros.
| `[_,H]` | High priority. This will be used often.
| `[_,M]` | Medium priority. This will be used with a medium frequency.
| `[_,L]` | Low priority. Nice to have, but we can likely live without it.
| `[_,!]` | Something that we never want to have in the language.

The use-cases we have considered are as follows:

|  Label  | Description |
| ------- | ----------- |
| `[-,L]` | Creating top-level constructs in `IO`, such as `IORef`. This is, in general, considered to be bad style, but can sometimes be useful.
| `[-,L]` | Using enso files like python is able to be for scripting work. The ability to write constructs at the top-level and just evaluate them.
| `[M,H]` | The ability to generate structures and / types for a dataframe at compilation time, or the automatic generation of an API for a library. A key recognition is that dependent types and type-level execution replace much of the need to be able to query the type-checker and runtime while writing a syntactic macro.
| `[M,H]` | Static metaprogramming (transformations from `AST -> AST`) to let users generate types and functions based on existing AST. There is the potential to want to be able to evaluate actions in `IO` while doing this, but it may not be necessary.
| `[-,!]` | Dynamic metaprogramming to let users mutate program state at runtime (e.g. changing atom shapes, function definitions), also known as 'monkey patching'. This is not something we want in the language, but we do perhaps want the ability to do so on values of type `Dynamic`.
| `[M,H]` | 'Remembering' things when compiling a file, such as remembering all structures marked by an `AST` annotation. An example use case for a mechanism like this is to generate pattern matches for all possible `AST` types. This can be done by letting macros write to a per-file peristent block of storage that could be serialised during precompilation.
| `[M,H]` | Grouping of macros (e.g. `deriveAll = derive Ord Debug Show`). This can be easily handled by doing discovery on functions used as macros, and treating it as a macro as well.
| `[?,M]` | Method-missing magic, akin to ruby. This is likely able to be handled using other, existing language mechanisms.

In summary and when considering the above use-cases, it seems that there is
little need for top-level expression evaluation in Enso. We can support all of
the above-listed important use-cases using syntactic (`AST -> AST`) macros,
while allowing for top-level evaluation would enable users to write a lot of
overly-magical code, which will always be a code-smell.

Syntactic macros, however, do not easily support a scripting workflow, but the
solution to this problem is simple. We can just provide an `enso run <file>`
command which will search for and execute the `main` function in the provided
file.

> The actionables for this section are as follows:
>
> - Formalise and clarify the semantics of `main`.

## High-Level Syntax and Semantic Notes
While the majority of syntactic design for the language has utilised top-level
bindings in a syntax similar to that of Haskell or Idris, some consideration
has been given to instead introducing function bindings using a `def` keyword.

This has a few major problems, including:

- The typing of variables becoming very ugly, with bad alignment.

  ```ruby
  foo : Int -> Int -> Int
  def foo a b = a + b
  ```

- The standard Haskell/Idris-style definition syntax would no longer be valid,
  but would also not be used anywhere.
- There would be duplicated syntax for doing the same thing (e.g. `val1 = 5`
  and `def val1 = 5` would be equivalent).
- The `=` operator would still need to be used for single-line function
  definitions, making the syntax inconsistent.
- Interface definitions become very confusing:

  ```ruby
  type HasName
    name : String

  type HasName2
    def name : String
  ```

Additionally, in the current syntax, a block assigned to a variable is one that
has its execution implicitly suspended until it is forced. This has a few
things that should be noted about it.

- There is a big mismatch between the semantics of assigning inline to a
  variable versus assigning a block to a variable. The following are not
  equivalent:

  ```ruby
  a = foo x y

  a =
    foo x y
  ```

- We could have a `suspend` function provided in the standard library, as
  laziness of argument evaluation is determined through type-signatures and is
  done automatically in the compiler.
- Such a function would likely not see heavy use.

As Enso types (other than atoms), are defined as sets of values (see the
section on [set types](#set-types) for details), we need a way to include an
atom inside another type that doesn't define it.

- It would be potentially possible to disambiguate this using syntactic markers
  but this is likely to be unclear to users.
- Instead we propose to use a keyword (e.g. `use` or `include`) to signify the
  inclusion of an atom inside a type definition.
  
However, please note that in case there is no top-level evaluation, there is no
need for any additional keyword because the inclusion syntax is not ambiguous
anymore.

> The actionable items for this section are as follows:
>
> - Further discussion on the semantics of top-level blocks, with a decision
>   made by 2019-12-10.
> - Make a decision regarding a keyword for including other atoms in a type
>   (e.g. `use`).
> - Make a final decision on whether it is `this` or `self`.

## Annotations
Much like annotations on the JVM, annotations in Enso are tags that perform a
purely syntactic transformation on the entity to which they are applied. The
implementation of this requires both parser changes and support for
user-defined macros, but for now it would be possible to work only with a set
of hard-coded annotation macros.

Annotations can be arbitrarily nested, so a set of annotation macros become
implicitly nested inside each other:

```ruby
@derive Eq Debug
@make_magic
type Maybe a
  use Nothing
  type Just
```

The above example is logically translated to:

```ruby
derive Eq Debug
  make_magic
    type Maybe a
      use Nothing
      type Just (value : a)
```

In the presence of annotations and macros, it becomes more and more important
that we are able to reserve words such as `type` to ensure that users can
always have a good sense of what the most common constructs in the language
mean, rather than allowing them to be overridden outside of the stdlib.

This would allow types to automatically derive `Debug`, for example, which
would be a function `debug` which prints detailed debugging information about
the type (e.g. locations, source info, types, etc).

> The actionables for this section are:
>
> - Decide if we want to reserve certain key bits of syntax for use only by
>   the standard library (with a focus on `type`).

## Types
Atoms are the fundamental building blocks of types in Enso. Where broader types
are sets of values, Atoms are 'atomic' and have unique identity. They are the
nominally-typed subset of Enso's types, that can be built up into a broader
notion of structural, set-based typing. All kinds of types in Enso can be
defined in arbitrary scopes, and need not be defined on the top-level.

For more information on Enso's type-system, please take a look in the
[`types.md`](type-system/types.md) document.

### Atoms
In Enso, Atoms are product types with named fields, where each field has a
distinct and un-constrained type. Atoms are defined by the `type` keyword,
which can be statically disambiguated from the standard usage of `type`.

Some examples of atoms are as follows, with a usage example:

```ruby
type Nothing
type Just value
atom Vec3 x y z
atom Vec2 x y

v = V3 1 2 3 : V3 1 2 3 : V3 Int Int Int : V3 Any Any Any : Any
```

The key notion of an atom is that it has _unique identity_. No atom can unify
with any other atom, even if they have the same fields with the same names. To
put this another way, an atom is _purely_ nominally typed.

#### Anonymous Atoms
Using the same keyword used to define atoms it is possible to define an
anonymous atom. The key disambiguator is syntactic, using upper- or lower-case
starts for names.

```ruby
point = type x y z
p1 = point 1 2 3 : Point Int Int Int
```

There are no differences in functionality between anonymous and named atoms.

> Actionables for this section:
>
> - What is the motivating use-case for an anonymous atom?

### Typesets
More complex types in Enso are known as typesets. All of these types are
_structural_. This means that unification on these types takes place based upon
the _structure_ of the type (otherwise known as its 'shape').

Two typesets `A` and `B` can be defined to be equal as follows, where equality
means that the sets represent the same type.

1.  `A` and `B` contain the same set of _labels._ A label is a _name_ given to
    a type.
2.  For each label in `A` and `B`, the type of the label in `A` must be equal to
    the type of the same label in `B`:

    1.  Atoms are only equal to themselves, accounting for application.
    2.  Types are equal to themselves recursively by the above.

Two typesets `A` and `B` also have a _subsumption_ relationship `<:` defined
between them. `A` is said to be subsumed by `B` (`A <: B`) if the following
hold:

**[WD]** I don't think it holds for functions (contravariances).

1.  `A` contains a subset of the labels in `B`.
2.  For each label in `A`, its type is a subset of the type of the same label in
    `B` (or equal to it):

    1.  An atom may not subsume another atom.
    2.  A type is subsumed by another time recursively by the above, accounting
        for defaults (e.g. `f : a -> b = x -> c` will unify with `f : a -> c`)
    3.  The subsumption judgement correctly accounts for covariance and
        contravariance with function terms.
    4.  The subsumption judgement correctly accounts for constraints on types.

As typesets are matched structurally, a typeset definition serves as both a type
and an interface.

> The actionables for this section are:
>
> - Determine if we want to support multiple dispatch in the future, as this has
>   impacts on whether the underlying theory for typesets needs to support
>   overidden labels (e.g. two definitions for `foo` with different types).

#### The Type Hierarchy
These typesets are organised into a _modular lattice_ of types, such that it is
clear which typesets are subsumed by a given typeset. There are a few key
things to note about this hierarchy:

- The 'top' type, that contains all typesets and atoms is `Any`.
- The 'bottom' type, that contains no typesets or atoms is `Nothing`.

#### Typesets and Smart Constructors
Enso defines the following operations on typesets that can be used to combine
and manipulate them:

- **Union:** `|` (e.g. `Maybe a = Nothing | Just a`)
- **Intersection:** `&` (e.g. `Person = HasName & HasPhone`)
- **Subtraction:** `\` (e.g. `NegativeNumber = Int \ Nat \ 0`)

Bijective applications of these constructors are able to be used for pattern
matching. Initially we only plan to support simple bijection detection, but
this may expand in the future. An example that would not be supported initially
follows:

```ruby
type V3 x y z

zV2 x y = V3 x y 0

test = match _ of
  ZV2 x y  -> ...
  V3 x y z -> ...

```

#### Type and Interface Definitions
Typesets are defined using a unified `type` keyword / macro that works as
follows:

1.  If you provide the keyword with only a name and fields, it generates an
    atom:

    ```ruby
    type Just value
    ```

2.  If provided with a body containing atom definitions, it defines a smart
    constructor that defines the atoms and related functions by returning a
    typeset. For example:

    ```ruby
    type Maybe a
      use Nothing
      type Just (value : a)

      isJust = case self of
        Nothing -> False
        Just _ -> True

      nothing = not isJust
    ```

    Translates to:

    ```ruby
    maybe a =
      atom Just value
      { (Nothing | Just a)
        & isJust: IsJust = isJust
        & nothing : Nothing = nothing }

    isJust : Maybe a -> Bool
    isJust self = case self of
      Nothing -> False
      Just _ -> True

    nothing : Maybe a -> Bool
    nothing = not isJust
    ```

3.  Though all types are interfaces, interfaces that define specific atoms are
    often not particularly useful. To this end, you can use the `type` keyword
    _without_ defining any atoms in the body to create a more-useful interface
    definition.

    ```ruby
    type HasName
      name: String

    printName: t:HasName -> Nothing
    printName t = t.name

    type Human name
    name (self: Int) = "IntegerName"

    main =
        printName (Human "Zenek")
        printName 7
    ```

4.  Explicit constraints can be put on the `self` type in a typeset, should it
    exist. This uses standard type-ascription syntax.

    ```ruby
    type Semigroup
      <> : self -> self

    type Monoid
      self : Semigroup
      use Nothing
    ```

Under the hood, typesets are based on GADT theory, and typing evidence is
_always_ discharged through pattern matching. This feature will not, however,
be available until we have a type-checker.

> The actionables for this section are as follows:
>
> - Determine what the nested `type Foo (a: t)` syntax actually means.
> - How do you define functions on (or a constructor for) one of the sub type
>   definitions.
>
>   ```ruby
>   type Foo
>     type Bar value
>       fnOnBar : Bar -> Bool
>   ```
>
> - Determine the exact details of how these definitions expand to typesets.

#### Anonymous Typesets
Given that typesets are unified structurally, it can often be very useful to
define interfaces as typesets in an ad-hoc manner while defining a function. To
this end we provide the `{}` syntax.

This syntax declares the members of the typeset explicitly. Member definitions
are of the form `name : type = default`, where the following rules apply:

- If a default is explicitly requested it becomes part of the subsumption
  judgement.
- If the type of a name is omitted it is inferred from a default if present, and
  is otherwise inferred to be `Any`.
- If only the type is present, auto-generated labels are provided using the
  index of the member in the typeset (e.g `{ Int & String }.1` has type `Int`).

The reason that the version with only the type is useful is that it means that
anonymous typesets subsume the uses for tuples.

> The actionables for this section are as follows:
>
> - Create examples of why anonymous typeset syntax is useful.
> - Decide if we want to support anonymous typesets.

#### Typeset Projections (Lenses)
In order to work efficiently with typesets, we need the ability to seamlessly
access and modify (immutably) their properties. In the context of our type
theory, this functionality is known as a _projection_, in that it projects a
value from (or into) a typeset.

Coming from Haskell, we are well-versed with the flexibility of lenses, and
more generally _optics_. To that end, we base our projection operations on
standard theories of optics. While we _do_ need to formalise this, for now we
provide examples of the expected basic usage. This only covers lenses, while in
the future we will likely want prisms and other more-advanced optics.

```ruby
type Engine
  type Combustion
    power:          Int
    cylinder_count: Int

  type Electric
    power:   Int
    is_blue: Bool


type Vehicle
  type Car
    color:     String
    max_speed: Int
    engine:    Engine

  type Bike
    color: String


type Person
  type Cons
    name:    String
    vehicle: Vehicle


main =
  p1 = Person.Cons "Joe" (Vehicle.Car 'pink' 300 (Engine.Combustion 500 8))
  print $ p1.name                   # -> Joe
  print $ p1.vehicle.color          # -> pink
  print $ p1.vehicle.max_speed      # -> Some 300
  print $ p1.vehicle.engine.power   # -> Some 500
  print $ p1.vehicle.engine.is_blue # -> None
  p1.vehicle.color     = 'red'      # OK
  p1.vehicle.max_speed = 310        # FAIL: security reasons. Allowing this
                                    #       in Haskell was the worst decision
                                    #       ever. After refactoring it
                                    #       silently does nothing there.

  p2 = p1.vehicle.max_speed    ?= 310 # OK
  p3 = p1.vehicle.engine.power  = 510 # FAIL
  p4 = p1.vehicle.engine.power ?= 510 # OK

  lens_name      = .name
  lens_color     = .vehicle.color
  lens_max_speed = .vehicle.max_speed
  lens_power     = .vehincle.engine.power

  ## Function like usage:
  print $ lens_name      p1
  print $ lens_color     p1
  print $ lens_max_speed p1
  print $ lens_power     p1

  p1 . at lens_name = ... # OK
```

> Actionables for this section:
>
> - Work out whether standard optics theory with custom types is sufficient for
>   us. We may want to support side effects.
> - Fix the example above. It isn't correct.
> - Determine how much of the above we can support without a type-checker. There
>   are likely to be a lot of edge-cases, so it's important that we make sure we
>   know how to get as much of it working as possible.

##### Special Fields
We also define special projections from typesets:

- `index`: The expression `t.n`, where `n` is of type `Number` is translated to
  `t.index n`.
- `field`: The expression `t.s` where `s` is of type `Text` is translated to
  `t.fieldByName s`.

### Implementing Interfaces
As typesets are matched structurally, types need not _explicitly_ implement
interfaces (a form of static duck-typing). However, when defining a new type, we
may _want_ to explicitly say that it defines an interface. This has two main
benefits:

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

> The actionables for this section are:
>
> - Work out what it means for an _atom_ to conform to an interface.

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
  to : t
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
> - Work out how much of this interface can be supported without a type checker
>   and type inference engine.

### Pattern Matching
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
    for both atoms and typesets.

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
> - Refine the syntax for the name-based case
> - Provide code examples for why the renaming use-case is important.

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
> - The definition of specificity for dispatch candidates (including how it
>   interacts with the subsumption relationship on typesets and the ordering of
>   arguments).
> - Do we want to treat the module as an argument upon which dispatch can happen
>   or is it something else?
> - Work out the whole self-initialization thing as Wojciech needs to think
>   about the problems with this system and uniformity / ambiguity.

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

If we want to support equivalents to multi-parameter type-classes in Haskell,
then we need to support multiple dispatch globally, as our method dispatch is
not opt-in (unlike typeclasses in Haskell).

#### Overlappable Functions
Overlappable functions is a proposal for obtaining multiple-dispatch-style
behaviour that dispatches functions based on a notion of specificity that is
a little more specific than general multiple-dispatch. It considers only local
bindings as candidates, and is hence not part of the global dispatch mechanism.

Consider the following example:

```ruby
foldFields: (f: field -> out) -> struct -> List out
foldFields f struct =
  go: List out -> t (a: field) -> List out
  go out (t a) = go (f a, out) t

  go: List out -> t -> List out
  go out _ = out

  go Nothing struct
```

It defines a set of inner functions that have types that are both _explicit_ and
_overlapping_. We then use a notion of specificity for these functions to
determine which to dispatch to.

Please note that the function `f` is not typed as `f : Any -> Any` because then
this would not work correctly. We are allowed to provide any valid sub-type
(see `<:` in the section on [typesets](#typesets) above) of a given type to a
function (while accounting for covariance and contravariance). In this example,
we want to provide a function that traverses all of the arguments and also want
the user to be able to pass an `f : Int -> String`, the type system needs to
verify that every field is of type `Int`.

Please note that there is a potential syntactic ambiguity here: `t a = v`. This
could either be interpreted as a function definition or structural pattern
matching. Fortunately, the latter is not needed often, and will only be needed
by advanced users. Instead, we require that structural pattern matching use
parentheses around the match `(t a) = v`.

> The actionables for this section are:
>
> - Do we really need this feature? Isn't it subsumed by the more useful notion
>   of multiple dispatch?
> - How does the above notion of structural pattern matching work in relation to
>   structural pattern matching for typesets?

### First-Class Modules
It is important in Enso for modules to be first-class language entities that can
be computed upon at runtime. However, we do not yet have a concrete design for
how to handle this. There are two main ways to do this:

- Unify the concept of modules with the concept of typesets, with some file
  scope magic to make this usable.
- Make modules their own first-class entity.

#### Self-Initialisation and Qualified Access
This is a proposal for how to handle qualified names and dispatching on the
module as an entity at runtime. There are two proposed sets of rules that are
intended to allow code like the example to work properly. They are both based on
the following idea:

```ruby
Int.inc                   = self + 1 ## is just a sugar to:
inc (self:Int)            = self + 1 ## which is a sugar to:
inc (module:A) (self:Int) = self + 1 ## which is the final form.
```

The first set of rules is as follows:

1.  When referenced in the same file `module` is applied automatically.
2.  When used implicitly (e.g. `5.inc`), `module` is also applied.
3.  When imported, like `import A` and used explicitly, the module is just an
    argument (e.g. `x = A.inc 5` or `x = inc A 5`).
4.  You are not allowed to define in a name multiple times (this precludes true
    dynamic dispatch).

The second set of rules is as follows:

1.  When `inc` is provided with `A` explicitly, then it is passed as an
    argument.
2.  In other cases, `A` is passed automatically.
3.  You are not allowed to define in a name multiple times (this precludes true
    dynamic dispatch).

The example code follows.

```ruby
## A.enso ##
def inc (self:Int) = self + 1

## B.enso ##
import A
print $ 5.inc
print $ inc 5
print $ A.inc 5
print $ inc A 5
```

## Broken Values
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

## Function Composition
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

> The actionables for this section are:
>
> - Work out how to do dynamic properly, keeping in mind that in a dynamic value
>   could self-modify underneath us.

### The Enso Boundary
Fortunately, we can at least avoid foreign languages modifying memory owned by
the Enso interpreter. As part of the interop library, Graal lets us mark memory
as read-only. This means that the majority of data passed out (from a functional
language like Enso) is not at risk. However, if we _do_ allow data to be worked
with mutably,

### An Insufficient Design For Dynamic
The following text contains a design for dynamic that doesn't actually account
for all the necessary use-cases in the real world. It is recorded here so that
it may inform the eventual design.

- `Dynamic a b` is a type where the `a` is used to provide a specification of
  the structure expected from the dynamic type, and the `b` is a specification
  of the _verified_ properties of the dynamic type.

  ```
  x : Dynamic { foo : Int -> String & prop : Int } {}
  ```

  This structure _need not be complete. Indeed, it will be fairly common to get
  values of type `Dynamic` about which we know nothing: `Dynamic {} {}`. As
  dynamic values are used, we can refine information about these values via
  pattern matching (moving properties from `a` into `b`), or adding properties
  to both if they hold.

- `Dynamic` has a constraint on the types of `a` and `b` such that `b` <: `a`
  where `<:` is assumed to be a subsumption relationship.

- The key recognition is that if a property is not contained in the `b`
  structure of a dynamic, it will be a type error to use that property.

- A value of type `Dynamic a b` can be converted to a value of type
  `Dynamic a a` by calling a method
  `assertValid : Dynamic a b -> Maybe (Dynamic a a)` on it that verifies the
  expected properties of the dynamic.

- A value of type `Dynamic a b` can be converted to a value of type `b` by
  calling `valid : Dynamic a b -> b`. This allows you to take the _verified_
  properties of your dynamic value and hoist it all into the type system safely.

- Users should also be able to `unsafeAssertValid` or something equivalent,
  which is an unsafe operation that treats a value of type `Dynamic a b` as
  having type `Dynamic a a`. Unlike `assertValid` above, this method performs
  _no_ verification.

Now, it can sometimes be useful to treat normal types as `Dynamic` values. For
this we have the following:

- A method on all structural types `asDynamic`. For a structural type t, it
  produces a value of type `Dynamic t t`.

- `Dynamic` provides a method that allows for defining functions and properties
  on dynamic values. This method is _safe_ such that for a `Dynamic a b` it
  extends the structural types `a` and `b` with the new properties:

  ```
  define : Dynamic a b -> prop : t -> Dynamic {a & prop : t} {b & prop : t}
  ```

The idea here is to use the two type variables to allow the user to understand
both what they _think_ the dynamic should provide, but also allow the type
system to track what the dynamic _does_ provide at any given moment.

This doesn't work in the face of types that can self-modify, meaning that there
is no performant way to work with dynamics short of unsafe assumptions about
them.

## The Main Function
The entry point for an Enso program is defined in a special top-level binding
called `main` in the file `Main.enso`. However, we also provide for a scripting
workflow in the form of `enso run`, which will look for a definition of `main`
in the file it is provided.
