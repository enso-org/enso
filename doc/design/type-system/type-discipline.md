___
- **Feature Name:** Modules
- **Start Date:** 2018-06-26
- **Change Type:** Breaking
- **RFC Dependencies:** Syntax Overhaul
- **RFC PR:**
- **Luna Issue:**
- **Implemented:**

# Summary
Any sensible programming language must provide its users with some way to
modularise their code, splitting it up into well-defined parts. The ML family of
languages, in particular 1ML and OCaml take the concept of a module further and
Luna, with its unique category-based type system, is in a position to provide
the most flexible implementation of modules yet, unifying the concepts of
modules, classes and interfaces.

<!-- MarkdownTOC levels="1,2,3" autolink="true" -->

- [Motivation](#motivation)
  - [\(Brief\) Type-System Primer](#brief-type-system-primer)
  - [A Note on Syntax](#a-note-on-syntax)
- [Reference-Level Explanation](#reference-level-explanation)
  - [Declaring Types](#declaring-types)
    - [Why a New Keyword?](#why-a-new-keyword)
  - [Unifying Types, Modules and Interfaces](#unifying-types-modules-and-interfaces)
    - [Types as Generics](#types-as-generics)
    - [Types As Modules](#types-as-modules)
    - [Types As Interfaces](#types-as-interfaces)
    - [Constructor Naming](#constructor-naming)
  - [Importing Types](#importing-types)
    - [Scoping Rules and Code Modularity](#scoping-rules-and-code-modularity)
  - [Anonymous Types](#anonymous-types)
    - [Anonymous Types as Types](#anonymous-types-as-types)
    - [Anonymous Types as Values](#anonymous-types-as-values)
  - [Constructors and Primitive Typing](#constructors-and-primitive-typing)
    - [The Reasoning for This](#the-reasoning-for-this)
  - [Nested Types](#nested-types)
  - [Example - Dependent Vector](#example---dependent-vector)
  - [Example - Linked List](#example---linked-list)
- [Type Conversions](#type-conversions)
  - [Convertible](#convertible)
  - [Coercible](#coercible)
- [Principles for Luna's Type System](#principles-for-lunas-type-system)
- [Structural Type Shorthand](#structural-type-shorthand)
- [Interfaces](#interfaces)
- [Desugaring Types to Rows](#desugaring-types-to-rows)
- [Testing the Type System](#testing-the-type-system)
- [Unresolved Questions](#unresolved-questions)
- [Steps](#steps)
- [Goals for the Type System](#goals-for-the-type-system)
- [References](#references)

<!-- /MarkdownTOC -->

# Motivation
At present, Luna has nothing but a rudimentary module system to aid in the
complexity management and scoping of code. For a sophisticated language this is
a bad state of affairs, and so this RFC aims to propose a redesign of certain
portions of Luna to present a _unified_ design which unifies modules (in both
the conventional and ML senses), classes, and interfaces under a single
first-class umbrella.

In doing so, the proposal supports a diversity of use-cases, allowing the
first-class manipulation of types, including the creation of anonymous types. In
doing so, it provides users with first-class modularity for their code, and
intuitive mechanisms for working with types in Luna's type system. This concept
thus brings a massive simplification to the Luna ecosystem, providing one
powerful mechanism to accomplish many key language features, without requiring
the user to understand more than the mechanisms of `type`, and the principle
behind Luna's type system.

In the end, Luna's current module system is insufficient for serious development
in the language, and needs to be replaced. In doing so, this RFC proposes to
take the time to bring a vast simplification to the language in the same swoop.

## (Brief) Type-System Primer
Luna's type system is based on the notion that each type is a name for a set of
_values_ (denoted by _constructors_). This makes the type system a
[Modular Lattice](https://en.wikipedia.org/wiki/Modular_lattice), if you're
interested. For an example, the type `Nat` contains constructors `1, 2, 3, ...`,
and is hence denotable by a set of its constructors.

As a result, typechecking doesn't work via _unification_ as one might expect if
they are familiar with other functional programming languages, but instead
checks if a given set of values is a valid substitution for another. We can, of
course, have the empty set (∅), and also sets containing single elements
(single constructors).

This notion is supported by an enforced equivalence between value-level and
type-level syntax in Luna, as the compiler makes no distinction between the two.
This means that it is perfectly valid to type `a : vector 1 2 3`, a set with
a single member. The resultant flexibility is very intuitive, and gives Luna
a very useful type-system in practice, allowing a form of strong, structural
typing.

## A Note on Syntax
Any syntax used in this document is TBC, pending the completion of the
documentation for the new syntax. Once that takes place, this document will be
updated to reflect it.

# Reference-Level Explanation
This design proposes a major breaking change for Luna, wholesale replacing
portions of the Language's syntax and semantics with an entirely new model. As a
result, this RFC aims to describe the changes piece-by-piece, to help the
reader establish a more holistic idea of the design presented here.

## Declaring Types
Declaring a new type in this new system uses the newly-introduced keyword,
`type`. Type declarations can (but need not) have:

- A name (e.g. `type Vector`)
- One or more type variables (e.g. `type Vector n a`)
- Constraints on type variables (e.g. `type (n : Nat) => Vector n a`)
- Interface annotations (e.g. `type TreeMap k v : Map k v`)

The body of a type is more what you would imagine, containing data, functions,
and type constructors in a variety of forms. Constructors are special
declarations in the type body that name parameters and/or provide types. The
type constructor forms are as follows, and may be familiar from other
functional languages:

1. **Automatic Constructors:** Declaring a type with fields but no explicit
   constructor will generate a constructor named as for the class. The following
   type, for example, has an automatic constructor called `point3D`, with type
   `Double -> Double -> Double -> Point3D`:

    ```
    type Point3D =
        x : Double
        y : Double
        z : Double
    ```

2. **Explicit Constructors:** Users can also declare constructors for their
   types explicitly, akin to being able to define multiple constructors to
   produce a sum type in Haskell. An example follows:

    ```
    type MsgPack =
        mpNull
        mpBool   Bool
        mpInt    Int
        mpString Text

    ```
   As you can see here, some of the constructors (e.g. `mpBool`) define the
   types expected of their arguments.

3. **Constructors with Named Arguments:** It is also possible to declare
   multiple constructors for a type where each of the arguments is given a name.

    ```
    type (a : Numeric) => Vector a =
        vector (x : a) (y : a) (z : a)
        scalar (w : a)
    ```

The body of a type can contain functions, data, or even _other types_, and _yes_
because you were wondering, types _can_ be defined inductively or using a GADT
style.

Above all this, however, the nature of Luna's type system means that types can
be declared in a way entirely unfamiliar from other functional languages. As
described in the [Type-System Primer](#brief-type-system-primer), Luna's type
system operates on _collections of values_. As a result, we provide a mechanism
to give your own collections names.

```
type MyNewType = Type1 | Type2 | Type3
```

Such a declaration can, of course, have constraints and type variables, much
like any of the more standard type declarations above. It cannot, however,
provide explicit lists of implemented interfaces.

### Why a New Keyword?
While it would've been possible to use an existing keyword for this unified
concept, it was felt that existing keywords such as `module` and `class` carried
too much baggage from their uses elsewhere. The chosen `type`, however, is very
explicit as it describes exactly what it does in Luna. Furthermore, with this
proposal `module ~ class ~ interface`, and all are members of the type-universe
`Type`, making it an even more appropriate choice.

## Unifying Types, Modules and Interfaces
Types declared in the above manner are able to operate as all of what one would
think of as conventional classes, modules and interfaces:

- **Classes:** Types provide containers for data and behaviour.
- **Modules:** Provide namespacing for code and data, and also allow for
  genericity in the implementation of types.
- **Interfaces:** Provide descriptions of behaviour required of a type, allowing
  for parametric polymorphism via abstraction.

All of these functionalities are provided by `type`, resulting in a highly
flexible language construct.

At a fundamental level, the definition of a new `type` in Luna is the creation
of a (usually named) category of values described by the data and behaviour it
possesses. These are first-class values in Luna, and can be created and
manipulated at runtime. It is possible, then, to dynamically determine a map
implementation to use based on runtime data. For example:

```
EfficientMap k v =
    if expectedBuckets > threshold
    then HashMap k v
    else TreeMap k v
```

At first glance, however, Luna types may seem like conventional _classes_ from
traditional object-oriented languages, but Luna types have significantly more
power.

### Types as Generics
To start off, types in Luna can be made _generic_ over other types. This can be
as simple as `Map k v`, where `k` is the type of the key and `v` is the type of
the value, but actually has more sophisticated use-cases.

One of these is to be able to parametrise your types over different
implementations of functionality, such as strings. This means that your type can
be generic over anything that implements the required interface, as shown in the
following example:

in `Util/Logger.luna`:

```
type (a : Textual) => Logger a =
    ...
```

in `Main.luna`:

```
import Data.Text
import Util.Logger Text

...
```
This means that in the scope of `Main.luna`, any instance of the `Logger` type
will use `Text` as its underlying implementation. This works because `Text` is
an instance of the `Textual` interface.

Another useful extension of this is that type arguments to generic types can be
partially applied via currying. If, for example, we have a `Map k v`, we can
produce a `StringMap = Map String` just by applying one of the type
arguments. This is equivalent to explicit specification of the free type
variable: `StringMap v = Map String v`.

### Types As Modules
The same notion of a type can be used to provide the functionality that is
traditionally expected of a _module_ (in the common, not ML sense). In most
programming languages, their module system provides a mechanism for code-reuse
through grouping and namespacing.

Indeed, Luna's types provide both of these functionalities:

- **Grouping of Code:** A `type` declaration in Luna acts as a container for
  code, with functions able to be declared in its scope.
- **Namespacing:** Unless otherwise declared (through a direct import
  statement), a `type` in Luna also provides a namespace to constructs declared
  inside its scope.

These are both best illustrated by example. Consider the following type. If it
is imported simply as `import Convert` (see
[Importing Types](#importing-types)), then `id` and `convert` are only
accessible within the scope of `Convert` (e.g. `Convert.convert`).

```
type (a : Coercible b) => Convert a b =
    id : a -> a
    convert : a -> b
```

It may seem odd at first glance to be considering a _module_ without an
implementation, but this is just an interface contained in its own scope!

### Types As Interfaces
A type in Luna can also act as a 'contract', a specification of the behaviour
expected of a type. The use of types as interfaces in Luna is, as you might
expect, contravariant. As long as the type satisfies the category defined by
the interface, it can be used in its place. This leads to the expected semantics
where a type `Foo` implementing `Bar` can be used where a `Bar` is expected.

Interfaces in Luna can range from general to very specific. As they define a
_category_ of values, interfaces can specify anything from function signatures
that must be present, all the way to names that must be present in the type's
scope and default behaviour. The following are all valid ways to define types
for use as interfaces in Luna.

```
# This interface requires a function called someFunction with the correct sig.
type Interface1 =
    someFunction : Int -> String

# This interface requires a function and a variable both named appropriately.
type (a : Numeric) => Interface2 a =
    someVar : a

    someFunction : a -> a
    someFunction = ...

# This interface requires a function foo with the appropriate type.
type Interface3 a = { foo : a -> a }
```

For more information on the last example, please read the section on
[anonymous types](#anonymous-types).

#### Implementing Interfaces
The nature of Luna's type system means that any type that _satisfies_ an
interface, even without explicitly implementing it, will be able to be used in
places where that interface is expected. However, in the cases of named
interfaces (not [anonymous types](#anonymous-types)), it is a compiler warning
to do so.

You can explicitly implement an interface in two ways. Examples of both can be
found at the end of the section.

1. **Implementation on the Type:** Interfaces can be directly implemented as
   part of the type's definition. In this case the type header is annotated with
   `: InterfaceName` (and filled type parameters as appropriate). The interface
   can then be used (if it has a default implementation), or the implementation
   can be provided in the type body.
2. **Standalone Implementation:** Interfaces can be implemented for types in a
   standalone implementation block. These take the form of `instance Interface
   for Type`, with any type parameters filled appropriately.

Both of these methods will support extension to automatic deriving strategies in
future iterations of the Luna compiler.

It should also be noted that it is not possible to implement orphan instances of
interfaces in Luna, as it leads to difficult to understand code. This means that
an interface must either be implemented in the same file as the interface
definition, or in the same file as the definition of the type for which the
interface is being implemented.

Consider an interface `PrettyPrinter` as follows, which has a default
implementation for its `prettyPrint` method.

```
type (a : Textual) => PrettyPrinter a b =
    prettyPrint : b -> a
    prettyPrint item = baseShow item
```

For types we own, we can implement this interface directly on the type. Consider
this example `Point` type.

```
type Point : PrettyPrinter Text Point =
    x : Double
    y : Double
    z : Double

    prettyPrint : Point -> Text
    prettyPrint self = ...
```

If we have a type defined in external library that we want to pretty print, we
can define a standalone instance instead. Consider a type `External`.

```
instance PrettyPrint Text External for External =
    prettyPrint : External -> Text
    prettyPrint ext = ...
```

#### On the Semantics of Standalone Implementations
Standalone implementations allow for limited extension methods on types. The
interface methods implemented for a type in the standalone definition can be
used like any other method on a Luna type.

#### Overlapping Interface Implementations
Sometimes it is beneficial to allow interfaces to overlap in one or more of
their type parameters. This does not mean Luna allows _duplicate_ instances (
where all of the type parameters are identical). These can be implemented by
either of the methods above, but the user may often run into issues when
attempting to make use of these interfaces.

Luna thus provides a mechanism for the programmer to manually specify which
instance of an interface should be selected in the cases where resolution is
ambiguous. Consider the following example, using the `PrettyPrinter` interface
defined above.

```
type Point2D : PrettyPrinter Text Point2D, PrettyPrinter ByteArray Point2D =
    x : Double
    y : Double

    prettyPrint : Point2D -> Text
    prettyPrint self = ...

    prettyPrint : Point2D -> ByteArray
    prettyPrint self = ...

loggerFn (a : PrettyPrinter b) -> Text -> a -> Text
loggerFn msg item = msg <> prettyPrint(Text) item
```

As you can see, the syntax for specifying the instance in the ambiguous case
uses parentheses to apply the type to the `prettyPrint` function.

### Constructor Naming
This proposal has made a change to the naming of constructors, making them have
lower-case names, like any other functions. This produces an elegant unification
between the standard constructors and any smart constructors that might be
created.

This does, however, require a change to the pattern matching syntax. Now, all
constructors in pattern matches are required to be wrapped in parentheses.
Consider the following example.

```
case conDecl of
    impCon@(implicitConstructor name type)    -> ...
    userCon@(userConstructor name (type sig)) -> ...
```

This is not entirely elegant, even though it is consistent, and is open to
improvement. The rationale behind the renaming of constructors is to make it
much simpler to work with types where constructor names are the same as the
type name. If this change wasn't made, you would be forced to pattern match on,
and construct with, `Type.Type` as the constructor, which is _very_ inelegant.

## Importing Types
To go along with the new system proposed in this RFC around code modularity,
the syntax for dealing with imports has been tweaked slightly. The following
import syntaxes are valid:

- **Direct Imports:** These import the primary module from the file. This brings
  the type and its constructors into scope. For example `import Data.Map` would
  bring `Map` and its constructors into scope.
- **Specified Imports:** These allow the specification of additional functions
  to be brought into the current scope. For example `import Data.Map: fromList`
  would bring `Map`, its constructors and `fromList` into scope.
- **Renamed Imports:** These allow for the programmer to rename the imported
  type. For example `import Data.Containers.Map as MapInterface` brings `Map`
  into scope named as `MapInterface`. Here, constructors are also imported.
- **Specialised Imports:** These allow the programmer to specialise type
  arguments as part of the import. For example `import Data.Map String` will
  import `Map` and its constructors with their first type arguments specialised
  to `String`.

These above import styles can be combined, for example renaming a partially
specialised import (`import Data.Map String as StringMap`), or specialising
functions imported into scope (`import Data.Map String: fromList`). Much like
curried type application seen elsewhere in this proposal, it is possible to
partially apply the type arguments of an import, as seen above.

#### The File Scope
Files in Luna should contain at least one `type`, with one type named the same
as the file. This `type` is known as the 'primary' type, and it is this type
that is referred to when importing the 'module'. A file `Data/Map.luna` may
contain `type Map`, `type Helper` and various other types, but the only things
visible outside the file are the primary type and things defined in its scope.
Inside the file, however, everything can be seen, with no need to
forward-declare.

### Scoping Rules and Code Modularity
Imports in Luna can be performed in _any_ scope, and are accessible from the
scope into which they are imported. This gives rise to a particularly intuitive
way of handling re-exports.

Consider the following file `Test.luna`. In this file, the imports of `Thing`
and `PrettyPrint` are not visible when `Test.luna` is imported. However,
`PrettyPrint` and `printer` are made visible from within the scope of `Test`.
This means that a user can write `import Test: printer` and have it work.

```
import Experiment.Thing
import Utils.PrettyPrint

type Test a : PrettyPrint Text (Test a) =
    import Utils.PrettyPrint: printer

    runTest : a -> Text
    runTest test = ...

    prettyPrint : Test a -> Text
    prettyPrint self = ...
```

## Anonymous Types
In addition to the syntax proposed above in [Declaring Types](#declaring-types),
this RFC also proposes a mechanism for quickly declaring anonymous types. These
types are anonymous in that they provide a category of values without applying
a name to their category, and can be created both as types and as values.

While it is possible to use the primary type declaration syntax without
providing an explicit name, this is highly impractical for most places where an
anonymous type becomes useful. This shorthand provides a way to get the same
benefit without the syntactic issues of the former.

A key, and potentially obvious feature of anonymous types is that they can allow
for polymorphic records in Luna. All types are backed by polymorphic records,
and row-extension is a primitive operation with this new module system. Luna
would not be the first to embrace row extension, lacks constraints, and truly
polymorphic records as a primitive (e.g. [Expresso](https://github.com/willtim/Expresso))
but would likely be the first to combine it with a unified module and type
system. A few thoughts:

- Most languages with polymorphic extensible rows default to making those
  records _closed_, while providing a construct to make them open.

    ```
    foo : (r\x, r\y) => {x : a, y : b | r} -> a
    ```

- Such a construct is the wrong default for Luna, where records should be open
  by default. As a result, we default to open rows:

    ```
    foo : {x : a, y : b} -> a
    ```
  In Luna, a type signature as above actually has an internal meaning that is
  identical to the previous type signature.
- Given that we want to default to open records, we should provide some
  syntactic nicety for defining closed records in a function type, even though
  the internal type-inference will default to open in the absence of a type
  signature.
- With rows as the primitive, we can use them to back other interfaces, such as
  tuples, heterogeneous lists, classes, interfaces, and modules.
- A record type signature is effectively an interface, and with first-class
  functions existing in Luna, a type's interface can succinctly be described by
  a record type.
- This ties straight back in to the categorical typing nature of Luna, with a
  record type trivially defining a category of values.

Luna, however, is planned to be a dependently-typed language. This means that
the underlying type theory will need support for dependent records. This is not
a particular point of difficulty in itself, with multiple dependent type
theories having support (e.g. [Quantitative Type Theory](https://bentnib.org/quantitative-type-theory.html)),
but must be accounted for.

Support is generally fairly simple in dependent type theory, as polymorphic
records translate to product types, on which dependent constraints are trivially
encoded.

### Anonymous Types as Types
When used in a type context, an anonymous type acts as a specification for an
interface that must be filled. This specification can contain anything from
types to names, and features its own syntax for inline declarations.

Consider the following examples:

- `{Int, Int, Int}`: This type declares a set of values where each value
  contains three integers.
- `{Int, foo : Self -> Int}`: This type declares a set of values with an integer
  and a function from `Self` to an Integer with name `foo`.
- `{Self -> Text -> Text}`: This defines an unnamed function. This may seem
  useless at first, but the input argument can be pattern-matched on as in the
  following example:

    ```
    foo : { Int, Int, Self -> Int } -> Int
    foo rec@{x, y, fn} = fn rec
    ```

`Self` is a piece of reserved syntax that allows anonymous types to refer to
their own type without knowing its name.

### Anonymous Types as Values
Anonymous types can also be constructed as values using similar syntax. You can
provide values directly, which will work in a context where names are not
required, or you can provide named values as in the following examples:

- `{0, 0}`: This anonymous value will work anywhere a type with two numbers and
  no other behaviour is expected.
- `{x = 0, y = 0, z = 0}`: This one provides explicit names for its values, and
  will work where names are required.
- `{x = 0, fn = someFunction}`: This will also work, defining the value for `fn`
  by use of a function visible in the scope.
- `{x = 0, fn = (f -> pure f)}`: Lambda functions can also be used.

## Constructors and Primitive Typing
While types in Luna provide categories of values, the constructors are the
values themselves. As Luna is a dependently-typed language with no distinction
between value- and type-level syntax, we are allowed to write _very_ specific
types.

Consider a type as follows:

```
type Point a
    x : a
    y : a
    z : a
```

This generates an automatic constructor of type `point : a -> a -> a`. So far,
this is standard, with little out of the ordinary. With Luna, however, if we
construct `point 1 2 3`, it can be typed as `Point Int`, but it can also be
typed as `point 1 2 3`. This is key to the nature of the type-system in Luna,
where values can be decomposed into their primitive constructors. `point 1 2 3`
can thus be thought of as a 'sub-type' of `Point Int`.

### The Reasoning for This
This may seem strange at first, but it's actually resultant from the simple
principles that underlie the Luna type-system. The reasoning for this behaviour
is as follows.

1. Luna lets us define types with constructors, be they explicit or implicit.

    ```
    type Point a
        x : a
        y : a
        z : a
    ```

2. In Luna, the value-level syntax is identical to the type-level syntax, so
   all of the following expressions are syntactically valid (and valid as types
   too):

    ```
    v = point 1 2 3 :: point 1 2 3
    v = point 1 2 3 :: point Int Int Int
    v = point 1 2 3 :: Point Int
    foo :: Point Int -> Int
    foo :: point 1 2 3 -> Int
    foo :: point Int Int Int -> Int
    ```

3. If (2) is correct then we've got a type `point` and we can use it
   everywhere other types are usable. In particular, it can be used as follows:

    ```
    type Foo = point Int Int Int
    type Foo a b = point a b a
    ```

4. If (3) is correct, then Luna's type system results in a type `point` which
   allows types to be passed as its values. This is the same constructor used
   to construct values of a type, but instead used in a type context.

## Nested Types
As any kind of language construct can be nested inside a `type`, we are able to
nest types arbitrarily in Luna. This leads to a very expressive language, but
the semantics of such nested types need careful attention.

This RFC proposes that a nested type becomes a separate type held by each
_instance_ of a type. This means that `foo.A != bar.A`, where `foo : Foo` and
`bar : Foo`. The benefits of doing this means that you have true associated
types, rather than just types contained by other types.

## Example - Dependent Vector
```
type (n : Nat) => Vector n a =
    # Constructor
    vector : (n = 1 : Nat)

    # The parameter name is visible at the 'type level', allowing dependent type
    mkVec : Nat -> Vector n a
    mkVec (n = 1) = ...
```

## Example - Linked List
This example uses currently forbidden syntax for declaring operators, but let's
pretend it works.

```
type List a =
    nil
    cons a (List a)

    (:) = cons
    []  = nil
```

# Type Conversions
Like in any programming language, Luna requires the ability to convert between
types. Sometimes these conversions have to happen at runtime, incurring a
computational cost, but sometimes these conversions can be 'free', in cases
where the compiler can prove that the types have the same representation at
runtime. Enter the `Coercible` and `Convertible` mechanisms.

## Convertible
There is a tension in Luna's design around conversions between types. In many
cases, our users performing data analysis will just 'want it to do the right
thing', whereas users writing production software will likely want control.

The resolution for this tension comes in the form of `Convertible`, one of the
wired-in interfaces in the compiler. This type is defined as follows, and
represents the category of runtime conversions between types. As these
conversions must take place at runtime, they are able to perform computations.

```
type Convertible a b:
    convert : a -> b

convertVia : <what goes here> => (t : [Type]) -> a -> b
```

The fact that `Convertible` is wired in lets the compiler treat it in a special
fashion. When it encounters a type mismatch between types `A` and `B`, the
compiler is able to look up all instances of `Convertible` to see if there is a
matching conversion. If there is, it will be automatically (invisibly) inserted.

Now this initially sounds like a recipe for a lack of control, but there are a
few elements of this design to keep in mind:

- Due to the nature of Luna's type system and how default arguments count
  towards the saturation of a function, an instance of convert can actually be
  defined to be configurable. Consider the following example, which defines an
  instance with an alternative signature.

  ```
  instance Convertible Text File.Path for Text:
      convert : (a : Text) -> Bool -> (b : File.Path)
      convert in -> expandEnvVars = True -> ...
  ```

  Under such a circumstance, the implicit call uses the default, but if a user
  wants to configure or control elements of the conversion behaviour, then they
  can be explicit `convert (expandVars = True)`. In cases where the types need
  to be made explicit to ensure instance resolution, they too can be applied
  (`convert (a = Text) (b = File.Path) foo`).
- This mechanism is unobtrusive in exploratory code, and contributes to the idea
  that things 'just work'.
- It is accompanied by an optional warning `-Wimplicit-conversions` that warns
  when a conversion is made without an explicit call to `convert`. This ensures
  that users can opt in to having more feedback as their codebase evolves from
  exploration to development. This warning will be accompanied by an IDE
  protocol quick-fix that allows local (or global) insertion of explicit
  conversions.
- The `Convertible` type is represented as an interface because users may need
  to constrain the types of their functions based upon the ability to convert
  between two types `a` and `b`. When used in this case, there is of course no
  access to any defaulted arguments without extending the interface (see the
  discussion on row extension above).
- A call to `convert` will only be implicitly inserted when the two types to be
  converted between are explicitly known by the type-checker. It will _not_
  insert speculative calls.
- Conversion takes place at runtime at the last possible moment. This means that
  if `a : Foo` is convertible to `Bar`, with some function `f : Bar -> ...`, the
  call `f a` is implicitly rewritten `f (convert a)`.
- A call to `convert` is only inserted implicitly when the type mismatch can be
  resolved
- The function `convertVia` lets you give a hint to the compiler as to the type
  to convert through. This has a default implementation in every instance of
  the interface. It is _never_ inserted by the compiler.

Finally, you may be wondering about the quality of error messages that this can
produce in the case where there is a type mismatch and not enough information to
resolve the type variables. To this end, there has been some discussion about
making `convert` a reserved name, but this is not certain yet.

It is an open question as to _where_ we infer convertible.

## Coercible
It is often necessary, particularly when working with structs over the C-FFI or
in the case of embedded syntaxes, to need to be able to convert between types
with zero runtime cost. The `Coercible` mechanism provides a way to do this that
is safe and checked by the compiler.

This is a wired-in type in the compiler, and unlike for `Convertible` above, it
cannot be defined by users. Instead, the compiler will automatically generate
pairs of coercions between _types that have identical runtime representations_.

```
type Coercible a b:
    coerce : a -> b
```

Unlike calls to `convert`, calls to `coerce` are never inserted by the compiler.
This is due to the fact that, while two types may have the same runtime
representation, the semantics of these types can differ wildly.

Much like above, there are some elements of this design that bear stating
explicitly:

- `Coercible` is represented as an interface to allow users to parametrise their
  functions on the availability of a coercion between two types.

# Principles for Luna's Type System

- Types in Luna are functions on sets (constructors included), and are based on
  the theory of rows described in the "Abstracting Extensible Data Types" paper.

- All interface definitions must resolve to a 1:1 mapping between resolution
  type (e.g. `Text`) and result type (e.g. `Functor Char Char`), modulo type
  annotations.

- Scope lookup proceeds from function scope outwards, and the body is in scope
  for the signature. The signature is in scope for the body.

- There is no distinction between types and kinds. That means that all kinds
  (e.g. Constraint, Type, Representation) are Type (Type in Type).

- We want to provide the ability to explicitly quantify type variables, for all
  of: dependent, visible, required, and relevant (e.g. `forall`, `foreach`).

- It should be clear from a signature or pattern match in isolation which
  variables are implicitly quantified.

- Naming in Luna is case-insensitive, so to hoist a bare function to the type
  level, you are required to capitalise it. `foo` and `Foo` are the same thing
  in values, but when used in a type, the former will be inferred as a free var.

- Implicitly quantified variables are parsed as explicit but hidden.

- Applications of types are done via named arguments.

- All arguments are named.

- There is inbuilt support for inference of hole-fits.

- Default arguments are always applied when left unfilled. We provide a syntax
  `...` for preventing use of a default.

- Argument names at the type and term level must not shadow each other.

- There is no syntactic sugar for multiple argument functions.

- Contexts (e.g. IO) are represented using `T in IO`. Multiple contexts are
  combined as standard `(IO | State Int)`, and it is written the same in arg
  position. Monadic Contexts.

- Types define namespaces.

- A more generic type can be written than will be inferred.

- Contexts may be omitted when writing types.

- Laziness may be omitted when writing types, but has explicit syntax.

- Computation defaults to strict.

- Laziness and Strictness is controlled by the type.

- Type definitions are, by default, desugared to open polymorphic rows.

- Type definitions that do not include data members, do not have generated
  constructors.

- Type definitions that do include data members are given autogenerated
  constructors.

- The desugaring of all type definitions will include default implementations
  and values.

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

- Dependent types in Luna will desugar to an application of Quantitative Type
  Theory.

- Tuples are strictly a less powerful case of rows.

- The type system requires support for impredicative types and higher-rank
  types with inference.

- Passing a lazy type to a function expecting a strict one (i.e. one that is not
  strictness-polymorphic) should force the computation.

- The typechecker should support interleaving of metaprogramming and
  type-checking for elab-style scripts.

- Supermonadic theory, as a strictly more-powerful theory over standard monads,
  allows trivial definitions in terms of standard monads with inferred trivial
  contexts. Luna's Monadic Contexts are based on this theory.

- The context inference algorithm should support a high-granularity (e.g. read,
  write, FFI, print, etc).

- The base theory of rows that underlies the typechecker must support projection
  of values based on arbitrary types.

- All of the above concepts are represented as operations over row types.

- Row projections are first-class citizens in the language. They are based on
  the projection mechanism described in "Abstracting Extensible Data Types".

- When a function has defaulted arguments, these arguments should be treated as
  filled for the purposes of matching types. This point is somewhat subsumed by
  ones above, but bears making explicit. `f : A -> B` should match any function
  of that type (accounting for defaulted arguments).

- The type-system will support wired in `Convertible` and `Coercible` instances.
  The former deals with runtime conversions between types, while the latter
  deals with zero-cost conversions between types of the same runtime
  representation. Both can be inserted by the compiler where necessary.

- Luna will make use of whatever advanced type-system features are at its
  disposal to improve safety and performance.

- Complex errors will be explained simply, even if this requires additional
  annotation from programmers who use the advanced features.

- In the context of dependent types, relevance inference will be performed.

- The Luna typechecker will integrate an aggressive proof rewrite engine to
  automate as much of the proof burden as possible.

- Inference is employed heavily, letting types span from invisible to fancy,
  with each level providing as many guarantees as is practicable.

- Luna _is not a proof system_, and its implementation of dependent types will
  reflect said fact, being biased towards more practical usage.

- The future implementation of dependent types into Luna will be based on RAE's
  thesis about dependent types in Haskell (particularly PICO and BAKE).

- Inference will propagate as far as possible, but under some circumstances it
  will require users to write types.

- Value-level names become part of the function interface.

    ```
    replicate : (n : Nat) -> a -> Vector n a
    replicate = num -> val -> ...

    # Alternatively
    replicate : Nat -> a -> Vector sz a
    replicate = sz -> val -> ...
    ```

  The issue here is that after an `=`, you want a bare name to be a _name_, and
  after a `:`, you want it to represent a type. In the above example, `a` is a
  free type variable (`forall a`), while `n` is a name.

- If it comes to a tension between typechecker speed and inference capability,
  Luna will err on the side of inference capability in order to promote ease of
  use. Speed will be increased by performing incremental type-checking where
  possible on subsequent changes.

- 'Categorical Typing' (e.g. the `1 <: Int`) relationship, is supported by two
  key realisations:
  + Atoms can be represented as elements of a row.
  + Labels can be the atom they project (e.g. `1 : 1`) in the context of
    polymorphic labels.
  Thereby, when `Int : (1:1, 2:2, 3:3, ...)`, we trivially have `12:12 <: Int`.
  We just need to ensure that the 'subtyping' rules for the primitives are
  wired-in. This still supports `Vector3D 1 2 3 <: Vector3D Int Int Int`. A
  close examination of how this works with functions is required.

- We do not intend to support duplicate row labels, and will use appropriate
  constraints on combination and projection to achieve this.

- Deallocation of resources will be performed by 'drop', which can be explicitly
  implemented by users if need be (a wired-in interface).

- Record syntax is `{}`.

- We do not want to support invisible arguments.

- Luna must support nested type definitions. These nested types are
  automatically labelled with their name (so constructors are `mkFoo`, rather
  than `Foo`). The nested type is constructed as part of the containing type.

- When writing a method, the type on which the method is defined must be the
  last argument.

- Every name uses the following syntax `name : type = value`, where either
  `type` or `value` can be omitted. This has additional sugar that allows users
  to write it as follows:

    ```
    name : type
    name = value
    ```

  This sugar is most likely to be seen for function definitions, but it works in
  all cases.

- Rows in Luna are open, and have polymorphic projections. A projection without
  a given type is assumed to be a label, but `{ (Foo : Type) : Int }` lets users
  use other types as projections. A row `{ myLabel : Int }` is syntax sugar for
  an explicit projection based on a label: `{ (myLabel : Label) : Int }`.

- Luna features built-in first-class lenses and prisms based on row projections,
  as described in "Abstracting Extensible Datatypes".

- Rows where no explicit labels are given are assumed to be tuples. This means
  that the row `{Int, Int, Char}` is syntax sugar for a row with autogenerated
  labels for projection: `{ 1 : Int, 2 : Int, 3 : Char }`. Names of the labels
  are TBC. In doing this, you enforce the expected 'ordering' constraint of a
  tuple through the names.

- Bare types, such as `Int` or `a : Int` are assumed to be rows with a single
  member. In the case where no name is given, a it is treated as a 1-tuple.

- We want the theory to have possible future support for GADTs.

- The operation `foo a b` desugars to `b.foo a` so as to construct a
  transformation applied between two types.

- We want error messages to be as informative as possible, and are willing to
  retain significant extra algorithmic state in the typechecker to ensure that
  they are. This means both _formatting_ and _useful information_.

- There is a formalisation of how this system lowers to System-FC as implemented
  in GHC Core.

- Constructors are treated specially for the purposes of pattern matching.

- The implementation supports `Dynamic`, a type that allows typechecked
  interactions with external data. The properties expected of a `Dynamic` value
  become part of the `Dynamic` type signature, so if you need to access a
  property `foo` on a value `a` that is dynamic, `a` has type
  `Dynamic { foo : T }`, and thereby represents a contract expected of the
  external data. Dynamic is a strict subtype of `Type`.

- The typechecker will work efficiently in the presence of defaulted arguments
  by lazily generating the possible permutations of a function's signature at
  its use sites. This can be made to interact favourably with unification, much
  like for prolog.
- Users need to explicitly run their contexts to provide the lifting.

- It is going to be important to retain as much information as possible in order
  to provide informative error messages. This means that the eventual algorithm
  is likely to combine techniques from both W and M (context-insensitive and
  context-sensitive respectively).

- Type errors need to track possible fixes in the available context.

- There is explicit support for constraints appearing at any point in a
  polymorphic type.

- Type equality in Luna is represented by both representational and structural
  equality. Never nominal equality. There is no inbuilt notion of nominal
  equality.

- The type-system includes a mechanism for reasoning about the runtime
  representation of types. It will allow the programmer to constrain an API
  based upon runtime representations. While this is described as a type-level
  mechanism, it only is insofar as kinds are also types. The kind of a type is
  an expression that contains the following information:
  + Levity: Types that are represented by a pointer can be both lifted (those
    that may contain 'bottom') and unlifted (those that cannot). Thunks are
    lifted.
  + Boxiness: Whether the type is boxed (represented by a pointer) or unboxed.
  + Representation: A description of the machine-level representation of the
    type.

  This is effectively represented by a set of data declarations as follows:

    ```hs
    data Levity
        = Lifted   -- Can contain bottom (is a lazy computation)
        | Unlifted -- Cannot contain bottom (has been forced or is strict)

    data Size = UInt64

    data MachineRep
        = FlatRep [MachineRep] -- For unboxed rows
        | UInt32
        | UInt64
        | SInt32
        | SInt64
        | Float32
        | Float64
        | ...

    data RuntimeRep
        = Boxed   Levity
        | Unboxed MachineRep

    data TYPE where TYPE :: RuntimeRep -> ? -- Wired in

    -- Where does `Constraint` come into this?
    ```

  Doing this allows programs to abstract over the representation of their types,
  and is very similar to the implementation described in the Levity Polymorphism
  paper. The one change we make is that `FlatRep` is recursive; with most of
  Luna's types able to be represented flat in memory. This means that the list
  `[MachineRep]` is able to account for any row of unboxed types.

  The return type of `TYPE` is still an open question. What does it mean for a
  dependently typed language to deal in unboxed types at runtime? Reference to
  the Levity Polymorphism paper will be required. We don't want the usage of
  this to rely on the JIT for code-generation, as it should operate in a static
  context as well.

  An example of where this is useful is the implementation of unboxed arrays,
  for which we want a flat in-memory layout with no indirections. Being able to
  parametrise the array type over the kind `forall k. RuntimeRep (Unboxed k)`,
  means that the type will only accept unboxed types.

- We want to support contexts on types such that instantiation can be guarded.
  For more information see the Partial Type-Constructors paper. If you have a
  type `type Num a => Foo a = ...`, then it should be a type error to
  instantiate `Foo a` where `Num a` doesn't hold. This allows a treatment of
  partial data. However, this isn't easily extensible across interfaces. Could
  we propagate the constraint to the constructors in a GADT-alike? Nevertheless,
  they act as well-formedness constraints on the type definition. This means
  that the desugaring for type definitions involves GADTs. The construction of
  a constrained type creates evidence that is discharged at the type's use site
  (pattern match or similar). This should be based on the reasoning in the
  Partial Data paper, and so all types should automatically be generalised with
  the 'well-formed type' constraints, where possible.

- Type constructors are special entities. Not to be confused with value
  constructors `Vector Int` vs. `mkVector 1`.

- The syntax is as follows:
  + Row Alternation: `|`
  + Row Subtraction: `\`
  + Row Concatenation: `&`

- We should be able to infer variants and records, but this behaviour can be
  overridden by explicit signatures.

- With regards to Monadic Contexts, `in` allows nesting of contexts, as opposed
  to composition of transformers. `=` 'peels off' the last layer of contexts.
  When composing contexts, we have `&` for composing transformers, and `|` for
  alternating them, similarly to the dual with rows.

- To provide more predictable inference in dependent contexts, this system will
  draw on the notion of _matchability polymorphism_ as outlined in the
  higher-order type-level programming paper. The key recognition to make,
  however is that where that paper was required to deal with
  backwards-compatibility concerns, we are not, and hence can generalise all
  definitions to be matchability polymorphic where appropriate.

- Implicit parameters (e.g. `{f : Type -> Type} -> f a -> f a` in Idris) are
  able to be declared as part of the signature context.

- As of yet it is undecided as to what form of type-checking and inference will
  be used. The idea is to match our goals against existing theory to inform
  implementation.
  + Boxy: Provides greater inference power for higher-rank types and
    impredicative instantiation. Integrates well with wobbly inference for GADTs
    and has a relatively simple metatheory.
  + Complete Bidirectional: An interesting theory with particular power to infer
    higher-rank types, but with no support for impredicative instantiation. Has
    some additional complexity through use of subtyping relationships. This is
    fully decidable, and subsumes standard Damas-Milner style inference.
  + Flexible Types/HML: Provides a translation from MLF to System-F, which may 
    be useful during translation to GHC Core. Requires annotations only on
    polymorphic function parameters (e.g. `fn f = (f 1, f True)` would require
    annotation of `f :: forall a . a -> a`). However, in the context of this
    requirement, all other instantiations (including impredicative and
    higher-rank) can be inferred automatically. Could this be combined with
    decidable rank-2 inference to increase expressiveness?
  + FPH: Lifts restrictions on polymorphic instantiation through use of an
    internal theory based on MLF. Focuses on impredicativity, but provides some
    commentary on how to extend the theory with approaches to higher-rank
    inference. This is based upon the Boxy work, but has trade-offs with regards
    to typeability in absence of annotations.
  + MLF: A highly complex type theory with the ability to represent strictly
    more types than System-F (and its variants). There are various theories that
    allow for translation of these types to System-F. It has theory supporting
    qualified types (a separate paper), which are necessary for our type system.
    I worry that choice of MLF will expose greater complexity to users.
  + Practical: Provides a local inference-based foundation for propagation of
    higher-rank type annotations at the top level to reduce the annotation
    burden. We can likely use this to inform our design, but it is somewhat
    subsumed by the HML approach.
  + QML: A simple system supporting impredicative instantiation, but with a much
    higher annotation burden than others. The underlying specification of
    checking and inference is very clear, however, so there are still potential
    lessons to be learned.
  + Wobbly: Provides a unified foundation for treating all types as GADTs, and
    hence allowing for bounded recursive type definitions. It precisely
    describes where type-signatures are required in the presence of GADTs. Has
    some interesting insight with regards to polymorphic recursion.

  It should be noted that none of these theories explicitly address extension to
  dependent type theories, so doing so would be entirely on our plate.
  Furthermore any theory chosen must have support for qualified types (e.g.
  those with constraints, existentials).

  To this end, I don't know if it is possible to always transparently support
  eta expansion of functions without annotation.

  My _current_ recommendation is to base things on HML. This is for the
  following reasons:
  + It maximises inference power for both higher-rank and impredicative type
    instantiations.
  + It has a simple rule for where annotations are _required_.
  + While it requires them in more places than MLF, the notion of where to put
    an annotation is defined by the function's external type, not its body.
  + The inference mechanism is far simpler than MLF as it only makes use of
    flexible types rather than constrained types.
  + There is existing work for adding qualified types to HML.

- There is an integration of constraints with interfaces. An implementation of
  an interface may be _more specific_ than the interface definition itself,
  through use of GADTs.

- Interfaces in Luna are inherently multi-parameter, by virtue of specifying a
  structure as a row.

- In an ideal world, we would like to only require programmer-provided type
  annotations in the following circumstances:
  1. Polymorphic Recursion (technically a case of #2)
  2. Higher-Rank Function Parameters
  3. Constrained Data-Types (GADTs)

  In order to achieve this, the final design will employ techniques from both
  unification-based Damas-Milner inference techniques, and annotation
  propagation inspired by local type-inference techniques.

# Structural Type Shorthand
In Luna, we want to be able to write a type-signature that represents types in
terms of the operations that take place on the input values. A classical example
is `add`:

```
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

```
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

# Interfaces
An interface in Luna is a representation of the (partial) structure of a type
given name.

# Desugaring Types to Rows

# Testing the Type System

# Unresolved Questions
<!-- Ara -->

Should we start doing our own inference from scratch?

1. How to integrate row polymorphism with the inference story?

2. How to do dependent types?

3. User-facing Complexity – what concepts are we ok to introduce / trade off 
   with inference power

4. Compiler complexity – how complex a codebase are we ok maintaining?

5. Interfaces and integration into the language:
    - How are they represented? 
    - Do we want a separate keyword for their definition?

    ```
    interface Iterable a:
        elementType : Type
        fmap : (a.elementType -> a.elementType) -> a
    ```

6. Auto-injectivity for Generalised inductive types (GADTS)? Are our type
   constructors _matchable_ (injective and generative)?

7. `:` vs `<:` in the presence of inductive types and open rows. Think about 
   covariance and contravariance. 

`f : (True -> a) | (1 -> b)`

The key to it all is covariance and contravariance of polymorphic type 
variables.

# Steps

1. Answer the remaining questions:
    - Provide a comparison of programs in HML and MLF, answering the following
      questions:
      1. What can be inferred (or otherwise) with each system?
      2. What additional concepts in types (+ proposed syntax), do these systems
         entail?
      3. In what cases are the concepts described in 2. exposed to the users?
2. Write down a high-level design for the system.
3. Formalise the important parts thereof.
4. Implementation
5. Formalise the remaining portions of the system.

# Goals for the Type System
In our design for Luna, we firmly believe that the type system should be able to
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
  than turn Luna into a research language. We want users to be able to add
  safety gradually.

# References
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

<!--
Welcome to the Lunatic's Asylym, where we thought it would be a good idea to try
and bring dependent types to the masses.
-->
