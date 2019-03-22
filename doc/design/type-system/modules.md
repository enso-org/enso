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
As this RFC is accompanied by a separate RFC proposing sweeping syntactic 
changes to the Luna language, it endeavours to use the new syntax throughout 
and should hence be read with that fact in mind.

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
   standalone implementation block. These take the form of `instance Interface for Type`, with any type parameters filled appropriately. 

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
type name. If this change wasn't made, you would be forced to pattern match on, and construct with, `Type.Type` as the constructor, which is _very_ inelegant. 

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
`PrettyPrint` and `printer` are made visible from within the scope of `Test`. This means that a user can write `import Test: printer` and have it work. 

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
system.

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
Anonymous types can also be constructed as values using similar syntax. You can provide values directly, which will work in a context where names are not 
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

# Unresolved Questions
This section should address any unresolved questions you have with the RFC at 
the current time. Some examples include:

- We definitely need further discussion on the situation with constructors and
  pattern matching.
- We definitely need further discussion on nested types.
- Some syntax can likely be cleaned up.
