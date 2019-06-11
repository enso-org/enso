___
- **Feature Name:** Type System
- **Start Date:** 2018-06-26
- **Change Type:** Breaking
- **RFC Dependencies:** Syntax Overhaul
- **RFC PR:**
- **Enso Issue:**
- **Implemented:**

# Summary
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

<!-- MarkdownTOC levels="1,2" autolink="true" -->

- [Motivation](#motivation)
- [Goals for the Type System](#goals-for-the-type-system)
- [Type Conversions](#type-conversions)
  - [Convertible](#convertible)
  - [Coercible](#coercible)
- [Principles for Enso's Type System](#principles-for-ensos-type-system)
- [Structural Type Shorthand](#structural-type-shorthand)
- [Interfaces](#interfaces)
  - [Interfaces as Names for Structures](#interfaces-as-names-for-structures)
  - [Interfaces as a Global Mapping](#interfaces-as-a-global-mapping)
  - [Interface Generality](#interface-generality)
- [Subtyping and User-Facing Type Definitions](#subtyping-and-user-facing-type-definitions)
- [Row Polymorphism and Inference](#row-polymorphism-and-inference)
- [Unresolved Questions](#unresolved-questions)
- [Dependency and Enso](#dependency-and-enso)
- [Steps](#steps)
- [References](#references)

<!-- /MarkdownTOC -->

# Motivation
At present, Enso has nothing but a rudimentary module system to aid in the
complexity management and scoping of code. For a sophisticated language this is
a bad state of affairs, and so this RFC aims to propose a redesign of certain
portions of Enso to present a _unified_ design which unifies modules (in both
the conventional and ML senses), classes, and interfaces under a single
first-class umbrella.

In doing so, the proposal supports a diversity of use-cases, allowing the
first-class manipulation of types, including the creation of anonymous types. In
doing so, it provides users with first-class modularity for their code, and
intuitive mechanisms for working with types in Enso's type system. This concept
thus brings a massive simplification to the Enso ecosystem, providing one
powerful mechanism to accomplish many key language features, without requiring
the user to understand more than the mechanisms of `type`, and the principle
behind Enso's type system.

In the end, Enso's current module system is insufficient for serious development
in the language, and needs to be replaced. In doing so, this RFC proposes to
take the time to bring a vast simplification to the language in the same swoop.

# Goals for the Type System
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

# Type Conversions
Like in any programming language, Enso requires the ability to convert between
types. Sometimes these conversions have to happen at runtime, incurring a
computational cost, but sometimes these conversions can be 'free', in cases
where the compiler can prove that the types have the same representation at
runtime. Enter the `Coercible` and `Convertible` mechanisms.

## Convertible
There is a tension in Enso's design around conversions between types. In many
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

- Due to the nature of Enso's type system and how default arguments count
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

# Principles for Enso's Type System

- Types in Enso are functions on sets (constructors included), and are based on
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

- Naming in Enso is case-insensitive, so to hoist a bare function to the type
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

- Dependent types in Enso will desugar to an application of Quantitative Type
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
  contexts. Enso's Monadic Contexts are based on this theory.

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

- Enso will make use of whatever advanced type-system features are at its
  disposal to improve safety and performance.

- Complex errors will be explained simply, even if this requires additional
  annotation from programmers who use the advanced features.

- In the context of dependent types, relevance inference will be performed.

- The Enso typechecker will integrate an aggressive proof rewrite engine to
  automate as much of the proof burden as possible.

- Inference is employed heavily, letting types span from invisible to fancy,
  with each level providing as many guarantees as is practicable.

- Enso _is not a proof system_, and its implementation of dependent types will
  reflect said fact, being biased towards more practical usage.

- The future implementation of dependent types into Enso will be based on RAE's
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
  Enso will err on the side of inference capability in order to promote ease of
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

- Enso must support nested type definitions. These nested types are
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

- Rows in Enso are open, and have polymorphic projections. A projection without
  a given type is assumed to be a label, but `{ (Foo : Type) : Int }` lets users
  use other types as projections. A row `{ myLabel : Int }` is syntax sugar for
  an explicit projection based on a label: `{ (myLabel : Label) : Int }`.

- Enso features built-in first-class lenses and prisms based on row projections,
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

- Type equality in Enso is represented by both representational and structural
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
  Enso's types able to be represented flat in memory. This means that the list
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
  + Row Concatenation: `&` or `,`

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

- Interfaces in Enso are inherently multi-parameter, by virtue of specifying a
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
In Enso, we want to be able to write a type-signature that represents types in
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
An interface in Enso is a representation of the (partial) structure of a type
given name. For a type in Enso to conform to an interface, under the hood it is
one that structurally conforms to the projections and result types provided by
the interface.

There are two possible treatments of interfaces as far as Enso's type system is
concerned:

1. **Names Only:** An interface is purely a name given to a type that describes
   a certain structure.
2. **Global Mapping:** More akin to how they behave in Haskell, an interface
   acts as a global mapping between a name and a structure (row) that contains
   the associated behaviour.

Both have their upsides and trade-offs, and each of which are explored below.
Each uses a separate keyword `interface` to define the interface, as this allows
for us to avoid generation of automatic constructors. In the below sections we
look at the following example.

```
interface Iterable : (a : Type) -> Type =
    ElemType : Type
    map : (a.ElemType -> a.ElemType) -> a -> a

    # A default implemented method
    <$> : (a.ElemType -> a.ElemType -> a -> a)
    <$> = a.map
```

We also use the `instance` keyword to denote a standalone implementation of an
interface.

There are a few elements of the design for interfaces that will hold regardless
of the choice made below.

- Interfaces are inherently type constructors that generate a row. This means
  that they can use the standard dependency machinery included in Enso to
  compute some or all of their types.
- Interfaces, as type constructors, are inherently multi-parameter should they
  need to be.

## Interfaces as Names for Structures
This first option is the most 'pure' with regards to the structural nature of
the type system. It works as follows:

- The `interface` keyword differs from the `type` keyword only in that it will
  never generate an automatic type constructor for the type.
- An interface definition creates a row constructor and associated row that can
  represent the operations required of a type that conforms to the interface.
  For example, the above definition would desugar as follows:

  ```
  Iterable : (a : Type) -> Type =
      { ElemType : Type
      , map : (a.ElemType -> a.ElemType) -> a -> a
      , <$> : (a.ElemType -> a.ElemType) -> a -> a = a.map
      }

  # The following is equivalent, but longer.
  Iterable : (a : Type) -> Type
  Iterable = a -> { ElemType : Type
                  , map : (a.ElemType -> a.ElemType) -> a -> a
                  , <$> : (a.ElemType -> a.ElemType) -> a -> a = a.map
                  }
  ```

- Any type that conforms to this structure is an implementation of this
  interface, regardless of an explicit definition.

The primary benefits of choosing such a design are as follows:

- Any type that conforms to the row declared by the interface is counted as an
  implementation of the interface, regardless of any keyword.
- We do not have to reserve names in scope, meaning that `mplus` could be
  represented by a `+`, as could numeric addition.
- It is very pure from a type-system perspective, with interfaces just being a
  way to declare a row without a constructor.
- It is trivial to provide default implementations, as you would for any type
  definition.

However, it is not all rosy. This design has some downsides with regards to
usability, particularly around the provision of useful diagnostics to users in
the form of inferred type signatures and type errors.

- Interface names are only useful as shorthand for programmers, as we can never
  infer a name based on the structure of a type. They can be used for explicit
  `implements` declarations to ensure that the methods of the interface are
  implemented, and they can be used in explicit signatures.
- As we can never infer interface usage, all inferred signatures will need to
  represent types in terms of their structure. This means that we can never say
  that `a` needs to be a `Number` for you to use `+`, and instead we can only
  say `a` needs to have a method `+ : a -> a -> a` in our type errors. While
  this is not inherently a problem, it does limit our ability to give users
  _named_ concepts to reason about their code with.

## Interfaces as a Global Mapping
The alternative design is to use the `interface` keyword to provide the compiler
(and hence the users) with a global mapping from method names to the names of
the interfaces. This would work as follows:

- The `interface` keyword generates a global mapping from the interface name to
  the names of the interface methods and properties. For the above interface, it
  would be `Iterable <-> (ElemType, map, <$>)`.
- This definition creates, in addition to the global mapping, a type constructor
  that produces a row, as for above. The desugaring is identical to the above.
- Types may still implicitly conform to interfaces.

The primary benefits of this design are as follows:

- Given that there is a global mapping of interface names to method and
  property names, whenever we see the usage of such a method we can infer that
  the type(s) in question implement the interface.
- We can use the interface name to method mapping to infer signatures that
  explicitly name the interface. This gives programmers the ability to reason
  about concepts (or structures) with names.

That is not to say that this approach, too, is without its downsides. In fact,
where the first approach has benefits, this approach tends to miss out on them:

- A name used in an interface is globally reserved, meaning that it can't be
  used for anything else. This means that numeric addition could be `+`, but
  monoidal concatenation would have to be named differently (e.g. `<>`).
- It is far more complex from the perspective of the type system. While, in this
  case, interfaces do generate rows, they also generate significant amounts of
  extra baggage to support this global mapping.

A variant on this approach would include the types of the methods and properties
in the mapping. This allows names to no longer need to be globally reserved, but
often means that inference still won't be able to match a name (e.g. if we
define `foo = a -> b -> a + b`, then we still can't guarantee that it belongs to
the `Num` interface, for example).

As a result, if we want to include types in the mapping, the recommendation is
for the first option, not this variant of the second.

## Interface Generality
One of the larger pain-points in Haskell comes from the fact that typeclass
structure places restrictions on what types can become an instance of the class.
Enso, instead, works from a foundation of rows. This means that we can trivially
make use of associated types in interfaces to compute more general signatures.

Consider the following `Iterable` interface, which expresses a map operation in
a way that is not easy in Haskell, and that is more general than `Functor`.

```
# The `=` is used here for consistency with unified definitions of the form
# (name : type = val).

interface Iterable : (a : Type) -> Type =
    elemType : Type
    map : (elemType -> elemType) -> a -> a

# Needs to account for internal type transformations (map toString) for example

instance Iterable Text =
    elemType = Char
    map = ...
```

Checking such a type is still able to be done through standard qualified
typechecking algorithms. There are, however, a few things to keep in mind:

- For a type to conform to an interface, it is sufficient for its implementation
  of the interface methods to be _callable_ with the signature given in the
  interface. This means that an implementation can add additional function
  parameters as long as they are defaulted.
- Of course, in the case where these function parameters are _used_, the type is
  no longer conforming with the interface. This is a bit nasty, but unavoidable.

Even nicer is the fact that such an approach can be combined with partial data
(restricted instantiation) to allow definition of `Iterable` across sets.
Consider the following:

```
instance Iterable (Set a) =
    elemType = a
    map = ...
```

# Subtyping and User-Facing Type Definitions
As complex as the internal type language of Enso may need to be, we ideally want
to only present a limited set of concepts to the user for use in writing their
own types. Internally, we have the following relations between types:

- `:` - This gives a type to an expression (this expression can be a name, a
  program fragment, or any valid expression). A program expression `a : b` means
  that the expression `a` has the type given by `b`.
- `~` - This relation asserts structural equality between types. If `a ~ b`,
  then the two types have exactly the same structure. This can be represented by
  the containment relation: `a ~ b = a <: b && b <: a`.
- `<:` - This relation asserts that one type is wholly contained in another, and
  can be thought of as a form of structural subtyping. If `a <: b`, then the
  type `a` is structurally equal to or a subtype of `b`. In essence, this means

Please note that this may initially be a little confusing. What, then, does it
mean to have an expression like `(name : type = value)`. Simply enough, this
universal definition format says that 'the identifier given by `name` has type
given by `type`, and has value given by `value`.' This works globally, including
for functions, and for type definitions.

```
type MyExample : (a : Type) -> (b : Type) -> Type =
    # The body of the type goes here, as it is the VALUE of the type constructor
    # In a truly dependently typed language, a type constructor is just a
    # function on types, and the `type` keyword isn't even needed other than to
    # indicate automatic constructor generation.
```

Externally, however, it is a different story. We want users to not have to think
at such depth about their types to express what they want. This means that we do
not intend to require users to write expressions involving structural equality
or containment (though we may still expose these relations to allow simpler
programming with types).

- In reality, we want users to really only make use of `:` when defining their
  types.
- This means that we need to be very clear about what it means, especially in
  the face of recursive types.

Let's examine the major cases:

1. **Polymorphic Function Arguments:**
2. **Partial Data Types:**
3. **Qualified Types in Functions:**
4. **Variable Definitions:**

Is variance always valid in the ways we need?

# Row Polymorphism and Inference
The foundation of Enso's usability is based on a _structural_ type system. This
means that there is no concept of nominal equality. In Enso types are equal if
they have the same structure. Under such a system, the only reason that users
should _need_ to write types is to provide a type that the inference engine
cannot infer (though this may be more general, more specific, or for a case that
cannot be inferred).

From a philosophical standpoint, we want Enso's type system to infer sensible
types for 99% of expressions that users write, and allow the users to _refine_
these inferred types. This refinement process can involve:

- Giving a more general (more permissive) type to an expression.
- Constrain an expression by giving a less permissive type to it.
- Add additional safety and checking by introducing more complex type-system
  usage (e.g. dependency, partial data, etc).

It should be noted that, in this sense, Enso's structural type system has no
concept of a 'principle' (or most-general) type for an expression, at least not
in the sense of traditional System-F.

# Unresolved Questions

1. How to integrate row polymorphism with the inference story?

2. How to do dependent types?

5. Representing ADTs as rows (variants and records).

6. Auto-injectivity for Generalised inductive types (GADTS)? Are our type
   constructors _matchable_ (injective and generative)?

Points that need to be accounted for in the 'wishlist' design:

- Basic types
- Constrained types
- Inference
- Dependent Types
- Containment/subtyping
- Dynamic
- Exception handling
- Monadic Contexts
- Complexity tradeoff between execution and implementation

```
read : String -> Ty? -> Ty
```

Named state items? Parametrise the state over a type and get via names, which
are projections.

- State parametrised over a record giving both name and type
- Do we want to look things up based on type?
- `State.get name` where `name` is a row projection.

- Monadic contexts desugared as transformers.


- Initially treat the value _in_ the state as a dynamic.

- Dependent types at runtime compared with dynamics.

- Encoding of strictness in the type system.

IO, State, Exception (not ! error)

- Dependent types as constructing proofs through combining types. Combining
  types provides evidence which can be discharged to create a proof. A value can
  then only be constructed by discharging a proof.

# Dependency and Enso
The ability to evaluate arbitrary functions on the type level inherently makes
Enso a dependently typed language, as arbitrary values can appear in types.

- The initial implementation will provide this facilities, but no system for
  automating the proof steps (c.f. f-star), or interactive theorem proving.
- While this allows people to express safety guarantees in the type system, it
  is a natural consequence of Enso's design.

# Steps

1. Wojciech produces examples of program fragments and the types that should be
   inferred based on the expressions.
2. Based on these fragments, try and synthesis a theoretical approach to both
   inference and type-checking based upon this.
3. Write down a high-level design based on this theory, stating the properties
   of the system that we want.
4. Formalise the necessary parts thereof.
5. Implement based upon this design + formalisation.
6. Formalise the remaining parts of the system.

By the end of working day on the 23rd of May, WD and AA need to have a
comprehensive mutual understanding of what the type system is going to be.

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
Welcome to the Ensotic's Asylym, where we thought it would be a good idea to try
and bring dependent types to the masses.
-->
