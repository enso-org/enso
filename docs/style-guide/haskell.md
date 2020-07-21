---
layout: style-guide
title: Haskell Style Guide
category: style-guide
tags: [style-guide]
order: 4
---

# Haskell Style Guide

Like many style guides, this Haskell style guide exists for two primary reasons.
The first is to provide guidelines that result in a consistent code style across
all of the Enso codebases, while the second is to guide people towards a style
that is expressive while still easy to read and understand.

In general, it aims to create a set of 'zero-thought' rules in order to ease the
programmer burden; there is usually only _one way_ to lay out code correctly.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Code Formatting](#code-formatting)
  - [Whitespace](#whitespace)
  - [Line Wrapping](#line-wrapping)
  - [Alignment](#alignment)
  - [Naming](#naming)
  - [Imports](#imports)
  - [Exports](#exports)
  - [Section Headers](#section-headers)
  - [Auto-Formatting](#auto-formatting)
- [Commenting](#commenting)
  - [Documentation Comments](#documentation-comments)
  - [Source Notes](#source-notes)
  - [TODO Comments](#todo-comments)
  - [Other Comment Usage](#other-comment-usage)
- [Program Design](#program-design)
  - [Libraries](#libraries)
  - [Modules](#modules)
  - [Data Declarations](#data-declarations)
  - [Testing and Benchmarking](#testing-and-benchmarking)
  - [Warnings, and Lints](#warnings-and-lints)
- [Language Extensions](#language-extensions)
  - [Default Extensions](#default-extensions)
  - [Allowed Extensions](#allowed-extensions)
  - [Allowed With Care](#allowed-with-care)
  - [Disallowed Extensions](#disallowed-extensions)

<!-- /MarkdownTOC -->

## Code Formatting

This section explains the rules for visually laying out your code. They provide
a robust set of guidelines for creating a consistent visual to the code.

### Whitespace

The rules for whitespace in the Enso codebases are relatively simple:

- 4 spaces are used for indentation, with no tabs.
- There should not be any trailing whitespace.
- There should be no spurious whitespace within the lines, unless it is used for
  [alignment](#alignment) as discussed below.

### Line Wrapping

In order to provide visual consistency across our codebases, and also to
contribute to making our code easier to scan, we enforce that all code should be
wrapped to 80 characters width at a maximum.

The nature of Haskell, however, means that it can sometimes be unclear where to
break lines. We use the following guidelines:

- Wrap all lines to a maximum length of 80 characters.
- Break the lines on operators where possible, rather than wrapping function
  arguments.

  ```hs
  -- This
  foo <- veryLongFunction1 veryLongArgument1
      $ veryLongFunction2 veryLongArgument2 veryLongArgument3

  -- Not this
  foo <- veryLongFunction1 veryLongArgument1 $ veryLongFunction2
      veryLongArgument2 veryLongArgument3
  ```

- When you have a choice of operators on which you could break, choose the one
  with the highest precedence. We find that this makes code significantly more
  readable.

  ```hs
  -- This
  potentialPkgRoot <- liftIO $ Directory.canonicalizePath
      =<< (canPath </>) <$> pkgRootFromExe @a

  -- Not this
  potentialPkgRoot <- liftIO $ Directory.canonicalizePath =<< (canPath </>)
      <$> pkgRootFromExe @a
  ```

- Wrap operators to the _start_ of the line, rather than leaving them trailing
  on a line.

  ```hs
  -- This
  foo <- veryLongFunction1 veryLongArgument1
      $ veryLongFunction2 veryLongArgument2 veryLongArgument3

  -- Not this
  foo <- veryLongFunction1 veryLongArgument1 $
      veryLongFunction2 veryLongArgument2 veryLongArgument3
  ```

- Function signatures should wrap on the `=>` and `->`, and in the context of a
  doc comment should have each argument on a separate line.
- Lists (and all list-like constructs e.g. constraint tuples, import lists)
  should be wrapped with a _leading_ comma, aligned with the opening bracket,
  and a space between the opening bracket and the first item. This is also used
  in record declarations.

  ```hs
  -- This
  myFunctionWithAVeryLongName :: forall a m . ( SomeConstraintOnA a
                                              , SomeMonadConstraint m )
      => a -> SomeOtherType -> m a

  -- Not this
  myFunctionWithAVeryLongName :: forall a m . (SomeConstraintOnA a,
                                              SomeMonadConstraint m)
      => a -> SomeOtherType -> m a
  ```

- If all else fails, wrap the lines using your best effort (usually what you
  find to be most readable). This may result in discussion during code review,
  but will provide a learning experience to augment this guide with more
  examples.

Please _do not_ shorten sensible names in order to make things fit into a single
line. We would much prefer that the code wraps to two lines and that naming
remains intelligible than names become so shortened as to be useless.

### Alignment

When there are multiple lines that are visually similar, we try to align the
similar portions of the lines vertically.

```hs
people   <- getAllPeople <$> worlds
names    <- getName      <$> people
surnames <- getSurnames  <$> names
```

This should _only_ be done when the lines don't need to be wrapped. If you have
lines long enough that this visual justification would cause them to wrap, you
should prefer to _not_ wrap the lines and forego the visual alignment.

Furthermore, if you have to wrap a visually similar line such that it now spans
multiple lines, it _no longer counts_ as visually similar, and hence subsequent
lines should not be aligned with it.

### Naming

Enso has some fairly simple general naming conventions, though the sections
below may provide more rules for use in specific cases.

- Types are written using `UpperCamelCase`.
- Variables and function names are written using `camelCase`.
- If a name contains an initialism or acronym, all parts of that initialism
  should be of the same case: `httpRequest` or `makeHTTPRequest`.
- Short variable names such as `a` and `b` should only be used in contexts where
  there is no other appropriate name (e.g. `flip (a, b) = (b, a)`). They should
  _never_ be used to refer to temporary data in a `where` or `let` expression.
- Any function that performs an unsafe operation that is not documented in its
  type (e.g. `head : [a] -> a`, which fails if the list is empty), must be named
  using the word 'unsafe' (e.g. `unsafeHead`). For more information on unsafe
  function usage, see the section on [safety](#safety). The one exception to
  this rule is for functions which fail intentionally on a broken implementation
  (e.g. "should not happen"-style fatal crashes).
- Naming should use American English spelling.

### Imports

Organising imports properly means that it's easy to find the provenance of a
given function even in the absence of IDE-style tooling. We organise our imports
in four sections, each of which may be omitted if empty.

1. **Re-Exports:** These are the modules that are to be re-exported from the
   current module. We import these qualified under a name `X` (for export), and
   then re-export these in the module header (see below for an example).
2. **Preludes:** As we recommend the use of `-XNoImplicitPrelude`, we then
   explicitly import the prelude in use. This is almost always going to be
   `Prologue` as described in the section on [libraries](#libraries) below.
3. **Qualified Imports:** A list of all modules imported qualified. The `as`
   portion of the import expressions should be vertically aligned.
4. **Unqualified Imports:** These must _always_ have an explicit import list.
   There are _no_ circumstances under which we allow a truly unqualified import.
   The import lists should be vertically aligned.

Imports within each section should be listed in alphabetical order, and should
be vertically aligned.

When we have a module that exports a type the same as its name, we import the
module qualified as its name, but we _also_ import the primary type from the
module unqualified. This can be seen with `Map` in the examples below.

This example is for a module that re-exports some names:

```hs
module Enso.MyModule (module Enso.MyModule, module X) where

import Enso.MyModule.Class as X (foo, bar)

import Prologue

import qualified Control.Monad.State as State
import qualified Data.Map            as Map

import Data.Map  (Map)
import Rectangle (Rectangle)
import Vector    (Vector (Vector), test)
```

However, in the context where your module doesn't re-export anything, you can
use the simplified form:

```hs
module Enso.MyModule where

import Prologue

import qualified Control.Monad.State as State
import qualified Data.Map            as Map

import Data.Map  (Map)
import Rectangle (Rectangle)
import Vector    (Vector (Vector), test)
```

### Exports

There is nothing more frustrating than having a need to use a function in a
module that hasn't been exported. To that end, we do not allow for restricted
export lists in our modules.

Instead, if you want to indicate that something is for internal use, you need to
define it in an internal module. For a module named `Enso.MyModule`, we can
define internal functions and data-types in `Enso.MyModule.Internal`. This means
that these functions can be imported by clients of the API if they need to, but
that we provide no guarantees about API stability when using those functions.

### Section Headers

In order to visually break up the code for easier 'visual grepping', we organise
it using section headers. These allow us to easily find the section that we are
looking for, even in a large file.

For each type defined in a file, it can be broken into sections as follows:

```hs
--------------------
-- === MyType === --
--------------------

-- === Definition === --
{- The definition of the type goes here -}


-- === API === --
{- The API of the type goes here -}


-- === Instances === --
{- Any instances for the type go here -}

```

The section header must be preceded by three blank lines, while the subsection
headers (except the first) should be preceded by two blank lines. Any of these
subsections may be omitted if they don't exist, and a file may contain multiple
of these sections as relevant.

### Auto-Formatting

While we have attempted to use haskell auto-formatters to enforce many of the
above stylistic choices in this document, none have been found to be flexible
enough for our needs. However, as tools evolve or new ones emerge, we are open
to revisiting this decision; if you know of a tool that would let us automate
the above stylistic rules, then please speak up.

## Commenting

Comments in code are a tricky area to get right as we have found that comments
often expire quickly, and in absence of a way to validate them, remain incorrect
for long periods of time. In order to best deal with this problem, we make the
keeping of comments up-to-date into an integral part of our programming practice
while also limiting the types and kinds of comments we allow.

Comments across the Enso codebases fall into three main types:

- **Documentation Comments:** API documentation for all language constructs that
  can have it (functions, typeclasses, and so on).
- **Source Notes:** Detailed explorations of design reasoning that avoid
  cluttering the code itself.
- **Tasks:** Things that need doing or fixing in the codebase.

When we write comments, we try to follow one general guideline. A comment should
explain _what_ and _why_, without mentioning _how_. The _how_ should be
self-explanatory from reading the code, and if you find that it is not, that is
a sign that the code in question needs refactoring.

Code should be written in such a way that it guides you over what it does, and
comments should not be used as a crutch for badly-designed code.

### Documentation Comments

One of the primary forms of comment that we allow across the Enso codebases is
the doc comment. Every language construct that can have an associated doc
comment should do so. These are intended to be consumed by users of the API, and
use the standard Haddock syntax. Doc comments should:

- Provide a short one-line explanation of the object being documented.
- Provide a longer description of the object, including examples where relevant.
- Explain the arguments to a function where relevant.

They should not reference internal implementation details, or be used to explain
choices made in the function's implementation. See [Source Notes](#source-notes)
below for how to indicate that kind of information.

### Source Notes

Source Notes is a mechanism for moving detailed design information about a piece
of code out of the code itself. In doing so, it retains the key information
about the design while not impeding the flow of the code.

Source notes are detailed comments that, like all comments, explain both the
_what_ and the _why_ of the code being described. In very rare cases, it may
include some _how_, but only to refer to why a particular method was chosen to
achieve the goals in question.

A source note comment is broken into two parts:

1. **Referrer:** This is a small comment left at the point where the explanation
   is relevant. It takes the following form: `-- Note [Note Name]`, where
   `Note Name` is a unique identifier across the codebase. These names should be
   descriptive, and make sure you search for it before using it, in case it is
   already in use.
2. **Source Note:** This is the comment itself, which is a large block comment
   placed after the first function in which it is referred to in the module. It
   uses the haskell block-comment syntax `{- ... -}`, and the first line names
   the note using the same referrer as above: `{- Note [Note Name]`. The name(s)
   in the note are underlined using a string of the `~` (tilde) character.

A source note may contain sections within it where necessary. These are titled
using the following syntax: `== Note [Note Name (Section Name)]`, and can be
referred to from a referrer much as the main source note can be.

Sometimes it is necessary to reference a source note in another module, but this
should never be done in-line. Instead, a piece of code should reference a source
note in the same module that references the other note while providing
additional context.

An example, taken from the GHC codebase, can be seen below.

```hs
prepareRhs :: SimplEnv -> OutExpr -> SimplM (SimplEnv, OutExpr)
-- Adds new floats to the env iff that allows us to return a good RHS
prepareRhs env (Cast rhs co)    -- Note [Float Coercions]
  | (ty1, _ty2) <- coercionKind co      -- Do *not* do this if rhs is unlifted
  , not (isUnLiftedType ty1)            -- seUnsae Note [Float Coercions (Unlifted)]
  = do  { (env', rhs') <- makeTrivial env rhs
        ; return (env', Cast rhs' co) }

        ...more equations for prepareRhs....

{- Note [Float Coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~
When we find the binding
        x = e `cast` co
we'd like to transform it to
        x' = e
        x = x `cast` co         -- A trivial binding
There's a chance that e will be a constructor application or function, or
something like that, so moving the coercion to the usage site may well cancel
the coercions and lead to further optimisation.
        ...more stuff about coercion floating...

== Note [Float Coercions (Unlifted)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ...explanations of floating for unlifted types...
-}
```

A source note like this is useful whenever you have design decisions to explain,
but can also be used for:

- **Formulae and Algorithms:** If your code makes use of a mathematical formula,
  or algorithm, it should note where the design element came from, preferably
  with a link.
- **Safety:** Sometimes it is necessary to use an unsafe API in a context where
  it is trivially made safe. You should always use a source note to explain why
  its usage is safe in this context.

### TODO Comments

We follow a simple convention for `TODO` comments in our codebases:

- The line starts with `TODO` or `FIXME`.
- It is then followed by the author's initials `[ARA]`, or for multiple people
  `[ARA, WD]`, in square brackets.
- It is then followed by an explanation of what needs to be done.

For example:

```hs
-- TODO [ARA] This is a bit of a kludge. Instead of X it should to Y, accounting
-- for the fact that Z.
```

### Other Comment Usage

There are, of course, a few other situations where commenting is very useful:

- **Commenting Out:** You may comment out code while developing it, but if you
  commit any commented out code, it should be accompanied by an explanation of
  why said code can't just be deleted.
- **Bugs:** You can use comments to indicate bugs in our code, as well as
  third-party bugs. In both cases, the comment should link to the issue tracker
  where the bug has been reported.

## Program Design

Any good style guide goes beyond purely stylistic rules, and also talks about
design styles to use in code.

### Libraries

The Enso project has many internal libraries that are useful, but we have found
that maintaining these on Hackage while they are under such active development
is counterproductive.

Instead, libraries live in the `lib/` folder of the primary project with which
they are associated (Enso, Enso Studio, or Dataframes). These libraries may be
freely used by others of our projects by depending on a git commit of the
project that they live in. All of these are safe to use.

#### Prologue

`Prologue` is our replacement for Haskell's `Prelude`. For the most part it is
compatible with the prelude, though it is designed with a safe API as the first
port of call.

As a rule of thumb, if the prelude exports a partial function, that function has
been made total in Prologue. This usually takes the form of returning `Maybe`,
rather than throwing an error (e.g. `head :: [a] -> Maybe a`). In the case where
a function has been redefined like this, the original version is available using
an unsafe name (e.g. `unsafeHead` in the case above).

Prologue also exports additional useful functionality from across the Haskell
ecosystem, such as utilities for working with Lenses and for writing type-level
computation.

It is highly recommended that you scan the code of Prologue.

#### Safety

It is incredibly important that we can trust the code that we use, and hence we
tend to disallow the definition of unsafe functions in our public API. When
defining an unsafe function, you must account for the following:

- It must be named `unsafeX`, as discussed in the section on [naming](#naming).
- Unsafe functions should only be used in the minimal scope in which it can be
  shown correct, not in larger pieces of code.
- Unsafe function definition must be accompanied by a source note explaining why
  it is not defined safely (e.g. performance).
- Unsafe function usage must be accompanied by a source note explaining why this
  usage of it is safe.

Furthermore, we do not allow for code containing pattern matches that can fail.

#### Control.Monad.Exception

We have our own exception framework based on `ExceptT` that encodes exception
usage at the type level. This ensures that all synchronous exceptions must be
dealt with.

It is defined in
[`lib/exception/`](https://github.com/enso-org/luna/tree/master/lib/exception)
and contains utilities for declaring that a function throws an exception, as
well as throwing and catching exceptions.

The primary part of this API is the `Throws` constraint, which can be passed
both a single exception type or a list of exceptions. It is a monadic exception
framework.

```hs
myFunction :: Throws '[MyErrorOne, MyErrorTwo] m => ArgType -> m ReturnType
```

We encourage our programmers to define their own exception types, and when doing
so they should use the following guidelines:

- We name them using 'Error' rather than 'Exception', so `MyError`, rather than
  `MyException`.
- We always provide an instance of `Exception` for our exception type.
- We avoid encoding error information as strings, instead passing a strongly
  typed representation of the problem around. This often means that we end up
  re-wrapping an error thrown inside our function.

### Modules

Unlike much of the Haskell ecosystem, we tend to design modules to be imported
_qualified_ rather than unqualified. This means that we have a few rules to keep
in mind:

- When designing a module that exports a type, the module should be named after
  that type. If it exports multiple types, there should be a primary type, or
  the other types should be factored out into their own modules.
- We import modules as their name. If you have a module `Enso.Space.MyType`, we
  import it qualified as `MyType`.
- Functions should be named with the assumption of being used qualified. This
  means that we rarely refer to the module name in the function name (e.g.
  `State.run` rather than `State.runState`).

### Data Declarations

When declaring data types in the Enso codebases, please make sure to keep the
following rules of thumb in mind:

- For single-constructor types:

  - Write the definition across multiple lines.
  - Always name your fields.
  - Always generate lenses.

  ```hs
  data Rectangle = MkRectangle
      { _width  :: Double
      , _height :: Double
      } deriving (Eq, Ord, Show)
  makeLenses ''Rectangle
  ```

- For multiple-constructor data-types:

  - Write the definition across multiple lines.
  - Never name your fields.
  - Generate prisms only when necessary.

  ```hs
  data Shape
      = ShapeCircle Circle
      | ShapeRect   Rectangle
      deriving (Eq, Ord, Show)
  ```

- Always prefer named fields over unnamed ones. You should only use unnamed
  fields if one or more of the following hold:
  - Your data type is one where you are are _sure_ that separate field access
    will never be needed.
  - You are defining a multiple-constructor data type.
- Sort deriving clauses in alphabetical order, and derive the following for your
  type if logically correct:
  - General Types: `Eq`, `Generic`, `NFData`, `Ord`, `Show`.
  - Parametric 1-Types: `Applicative`, `Alternative`, `Functor`.
  - Monads: `Monad`, `MonadFix`.
  - Monad Transformers: `MonadTrans`.

#### Lenses

The Enso codebases make significant use of Lenses, and so we have some rules for
their use:

- Always use the `makeLenses` wrapper exported from `Prologue`.
- Always generate lenses for single-constructor data types.
- Never generate lenses for multi-constructor data types (though you may
  sometimes want to generate prisms).
- Fields in data types should be named with a single underscore.
- If you have multiple types where the fields need the same name, the `Prologue`
  lens wrappers will disambiguate the names for you as follows as long as you
  use a double underscore in the data declaration (e.g. `__x`).

```hs
data Vector = Vector
    { __x :: Double
    , __y :: Double
    , __z :: Double
    } deriving (Show)
makeLenses ''Vector

data Point = Point
    { __x :: Double
    , __y :: Double
    } deriving (Show)
makeLenses ''Point
```

This will generate lenses with names like `vector_x`, `vector_y`, and `point_x`,
`point_y`.

### Testing and Benchmarking

New code should always be accompanied by tests. These can be unit, integration,
or some combination of the two, and they should always aim to test the new code
in a rigorous fashion.

- We tend to use `HSpec`, but also make use of QuickCheck for property-based
  testing.
- Tests should be declared in the project configuration so they can be trivially
  run, and should use the mechanisms HSpec provides for automatic test
  discovery.
- A test file should be named after the module it tests. If the module is named
  `Enso.MyModule`, then the test file should be named `Enso.MyModuleSpec`.

Any performance-critical code should also be accompanied by a set of benchmarks.
These are intended to allow us to catch performance regressions as the code
evolves, but also ensure that we have some idea of the code's performance in
general.

- We use `Criterion` for our benchmarks.
- We measure time, but also memory usage and CPU time where possible.
- Where relevant, benchmarks may set thresholds which, when surpassed, cause the
  benchmark to fail. These thresholds should be set for a release build, and not
  for a development build.

_Do not benchmark a development build_ as the data you get will often be
entirely useless.

### Warnings, and Lints

In general, we aim for a codebase that is free of warnings and lints, and we do
this using the following ideas:

#### Warnings

New code should introduce no new warnings onto main. You may build with warnings
on your own branch, but the code that is submitted as part of a PR should not
introduce new warnings. You should also endeavour to fix any warnings that you
come across during development.

Sometimes it is impossible to fix a warning (e.g. TemplateHaskell generated code
often warns about unused pattern matches). In such cases, you are allowed to
suppress the warning at the module level using an `OPTIONS_GHC` pragma, but this
must be accompanied by a source note explaining _why_ the warning cannot be
fixed otherwise.

#### Lints

We also recommend using HLint on your code as a stylistic guide, as we find that
its suggestions in general lead to more readable code. If you don't know how to
set up automatic linting for your editor, somebody will be able to help.

An example of an anti-pattern that HLint will catch is the repeated-`$`. Instead
of `foo $ bar $ baz $ bam quux`, you should write `foo . bar. baz $ bam quux` to
use function composition.

## Language Extensions

Much like any sophisticated Haskell codebase, Enso makes heavy use of the GHC
language extensions. We have a broad swath of extensions that are enabled by
default across our projects, and a further set which are allowed whenever
necessary. We also have a set of extensions that are allowed with care, which
must be used sparingly.

When enabling a non-default extension, we never do it at the project or package
level. Instead, they are enabled on a file-by-file basis using a `LANGUAGE`
pragma. You may also negate default extensions, if necessary, using this same
technique.

It should be noted that not all of the extensions listed below are available
across all compiler versions. If you are unsure whether an extension is
available to you, we recommend checking the GHC Users Guide entry for that
extension (linked from the extension's table below).

### Default Extensions

The following language extensions are considered to be so safe, or to have such
high utility, that they are considered to be Enso's set of default extensions.
You can find said set of extensions for Enso itself defined in a
[common configuration file](https://github.com/enso-org/luna/blob/master/config/hpack-common.yaml).

#### AllowAmbiguousTypes

|          |                                                                                                                                          |
| :------- | :--------------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`AllowAmbiguousTypes`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-AllowAmbiguousTypes) |
| **Flag** | `-XAllowAmbiguousTypes`                                                                                                                  |

This extension is particularly useful in the context of `-XTypeApplications`
where the use of type applications can disambiguate the call to an ambiguous
function.

We often use the design pattern where a function has a free type variable not
used by any of its arguments, which is then applied via type applications. This
would not be possible without `-XAllowAmbiguousTypes`.

#### ApplicativeDo

|          |                                                                                                                              |
| :------- | :--------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`ApplicativeDo`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ApplicativeDo) |
| **Flag** | `-XApplicativeDo`                                                                                                            |

This extension allows desugaring of do-notation based on applicative operations
(`<$>`, `<*>`, and `join`) as far as is possible. This will preserve the
original semantics as long as the type has an appropriate applicative instance.

Applicative operations are often easier to optimise than monadic ones, so if you
can write a computation using applicatives please do. This is the same reason
that we prefer `pure` to `return`.

#### BangPatterns

|          |                                                                                                                            |
| :------- | :------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`BangPatterns`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-BangPatterns) |
| **Flag** | `-XBangPatterns`                                                                                                           |

This extension allows for strict pattern matching, where the type being matched
against is evaluated to WHNF before the match takes place. This is very useful
in performance critical code where you want more control over strictness and
laziness.

#### BinaryLiterals

|          |                                                                                                                                |
| :------- | :----------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`BinaryLiterals`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-BinaryLiterals) |
| **Flag** | `-XBinaryLiterals`                                                                                                             |

This extensions allow for binary literals to be written using the `0b` prefix.
This can be very useful when writing bit-masks, and other low-level code.

#### ConstraintKinds

|          |                                                                                                                                  |
| :------- | :------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`ConstraintKinds`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ConstraintKinds) |
| **Flag** | `-XConstraintKinds`                                                                                                              |

This allows any types which have kind `Constraint` to be used in contexts (in
functions, type-classes, etc). This works for class constraints, implicit
parameters, and type quality constraints. It also enables type constraint
synonyms.

All of these are very useful.

#### DataKinds

|          |                                                                                                                      |
| :------- | :------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`DataKinds`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DataKinds) |
| **Flag** | `-XDataKinds`                                                                                                        |

This extension enables the promotion of data types to be kinds. All data types
are promoted to kinds and the value constructors are promoted to type
constructors.

This is incredibly useful, and used heavily in the type-level programming that
makes the Enso codebase so expressive and yet so safe.

#### DefaultSignatures

|          |                                                                                                                                      |
| :------- | :----------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`DefaultSignatures`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DefaultSignatures) |
| **Flag** | `-XDefaultSignatures`                                                                                                                |

When you declare a default in a typeclass, it conventionally has to have exactly
the same type signature as the typeclass method. This extension lifts this
restriction to allow you to specify a more-specific signature for the default
implementation of a typeclass method.

#### DeriveDataTypeable

|          |                                                                                                                                        |
| :------- | :------------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`DeriveDataTypeable`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DeriveDataTypeable) |
| **Flag** | `-XDeriveDataTypeable`                                                                                                                 |

This extension enables deriving of the special kind-polymorphic `Typeable`
typeclass. Instances of this class cannot be written by hand, and they associate
type representations with types. This is often useful for low-level programming.

#### DeriveFoldable

|          |                                                                                                                                |
| :------- | :----------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`DeriveFoldable`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DeriveFoldable) |
| **Flag** | `-XDeriveFoldable`                                                                                                             |

This enables deriving of the `Foldable` typeclass, which represents structures
that can be folded over. This allows automated deriving for any data type with
kind `Type -> Type`.

#### DeriveFunctor

|          |                                                                                                                              |
| :------- | :--------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`DeriveFunctor`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DeriveFunctor) |
| **Flag** | `-XDeriveFunctor`                                                                                                            |

This enables automated deriving of the `Functor` typeclass for any data type
with kind `Type -> Type`.

#### DeriveGeneric

|          |                                                                                                                              |
| :------- | :--------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`DeriveGeneric`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DeriveGeneric) |
| **Flag** | `-XDeriveGeneric`                                                                                                            |

Enables automated deriving of the `Generic` typeclass. Generic is a typeclass
that represents the structure of data types in a generic fashion, allowing for
generic programming.

#### DeriveTraversable

|          |                                                                                                                                      |
| :------- | :----------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`DeriveTraversable`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DeriveTraversable) |
| **Flag** | `-XDeriveTraversable`                                                                                                                |

Enables automated deriving of the `Traversable` typeclass that represents types
that can be traversed. It is a valid derivation for any data type with kind
`Type -> Type`.

#### DerivingStrategies

|          |                                                                                                                                        |
| :------- | :------------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`DerivingStrategies`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DerivingStrategies) |
| **Flag** | `-XDerivingStrategies`                                                                                                                 |

Under certain circumstances it can be ambiguous as to which method to use to
derive an instance of a class for a data type. This extension allows users to
manually supply the strategy by which an instance is derived.

If it is not specified, it uses the defaulting rules as described at the above
link.

#### DerivingVia

|          |                                                                                                                          |
| :------- | :----------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`DerivingVia`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DerivingVia) |
| **Flag** | `-XDerivingVia`                                                                                                          |

This allows deriving a class instance for a type by specifying another type of
equal runtime representation (such that there exists a Coercible instance
between the two). It is indicated by use of the `via` deriving strategy, and
requires the specification of another type (the via-type) to coerce through.

#### DuplicateRecordFields

|          |                                                                                                                                              |
| :------- | :------------------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`DuplicateRecordFields`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DuplicateRecordFields) |
| **Flag** | `-XDuplicateRecordFields`                                                                                                                    |

This extension allows definitions of records with identically named fields. This
is very useful in the context of Prologue's `makeLenses` wrapper as discussed
above in the section on [lenses](#lenses).

#### EmptyDataDecls

|          |                                                                                                                                |
| :------- | :----------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`EmptyDataDecls`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-EmptyDataDecls) |
| **Flag** | `-XEmptyDataDecls`                                                                                                             |

Allows the definition of data types with no value constructors. This is very
useful in conjunction with `-XDataKinds` to allow the creation of more safety
properties in types through the use of rich kinds.

#### FlexibleContexts

|          |                                                                                                                                    |
| :------- | :--------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`FlexibleContexts`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-FlexibleContexts) |
| **Flag** | `-XFlexibleContexts`                                                                                                               |

This enables the use of complex constraints in class declarations. This means
that anything with kind `Constraint` is usable in a class declaration's context.

#### FlexibleInstances

|          |                                                                                                                                      |
| :------- | :----------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`FlexibleInstances`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-FlexibleInstances) |
| **Flag** | `-XFlexibleInstances`                                                                                                                |

This allows the definition of typeclasses with arbitrarily-nested types in the
instance head. This, like many of these extensions, is enabled by default to
support rich type-level programming.

#### Functional Dependencies

|          |                                                                                                                                                |
| :------- | :--------------------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`FunctionalDependencies`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-FunctionalDependencies) |
| **Flag** | `-XFunctionalDependencies`                                                                                                                     |

Despite this extension being on the 'defaults' list, this is only for the very
rare 1% of cases where Functional Dependencies allow you to express a construct
that Type Families do not.

You should never need to use a Functional Dependency, and if you think you do it
is likely that your code can be expressed in a far more clean manner by using
Type Families.

#### GeneralizedNewtypeDeriving

|          |                                                                                                                                                        |
| :------- | :----------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`GeneralizedNewtypeDeriving`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-GeneralizedNewtypeDeriving) |
| **Flag** | `-XGeneralizedNewtypeDeriving`                                                                                                                         |

This enables the generalised deriving mechanism for `newtype` definitions. This
means that a newtype can inherit some instances from its representation. This
has been somewhat superseded by `-XDerivingVia`

#### InstanceSigs

|          |                                                                                                                            |
| :------- | :------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`InstanceSigs`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-InstanceSigs) |
| **Flag** | `-XInstanceSigs`                                                                                                           |

This extension allows you to write type signatures in the instance definitions
for type classes. This signature must be identical to, or more polymorphic than,
the signature provided in the class definition.

#### LambdaCase

|          |                                                                                                                        |
| :------- | :--------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`LambdaCase`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-LambdaCase) |
| **Flag** | `-XLambdaCase`                                                                                                         |

Enables `\case` as an alternative to `case <...> of`. This often results in much
cleaner code.

#### LiberalTypeSynonyms

|          |                                                                                                                                          |
| :------- | :--------------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`LiberalTypeSynonyms`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-LiberalTypeSynonyms) |
| **Flag** | `-XLiberalTypeSynonyms`                                                                                                                  |

This extension moves the type synonym validity check to _after_ the synonym is
expanded, rather than before. This makes said synonyms more useful in the
context of type-level programming constructs.

#### MonadComprehensions

|          |                                                                                                                                                        |
| :------- | :----------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`GeneralizedNewtypeDeriving`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-GeneralizedNewtypeDeriving) |
| **Flag** | `-XGeneralizedNewtypeDeriving`                                                                                                                         |

Enables a generalisation of the list comprehension notation that works across
any type that is an instance of `Monad`.

#### MultiParamTypeClasses

|          |                                                                                                                                              |
| :------- | :------------------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`MultiParamTypeClasses`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-MultiParamTypeClasses) |
| **Flag** | `-XMultiParamTypeClasses`                                                                                                                    |

Enables the ability to write type classes with multiple parameters. This is very
useful for type-level programming, and to express relationships between types in
typeclasses.

#### MultiWayIf

|          |                                                                                                                        |
| :------- | :--------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`MultiWayIf`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-MultiWayIf) |
| **Flag** | `-XMultiWayIf`                                                                                                         |

This extension allows GHC to accept conditional expressions with multiple
branches, using the guard-style notation familiar from function definitions.

#### NamedWildCards

#### NegativeLiterals

#### NoImplicitPrelude

|          |                                                                                                                                      |
| :------- | :----------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`NoImplicitPrelude`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-NoImplicitPrelude) |
| **Flag** | `-XNoImplicitPrelude`                                                                                                                |

Disables the implicit import of the prelude into every module. This enables us
to use `Prologue`, our own custom prelude (discussed in the section on
[prologue](#prologue)).

#### NumDecimals

|          |                                                                                                                          |
| :------- | :----------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`NumDecimals`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-NumDecimals) |
| **Flag** | `-XNumDecimals`                                                                                                          |

Enables writing integer literals using exponential syntax.

#### OverloadedLabels

|          |                                                                                                                                    |
| :------- | :--------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`OverloadedLabels`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-OverloadedLabels) |
| **Flag** | `-XOverloadedLabels`                                                                                                               |

Enables support for Overloaded Labels, a type of identifier whose type depends
both on its literal text and its kind. This is similar to `-XOverloadedStrings`.

#### OverloadedStrings

|          |                                                                                                                                      |
| :------- | :----------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`OverloadedStrings`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-OverloadedStrings) |
| **Flag** | `-XOverloadedStrings`                                                                                                                |

Enables overloading of the native `String` type. This means that string literals
are given their type based on contextual information as, and a string literal
can be used to represent any type that is an instance of `IsString`.

#### PatternSynonyms

|          |                                                                                                                                  |
| :------- | :------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`PatternSynonyms`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-PatternSynonyms) |
| **Flag** | `-XPatternSynonyms`                                                                                                              |

Pattern synonyms enable giving names to parametrized pattern schemes. They can
also be thought of as abstract constructors that don’t have a bearing on data
representation. They can be unidirectional or bidirectional, and are incredibly
useful for defining clean APIs to not-so-clean data.

#### QuasiQuotes

|          |                                                                                                                          |
| :------- | :----------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`QuasiQuotes`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-QuasiQuotes) |
| **Flag** | `-XQuasiQuotes`                                                                                                          |

Quasi-quotation allows patterns and expressions to be written using
programmer-defined concrete syntax. This extension enables the use of quotations
in Haskell source files.

#### RankNTypes

|          |                                                                                                                        |
| :------- | :--------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`RankNTypes`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-RankNTypes) |
| **Flag** | `-XRankNTypes`                                                                                                         |

Enables the ability to express arbitrary-rank polymorphic types (those with a
`forall` which is not on the far left of the type). These are incredibly useful
for defining clean and safe APIs.

#### RecursiveDo

|          |                                                                                                                          |
| :------- | :----------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`RecursiveDo`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-RecursiveDo) |
| **Flag** | `-XRecursiveDo`                                                                                                          |

This extension enables recursive binding in do-notation for any monad which is
an instance of MonadFix. Bindings introduced in this context are recursively
defined, much as for an ordinary `let`-expression.

#### ScopedTypeVariables

|          |                                                                                                                                          |
| :------- | :--------------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`ScopedTypeVariables`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ScopedTypeVariables) |
| **Flag** | `-XScopedTypeVariables`                                                                                                                  |

This enables lexical scoping of type variables introduced using an explicit
`forall` in the type signature of a function. With this extension enabled, the
scope of this variables is extended to the function body.

#### StandaloneDeriving

|          |                                                                                                                                        |
| :------- | :------------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`StandaloneDeriving`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-StandaloneDeriving) |
| **Flag** | `-XStandaloneDeriving`                                                                                                                 |

Allows the creation of `deriving` declarations that are not directly associated
with the class that is being derived. This is useful in the context where you
need to create orphan instances, or to derive some non-default classes.

#### Strict

|          |                                                                                                                |
| :------- | :------------------------------------------------------------------------------------------------------------- |
| **Name** | [`Strict`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-Strict) |
| **Flag** | `-XStrict`                                                                                                     |

We have found that making our code strict-by-default allows us to reason much
more easily about its performance. When we want lazy evaluation, we use a
combination of the negation flags and lazy pattern matching to achieve our
goals.

When disabling strict for a module using `-XNoStrict`, you also need to add
`-XNoStrictData`.

#### StrictData

|          |                                                                                                                        |
| :------- | :--------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`StrictData`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-StrictData) |
| **Flag** | `-XStrictData`                                                                                                         |

Much like the above, this helps with reasoning about performance, but needs to
be explicitly disabled in contexts where the strictness is undesirable.

#### TemplateHaskell

|          |                                                                                                                                  |
| :------- | :------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`TemplateHaskell`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TemplateHaskell) |
| **Flag** | `-XTemplateHaskell`                                                                                                              |

Enables the usage of Template Haskell, including the syntax for splices and
quotes. TH is a meta-language that allows for generating Haskell code from
arbitrary input.

#### TupleSections

|          |                                                                                                                              |
| :------- | :--------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`TupleSections`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TupleSections) |
| **Flag** | `-XTupleSections`                                                                                                            |

Much like we can do operator sections to partially apply operators, this
extension enables partial application of tuple constructors.

#### TypeApplications

|          |                                                                                                                                    |
| :------- | :--------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`TypeApplications`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeApplications) |
| **Flag** | `-XTypeApplications`                                                                                                               |

This extension allows you to use visible type application in expressions. This
allows for easily providing types that are ambiguous (or otherwise) to GHC in a
way that doesn't require writing complete type signatures. We make heavy use of
type applications in our type-level programming and API.

These should always be used as an alternative to `Proxy`, as they are just as
useful for passing type information around without provision of data, and lead
to nice and clean APIs.

#### TypeFamilies

|          |                                                                                                                            |
| :------- | :------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`TypeFamilies`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeFamilies) |
| **Flag** | `-XTypeFamilies`                                                                                                           |

Type Families can be thought of as type-level functions, or functions on types.
They are slightly more verbose than functional dependencies, but provide much
better reusability, clearer contexts, and are far easier to compose. They should
always be used in preference to functional dependencies.

When using Type Families, please keep the following things in mind:

- Prefer open type families to closed type families.
- Use closed type families if you want a fall-back when checking types.

  ```hs
  type family SumOf where
      SumOf Vector a      = Vector
      SumOf a      Vector = Vector
      SumOf a      a      = a
  ```

- Do not use closed type families unless you are absolutely sure that your type
  family should not be able to be extended in the future.

#### TypeFamilyDependencies

|          |                                                                                                                                                |
| :------- | :--------------------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`TypeFamilyDependencies`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeFamilyDependencies) |
| **Flag** | `-XTypeFamilyDependencies`                                                                                                                     |

This extension allows type families to be annotated with injectivity information
using syntax similar to that used for functional dependencies. This information
is used by GHC during type-checking to resolve the types of expressions that
would otherwise be ambiguous.

#### TypeOperators

|          |                                                                                                                              |
| :------- | :--------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`TypeOperators`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeOperators) |
| **Flag** | `-XTypeOperators`                                                                                                            |

Type operators is a simple extension that allows for the definition of types
with operators as their names. Much like you can define term-level operators,
this lets you define type-level operators. This is a big boon for the
expressiveness of type-level APIs.

#### UnicodeSyntax

|          |                                                                                                                              |
| :------- | :--------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`UnicodeSyntax`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-UnicodeSyntax) |
| **Flag** | `-XUnicodeSyntax`                                                                                                            |

Enables unicode syntax for certain parts of the Haskell language.

#### ViewPatterns

|          |                                                                                                                            |
| :------- | :------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`ViewPatterns`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ViewPatterns) |
| **Flag** | `-XViewPatterns`                                                                                                           |

View patterns provide a mechanism for pattern matching against abstract types by
letting the programmer execute arbitrary logic as part of a pattern match. This
is very useful for the creation of clean APIs.

### Allowed Extensions

These extensions can be used in your code without reservation, but are not
enabled by default because they may interact negatively with other parts of the
codebase.

#### BlockArguments

|          |                                                                                                                                |
| :------- | :----------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`BlockArguments`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-BlockArguments) |
| **Flag** | `-XBlockArguments`                                                                                                             |

Block arguments allow expressions such as `do`, `\`, `if`, `case`, and `let`, to
be used as both arguments to operators and to functions. This can often make
code more readable than it otherwise would be.

#### GADTs

|          |                                                                                                              |
| :------- | :----------------------------------------------------------------------------------------------------------- |
| **Name** | [`GADTs`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-GADTs) |
| **Flag** | `-XGADTs`                                                                                                    |

This enables Generalised Algebraic Data Types, which expand upon normal data
definitions to allow both contexts for constructors and richer return types.
When this extension is enabled, there is a new style of data declaration
available for declaring GADTs.

#### HexFloatLiterals

|          |                                                                                                                                    |
| :------- | :--------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`HexFloatLiterals`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-HexFloatLiterals) |
| **Flag** | `-XHexFloatLiterals`                                                                                                               |

Enables the ability to specify floating point literals using hexadecimal to
ensure that no rounding or truncation takes place.

#### MagicHash

|          |                                                                                                                      |
| :------- | :------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`MagicHash`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-MagicHash) |
| **Flag** | `-XMagicHash`                                                                                                        |

Allows the use of `#` as a postfix modifier to identifiers. This allows the
programmer to refer to the names of many of GHC's internal unboxed types for use
in surface Haskell.

#### NumericUnderscores

|          |                                                                                                                                        |
| :------- | :------------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`NumericUnderscores`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-NumericUnderscores) |
| **Flag** | `-XNumericUnderscores`                                                                                                                 |

This extension allows breaking up of long numeric literals using underscores
(e.g `1_000_000_000`), which can often aid readability.

#### PolyKinds

|          |                                                                                                                      |
| :------- | :------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`PolyKinds`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-PolyKinds) |
| **Flag** | `-XPolyKinds`                                                                                                        |

This extension enables users to access the full power of GHC's kind system, and
allows for programming with kinds as well as types and values. It expands the
scope of kind inference to bring additional power, but is sometimes unable to
infer types at the value level as a result.

You should only enable `-XPolyKinds` in contexts where you need the power that
it brings.

#### Quantified Constraints

|          |                                                                                                                                              |
| :------- | :------------------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`QuantifiedConstraints`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-QuantifiedConstraints) |
| **Flag** | `-XQuantifiedConstraints`                                                                                                                    |

Quantified constraints bring additional expressiveness to the constraint
language used in contexts in GHC Haskell. In essence, it allows for contexts to
contain nested contexts, and hence for users to express more complex constraints
than they would otherwise be able to.

#### RoleAnnotations

|          |                                                                                                                                  |
| :------- | :------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`RoleAnnotations`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-RoleAnnotations) |
| **Flag** | `-XRoleAnnotations`                                                                                                              |

Role annotations allow programmers to constrain the type inference process by
specifying the roles of the class and type parameters that they declare.

#### UnboxedSums

|          |                                                                                                                          |
| :------- | :----------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`UnboxedSums`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-UnboxedSums) |
| **Flag** | `-XUnboxedSums`                                                                                                          |

Enables the syntax for writing anonymous, unboxed sum types. These can be very
useful for writing performance critical code, as they can be used as a standard
anonymous sum type, including in pattern matching and at the type level.

#### UnboxedTuples

|          |                                                                                                                              |
| :------- | :--------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`UnboxedTuples`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-UnboxedTuples) |
| **Flag** | `-XUnboxedTuples`                                                                                                            |

This extension enables the syntax and use of unboxed tuples. This can be thought
of as a dual to the above `-XunboxedSums` as it allows for the declaration and
manipulation of unboxed product types.

### Allowed With Care

If you make use of any of these extensions in your code, you should accompany
their usage by a source note that explains why they are used.

#### CApiFFI

|          |                                                                                                                  |
| :------- | :--------------------------------------------------------------------------------------------------------------- |
| **Name** | [`CApiFFI`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-CApiFFI) |
| **Flag** | `-XCApiFFI`                                                                                                      |

Enables the `capi` calling convention for foreign function declarations. This
should only be used when you need to call a foreign function using the C-level
API, rather than across the platform's ABI. This enables the programmer to make
use of preprocessor macros and the like, as the call is resolved as if it was
against the C language.

#### CPP

|          |                                                                                                          |
| :------- | :------------------------------------------------------------------------------------------------------- |
| **Name** | [`CPP`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-CPP) |
| **Flag** | `-XCPP`                                                                                                  |

Enables the C preprocessor. We strongly discourage use of the preprocessor, but
it is sometimes unavoidable when you need to do purely string-based
preprocessing of a Haskell source file. It should only be used if you have _no_
_other_ solution to your problem.

#### PostfixOperators

|          |                                                                                                                                    |
| :------- | :--------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`PostfixOperators`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-PostfixOperators) |
| **Flag** | `-XPostfixOperators`                                                                                                               |

Enables the definition of left-sections for postfix operators. Please take care
if you enable this that it does not lead to unclear code. You should not export
a postfix operator from a module, as we do not condone enabling this throughout
the entire codebase.

#### StaticPointers

|          |                                                                                                                                |
| :------- | :----------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`StaticPointers`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-StaticPointers) |
| **Flag** | `-XStaticPointers`                                                                                                             |

Allows static resolution of a limited subset of expressions to a value at
compile time. This allows for some precomputation of values during compilation
for later lookup at runtime. While this can be useful for some low-level code,
much care must be taken when it is used.

#### UndecidableInstances

|          |                                                                                                                                            |
| :------- | :----------------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`UndecidableInstances`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-UndecidableInstances) |
| **Flag** | `-XUndecidableInstances`                                                                                                                   |

This extension permits the definition of typeclass instances that could
potentially lead to non-termination of the type-checker. This is sometimes
necessary to define the instance you want, but care must be taken to ensure that
you only enable this extension when you are _sure_ that your instances are
terminating.

#### UndecidableSuperclasses

|          |                                                                                                                                                  |
| :------- | :----------------------------------------------------------------------------------------------------------------------------------------------- |
| **Name** | [`UndecidableSuperclasses`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-UndecidableSuperclasses) |
| **Flag** | `-XUndecidableSuperclasses`                                                                                                                      |

Permits the definition of superclass constraints which can potentially lead to
the non-termination of the type-checker. Much like the above, this is sometimes
necessary but should only be enabled when you are _sure_ that you will not cause
the typechecker to loop.

### Disallowed Extensions

If a language extension hasn't been listed in the above sections, then it is
considered to be disallowed throughout the Enso codebases. If you have a good
reason to want to use one of these disallowed extensions, please talk to Ara or
Wojciech to discuss its usage.

If an extension not listed above is _implied_ by one of the extensions listed
above (e.g. `-XRankNTypes` implies `-XExplicitForall`), then the implied
extension is also considered at least as safe as the category the implying
extension is in.
