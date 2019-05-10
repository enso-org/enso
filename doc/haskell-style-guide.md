# Haskell Style Guide
Like many style guides, this Haskell style guide exists for two primary reasons.
The first is to provide guidelines that result in a consistent code style across
all of the Luna codebases, while the second is to guide people towards a style
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
- [Functional Dependencies vs. Type Families](#functional-dependencies-vs-type-families)
- [Type Families](#type-families)
- [Proxy Types / Type Applications](#proxy-types--type-applications)

<!-- /MarkdownTOC -->

## Code Formatting
This section explains the rules for visually laying out your code. They provide
a robust set of guidelines for creating a consistent visual to the code.

### Whitespace
The rules for whitespace in the Luna codebases are relatively simple:

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

- Function signatures should wrap on the `=>` and `->`, and in the context of
  a doc comment should have each argument on a separate line.
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
Luna has some fairly simple general naming conventions, though the sections
below may provide more rules for use in specific cases.

- Types are written using `UpperCamelCase`.
- Variables and function names are written using `camelCase`.
- If a name contains an initialism or acronym, all parts of that initialism
  should be of the same case: `httpRequest` or `makeHTTPRequest`.
- Short variable names such as `a` and `b` should only be used in contexts where
  there is no other appropriate name (e.g. `flip (a, b) = (b, a)`). They should
  _never_ be used to refer to temporary data in a `where` or `let` expression.

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
module Luna.MyModule (module Luna.MyModule, module X) where

import Luna.MyModule.Class as X (foo, bar)

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
module Luna.MyModule where

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
define it in an internal module. For a module named `Luna.MyModule`, we can
define internal functions and data-types in `Luna.MyModule.Internal`. This means
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
Comments are a tricky area to get right, as we have found that comments often
expire quickly and, in absence of a way to validate them, remain incorrect for
long periods of time. That is not to say, however, that we eschew comments
entirely. Instead, we make keeping comments up to date an integral part of our
programming practice, while also limiting the types of comments that we allow.

When we write comments, we try to follow one general guideline. A comment should
explain _what_ and _why_, without mentioning _how_. The _how_ should be
self-explanatory from reading the code, and if you find that it is not, that is
a sign that the code in question needs refactoring.

Code should be written in such a way that it guides you over what it does, and
comments should not be used as a crutch for badly-designed code.

### Documentation Comments
One of the primary forms of comment that we allow across the Luna codebases is
the doc comment. These are intended to be consumed by users of the API, and use
the standard Haddock syntax. Doc comments should:

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
  , not (isUnLiftedType ty1)            -- see Note [Float Coercions (Unlifted)]
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
The Luna project has many internal libraries that are useful, but we have found
that maintaining these on Hackage while they are under such active development
is counterproductive.

Instead, libraries live in the `lib/` folder of the primary project with which
they are associated (Luna, Luna Studio, or Dataframes). These libraries may be
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

- It must be named `unsafeX`.
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

It is defined in [`lib/exception/`](https://github.com/luna/luna/tree/master/lib/exception)
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
- We import modules as their name. If you have a module `Luna.Space.MyType`, we
  import it qualified as `MyType`.
- Functions should be named with the assumption of being used qualified. This
  means that we rarely refer to the module name in the function name (e.g.
  `State.run` rather than `State.runState`).

### Data Declarations
When declaring data types in the Luna codebases, please make sure to keep the
following rules of thumb in mind:

- For single-constructor types:
  + Write the definition across multiple lines. 
  + Always name your fields.
  + Always generate lenses. 

  ```hs
  data Rectangle = MkRectangle
      { _width  :: Double
      , _height :: Double
      } deriving (Eq, Ord, Show)
  makeLenses ''Rectangle
  ```
- For multiple-constructor data-types:
  + Write the definition across multiple lines.
  + Never name your fields.
  + Generate prisms only when necessary.

  ```hs
  data Shape
      = ShapeCircle Circle
      | ShapeRect   Rectangle
      deriving (Eq, Ord, Show)
  ```

- Always prefer named fields over unnamed ones. You should only use unnamed
  fields if one or more of the following hold:
  + Your data type is one where you are are _sure_ that separate field access 
    will never be needed.
  + You are defining a multiple-constructor data type.
- Sort deriving clauses in alphabetical order, and derive the following for your
  type if logically correct:
  + General Types: `Eq`, `Generic`, `NFData`, `Ord`, `Show`.
  + Parametric 1-Types: `Applicative`, `Alternative`, `Functor`.
  + Monads: `Monad`, `MonadFix`.
  + Monad Transformers: `MonadTrans`.

#### Lenses
The Luna codebases make significant use of Lenses, and so we have some rules for
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
  `Luna.MyModule`, then the test file should be named `Luna.MyModuleSpec`.

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
New code should introduce no new warnings onto master. You may build with
warnings on your own branch, but the code that is submitted as part of a PR
should not introduce new warnings. You should also endeavour to fix any warnings
that you come across during development.

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
of `foo $ bar $ baz $ bam quux`, you should write `foo . bar. baz $ bam quux`
to use function composition.

## Language Extensions
Much like any sophisticated Haskell codebase, Luna makes heavy use of the GHC
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
high utility, that they are considered to be Luna's set of default extensions.
You can find said set of extensions for Luna itself defined in a
[common configuration file](https://github.com/luna/luna/blob/master/config/hpack-common.yaml).

#### AllowAmbiguousTypes

|          |                                                                                                                                          |
|:---------|:-----------------------------------------------------------------------------------------------------------------------------------------|
| **Name** | [`AllowAmbiguousTypes`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-AllowAmbiguousTypes) |
| **Flag** | `-XAllowAmbiguousTypes`                                                                                                                  |

This extension is particularly useful in the context of

#### Strict
Talk about `NoStrict`, the reasoning behind strict-by-default

#### StrictData
Talk about `NoStrictData`

### Allowed Extensions
These extensions can be used in your code without reservation, but are not
enabled by default because they may interact negatively with other parts of the
codebase.

### Allowed With Care
If you make use of any of these extensions in your code, you should accompany
their usage by a source note that explains why they are used.

### Disallowed Extensions
If a language extension hasn't been listed in the above sections, then it is
considered to be disallowed throughout the Luna codebases. If you have a good
reason to want to use one of these disallowed extensions, please talk to Ara or
Wojciech to discuss its usage.








## Functional Dependencies vs. Type Families
Do not use `Functional Dependencies`. Use `Type Families` instead. Type families
provide greater mechanism then functional dependencies in 99% use cases. If you
think you are in this 1% you are probably wrong. Type families are a little more
verbose, however, they provide much better code reusability, composability and
much clearer contexts.

For example, given

```
data Vector = Vector { _x :: Double, _y :: Double, _z :: Double } deriving (Show)
makeLenses ''Vector
```

instead of

```
class Sum a b c | a b -> c where
    add :: a -> b -> c

instance Sum Double Double Double where add = (+)
instance Sum Double Vector Vector where add n              (Vector x y z)    = Vector (x+n)  (y+n)  (z+n)
instance Sum Vector Double Vector where add (Vector x y z) n                 = Vector (x+n)  (y+n)  (z+n)
instance Sum Vector Vector Vector where add (Vector x y z) (Vector x' y' z') = Vector (x+x') (y+y') (z+z')
```

you should write

```
type family SumOf a b
class Sum a b where
    add :: a -> b -> SumOf a b

type instance SumOf Double Double = Double
type instance SumOf Vector Double = Vector
type instance SumOf Double Vector = Vector
type instance SumOf Vector Vector = Vector
instance Sum Double Double where add = (+)
instance Sum Double Vector where add n              (Vector x y z)    = Vector (x+n)  (y+n)  (z+n)
instance Sum Vector Double where add (Vector x y z) n                 = Vector (x+n)  (y+n)  (z+n)
instance Sum Vector Vector where add (Vector x y z) (Vector x' y' z') = Vector (x+x') (y+y') (z+z')
```

In order to make one of the issues with functional dependencies more obvious,
consider the following function type:

```
foo :: Sum a b c => a -> b -> Int
```

It just means that we take two things `a` and `b`, which should be addable, but
we don't care about the result (for example we are guaranteed it feels some
constraints - like it could be printable) and we result in an `Int`. We've got
an unused type variable `c` in the context head, but there is currently no way
to hide it (unless GHC supports ImpredicatibleContexts, but even then it would
be an anti-pattern in such cases). So we cannot now define:

```
type Ctx a b = forall c. Sum a b
foo :: Ctx a b => a -> b -> Int
```

It will be rejected by the compiler.


## Type Families
* Prefer using open type families over closed ones, because they implement
  [Open World Assumption](https://en.wikipedia.org/wiki/Open-world_assumption).
  Closed type families are useful if you want a "fallback option" when checking
  types. For example:

```
type family SumOf where
    SumOf Vector a      = Vector
    SumOf a      Vector = Vector
    SumOf a      a      = a
```
is completely valid if you are sure, that `SumOf` should work only for a pair of
(any type and `Vector`), (`Vector` and any type) or (the same two types). Only
if you can prove that it should never be extended further, use closed type
families.

## Proxy Types / Type Applications
Do not use proxy types, use modern Haskell's type application instead. Proxy
types are useful for passing type information around, without the need to
provide data. For example, imagine we want to create a renderer typeclass, which
would be able to generate images using either SVG or WebGL backends. In the most
basic way we could implement it as follow:

```
data SVG   = SVG   deriving (Show)
data WebGL = WebGL deriving (Show)

class Renderable backend a where
    render :: backend -> a -> Image

instance Renderable SVG Circle    where render _ (Circle r)      = ...
instance Renderable SVG Rectangle where render _ (Rectangle w h) = ...

s = render SVG (Circle 10)
```

There are few ugly design decisions here:
* We have defined `SVG` and `WebGL` constructors and we will use them only to
  tell which renderer to use. They are not useful for anything more than that.
* Every type instance have to discard its first element, it is passed only to
  choose the right instance.

Better (but still not the best) approach is to use Proxy instead. Proxy is
defined as `data Proxy a = Proxy`:

```
data SVG
data WebGL

class Renderable backend a where
    render :: Proxy backend -> a -> Image

instance Renderable SVG Circle    where render _ (Circle r)      = ...
instance Renderable SVG Rectangle where render _ (Rectangle w h) = ...


s = render (Proxy :: Proxy SVG) (Circle 10)
```

Using this approach:
* We do not define constructors anymore, only types to choose the right
  instance, which is good
* We still need to discard the first argument when defining the type class
  instance
* The code is much more verbose and hard to read, which is bad.

We can use type applications for the rescue:

```
data SVG
data WebGL

class Renderable backend a where
    render :: a -> Image

instance Renderable SVG Circle    where render (Circle r)      = ...
instance Renderable SVG Rectangle where render (Rectangle w h) = ...


s = render @SVG (Circle 10)
```
This approach is clear, concise and in modern Haskell style. In order to define
the type class, you need the `-XAllowAmbiguousTypes` extension, which basically
lets you use in instance head type variables that are not deduced if not passed
explicitly.
