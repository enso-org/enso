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
  - [Imports](#imports)
  - [Auto-Formatting](#auto-formatting)
- [Commenting](#commenting)
  - [Documentation Comments](#documentation-comments)
  - [Source Notes](#source-notes)
  - [Other Comment Usage](#other-comment-usage)
- [Program Design](#program-design)
  - [Namespaces](#namespaces)
  - [Modules](#modules)
  - [Data Declarations](#data-declarations)
  - [Testing and Benchmarking](#testing-and-benchmarking)
  - [Errors, Warnings, and Lints](#errors-warnings-and-lints)
- [Language Extensions](#language-extensions)
  - [Default Extensions](#default-extensions)
  - [Allowed Extensions](#allowed-extensions)
  - [Allowed With Care](#allowed-with-care)
  - [Disallowed Extensions](#disallowed-extensions)
- [Code layout](#code-layout)
- [Code Alignment](#code-alignment)
- [Comments in code](#comments-in-code)
- [Naming](#naming)
- [Libraries](#libraries)
- [Extensions](#extensions)
- [Imports](#imports-1)
- [Safety](#safety)
- [Data-Type Definitions](#data-type-definitions)
- [Lenses](#lenses)
- [Functional Dependencies vs. Type Families](#functional-dependencies-vs-type-families)
- [Type Families](#type-families)
- [Proxy Types](#proxy-types)

<!-- /MarkdownTOC -->

## Code Formatting

### Whitespace
The rules for whitespace in the Luna codebases are relatively simple:

- 4 spaces are used for indentation, with no tabs.
- There should not be any trailing whitespace. 
- There should be no spurious whitespace within the lines, unless it is used for
  [alignment](#alignment) as discussed below.

### Line Wrapping

Break on operators where possible
Break at highest precedence

### Alignment


### Imports
Design for qualified imports

### Auto-Formatting
While we have attempted to use haskell auto-formatters to enforce many of the 
above stylistic choices in this document, none have been found to be flexible
enouh

## Commenting
What and why, not how

### Documentation Comments

### Source Notes
Sometimes an exception to the 'not how' rule.

### Other Comment Usage

## Program Design

### Namespaces

### Modules
Design for qualified imports.

### Data Declarations

### Testing and Benchmarking

### Errors, Warnings, and Lints
Default error config. New code should be warnings free.

## Language Extensions


Go through the list
Note which ones are safe to enable by default
Not all are available depending on the compiler in use

### Default Extensions
The following language extensions are considered to be so safe, or to have such
high utility, that they are considered to be Luna's set of default extensions.
You can find said set of extensions for the 

#### AllowAmbiguousTypes

|          |                                                                                                                                          |
|:---------|:-----------------------------------------------------------------------------------------------------------------------------------------|
| **Name** | [`AllowAmbiguousTypes`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-AllowAmbiguousTypes) |
| **Flag** | `-XAllowAmbiguousTypes`                                                                                                                  |

### Allowed Extensions

### Allowed With Care

### Disallowed Extensions
If a language extension hasn't been listed in the above sections, then it is
considered to be disallowed throughout the Luna codebases. If you have a good
reason to want to use one of these disallowed extensions, please talk to Ara to
discuss its usage.













## Code layout
* Indentations are always 4 spaces, no tabs are allowed.
* Lines of code should not end with any empty spaces (most editors make it
  automatically for you)

## Code Alignment
* Try to align parts of expressions, which are vertically similar. This rule is
  used in all the examples in this document, so you can easily see how it is
  done in practice. To grasp the idea fast, consider:

  ```
  people   <- getAllPeople <$> worlds
  names    <- getName      <$> people
  surnames <- getSurnames  <$> names
  ```

## Comments in code
* Commenting code is a bad practice. Comments expire fast and are not validated.
  A code should be auto-explanatory. It should be written in such way, that
  reading it will guide you over what it does. If your code is too long or it is
  not obvious what it does by fast looking at it, you should split it over
  well-named functions.
* Use comments to indicate bugs, todos and third-party bugs (preferably with a
  link to a bug tracker/discussion)
* Use comments to explain why your code is safe (as described in a later
  section).
* Use comments to disable code while developing it.
* Always use comments to indicate where a mathematical formula comes from.
* Do not use comments otherwise.

## Naming
The following are general naming rules you should follow. There are more
context-specific rules and you will find them in the following sections.

* Names in general (variables, functions, types) are written using camel case
  convention and it should be easy to understand what a particular name means.
  For example, names like `a` or `b` are allowed in very specific use cases like
  `flip (a,b) = (b,a)`, however, you cannot use them to keep temporary data in a
  where clause.

## Libraries
* The basic library is `Prologue`, you should always disable `Prelude` auto
  import and import `Prologue` instead. Please be sure to read the source code
  of `Prologue`. In most cases it is compatible with `Prelude`, however, it was
  designed with safety in the first place, so there are some basic differences.
  For example, the `head` function returns value encoded in `Maybe`. If you want
  the unsafe behavior, use `unsafeHead` instead.
- Any uses of unsafe APIs should be accompanied by a comment explaining why it
  is safe.

## Extensions
The following extensions are considered safe and you can use them freely. You
can even enable all of them in your stack config file:
```
- AllowAmbiguousTypes
- ApplicativeDo
- Arrows
- BangPatterns
- BinaryLiterals
- ConstraintKinds
- DataKinds
- DefaultSignatures
- DeriveDataTypeable
- DeriveFoldable
- DeriveFunctor
- DeriveGeneric
- DeriveTraversable
- DoAndIfThenElse
- DuplicateRecordFields
- EmptyDataDecls
- FlexibleContexts
- FlexibleInstances
- FunctionalDependencies
- GeneralizedNewtypeDeriving
- InstanceSigs
- LambdaCase
- LiberalTypeSynonyms
- MonadComprehensions
- MonadFailDesugaring
- MultiWayIf
- NamedWildCards
- NegativeLiterals
- NoImplicitPrelude
- NumDecimals
- OverloadedLabels
- OverloadedStrings
- PackageImports
- QuasiQuotes
- RankNTypes
- RecursiveDo
- RelaxedPolyRec
- ScopedTypeVariables
- StandaloneDeriving
- TemplateHaskell
- TupleSections
- TypeApplications
- TypeFamilies
- TypeFamilyDependencies
- TypeOperators
- ViewPatterns
```

## Imports
Organizing your imports makes imported functions and modules easier to find in a
file. They should be organized in four sections, each separated by a single
empty line:

1. Modules to be re-exported, all imported using `qualified` syntax into `X`
   scope (see example below).
2. `Prologue` and all `Prelude` like modules.
3. Qualified imported modules.
4. Non-qualified imported modules with explicit function listings, unless you
   import really well-known module.

There is no need to leave extra space for `qualified` keyword in second and
fourth section. Each section can be omitted if empty. Additionally, when
importing type constructors, list them explicitly rather than using `(..)`.

For example, this is correct imports section:

```
module MyModule (module MyModule, module X) where

import MyModule.Class as X (foo, bar)

import Prologue

import qualified Data.Map            as Map
import qualified Control.Monad.State as State

import Vector    (Vector (Vector), test)
import Rectangle (Rectangle)
```

If the module does not re-export anything, you should use the simplified form
instead:

```
module MyModule where

import Prologue

import qualified Data.Map            as Map
import qualified Control.Monad.State as State

import Vector    (Vector (Vector), test)
import Rectangle (Rectangle)
```

## Safety
Being sure that we can trust the code is very important, thus:

* Every non-total function should be marked unsafe. For example,
  `unsafeHead (a:_) = a`.
* You cannot use unsafe functions in any bigger code block - only in very small
  utilities that obviously prove that the usage of an unsafe function is safe in
  this context. Such functions should be marked unsafe if they do not guarantee
  totality. Please use comments to describe why you use unsafe function and why
  it is safe in a particular context unless it is very obvious.
* Never use irrefutable patterns. Anything like `Just a = foo` or `a : as = lst`
  is always wrong code. Always.

## Data-Type Definitions
  * Always generate Lens instances for single constructor data types.
  * Never generate Lens instances for multi-constructor data types.
  * If it is possible and logically correct, always derive following instances
    for your data types:
    * general: `Show`, `Eq`, `Ord`, `NFData`, `Generic`
    * parametric-1 types: `Functor`, `Applicative`, `Alternative`
    * monads: `Monad`, `MonadFix`
    * monad transformers: `MonadTrans`
  * Prefer named fields over unnamed ones. You can use unnamed fields if and
    only if at least one of those holds true:
    * Your data type is used locally and you will never need separate field
      access (preferably only within a single module)
    * You are sure nobody will ever need a separate field access (only accessing
      all the fields make sense).
  * Write single constructor data types with named fields in multiple lines

    ```
    data Rectangle = Rectangle
        { _width  :: Double
        , _height :: Double
        } deriving (Show)
    makeLenses ''Rectangle
    ```

    You can also use more compact form if you declare many connected data types

    ```
    data Rectangle = Rectangle { _width  :: Double, _height :: Double } deriving (Show)
    data Circle    = Circle    { _radius :: Double                    } deriving (Show)
    data Line      = Line      { _begin  :: Point , _end :: Point     } deriving (Show)
    makeLenses [''Rectangle, ''Circle, ''Line]
    ```

  * Do not use names when writing multiple constructor data types. If you want
    to name your fields it is much better to create a bundle of data types like:

    ```
    data Circle    = Circle    { _radius :: Double                    } deriving (Show)
    data Rectangle = Rectangle { _width  :: Double, _height :: Double } deriving (Show)
    data Shape
        = ShapeCircle    Circle
        | ShapeRectangle Rectangle
        deriving (Show)
    makeLenses ''[Circle, Rectangle]
    ```

  * :warning: Use rarely! Only when you are sure nobody will ever need to access
    only one of these fields! Write single constructor data types with unnamed
    fields within a single line.


    ```
    data Rectangle = Rectangle Double Double deriving (Show)
    ```

  * :warning: Use rarely! Only when you are sure nobody will ever need to access
    only one of these fields! Write multiple constructor data types with unnamed
    fields in multiple lines:

    ```
    data Shape
        = Circle Double
        | Rectangle Double Double
        deriving (Show)
    ```
  * Sort deriving classes in alphabetic order.

## Lenses
If multiple data types need field of the same name and you want to generate Lens
instances for it, use Prologue's Lens wrappers as follow:

```
data Vector = Vector { __x :: Double, __y::Double, __z::Double } deriving (Show)
data Point  = Point  { __x :: Double, __y::Double              } deriving (Show)
makeLenses ''Vector
makeLenses ''Point

class           Dim1 a where x :: Lens' a Double
class Dim1 a => Dim2 a where y :: Lens' a Double

instance Dim1 Vector where x = vector_x
instance Dim2 Vector where y = vector_y
instance Dim1 Point  where x = point_x
instance Dim2 Point  where y = point_y
```
Names like `vector_x`, `vector_y`, `point_x` and `point_y` are automatically
generated if you use double underscore instead of single in data type
definition.

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

## Proxy Types
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
