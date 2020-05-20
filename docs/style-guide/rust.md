---
layout: style-guide
title: Rust Style Guide
category: style-guide
tags: [style-guide]
order: 3
---

# Rust Style Guide
Like many style guides, this Rust style guide exists for two primary reasons.
The first is to provide guidelines that result in a consistent code style across
all of the Enso codebases, while the second is to guide people towards a style
that is expressive while still easy to read and understand.

In general, it aims to create a set of 'zero-thought' rules in order to ease the
programmer burden; there is usually only _one way_ to lay out code correctly.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Code Formatting](#code-formatting)
  - [Line Width](#line-width)
  - [Imports](#imports)
  - [Sections](#sections)
  - [Vertical Spacing](#vertical-spacing)
  - [Multi-Line Expressions](#multi-line-expressions)
  - [Vertical Alignment](#vertical-alignment)
  - [Spacing](#spacing)
  - [Impl Definitions](#impl-definitions)
  - [Getters and Setters](#getters-and-setters)
  - [Trait Exports](#trait-exports)
- [Naming](#naming)
- [Package Structure and Naming](#package-structure-and-naming)
  - [The Public API](#the-public-api)
- [Build Tooling](#build-tooling)
- [Commenting](#commenting)
  - [Documentation Comments](#documentation-comments)
  - [Source Notes](#source-notes)
  - [TODO Comments](#todo-comments)
  - [Other Comment Usage](#other-comment-usage)
- [Program Design](#program-design)
  - [Code Complexity](#code-complexity)
  - [Safety](#safety)
  - [Testing and Benchmarking](#testing-and-benchmarking)
  - [Warnings, and Lints](#warnings-and-lints)

<!-- /MarkdownTOC -->

## Code Formatting
This section explains the rules for visually laying out your code. They provide
a robust set of guidelines for creating a consistent visual to the code.

Code style is _far_ more than just the visual formatting of the code, especially
as formatting can often be automated. According to the documentation of rustfmt,
"formatting code is a mostly mechanical task which takes both time and mental
effort." While, in many cases, the programmer can be relieved of this burden
through use of an automated formatter, it is sometimes the case that such a tool
imposes _more_ cognitive load in programmers. With rustfmt, programmers tend to
have to refactor long lines to use variables, and move code to specific modules
or sections lest rustfmt produce code that is hard to read and write. Thus, it
is very important to write code in such a way that we can be proud of its
quality.

Due to the fact that `rustfmt` doesn't support multiple of our requirements, we
have created a guide for how to format Rust code for this project. Please read
it carefully.

We hope that, in the future, `rustfmt` will come to support many of the things
described below, but even so, many portions of this guide will need to be
handled manually.

### Line Width
Each line in the source file should be of a maximum of 80 characters of text.
This includes comments.

### Imports
The imports section at the top of a file should be separated into four groups.
These groups should be sorted in alphabetical order and are divided as follows:

```rust
// Group 1: sub-module definitions.
// Group 2: prelude-like imports.
// Group 3: local-crate imports.
// Group 4: external imports.
```

Please look at the following by way of example:

```rust
pub mod display_object;

use crate::prelude::*;

use crate::closure;
use crate::data::opt_vec::OptVec;
use crate::dirty;
use crate::system::web::group;

use nalgebra::Matrix4;
use nalgebra::Vector3;
```

### Sections
Rust source files should be divided into sections, with a header placed before
the definition of each new concept in a file.

By the term "concept," we are referring primarily to a structure with a set of
related implementations. However if the related implementations rely on some
simple helper structs, these may also be defined in the same section. A section
should have a header as follows.

```rust
// ===================
// === SectionName ===
// ===================
```

Additionally, the code in each section should further be divided into
sub-sections that group relevant functionality within the section. The header
for a sub-section is as follows.

```rust
// === SubSectionName ===
```

At least one section should be defined in every file.

#### An Example of Using Sections
Here is a large-scale example of how sections should be used in source files.

```rust
// =================
// === AxisOrder ===
// =================

/// Defines the order in which particular axis coordinates are processed. Used
/// for example to define the rotation order in `DisplayObject`.
pub enum AxisOrder {XYZ,XZY,YXZ,YZX,ZXY,ZYX}

impl Default for AxisOrder {
    fn default() -> Self {Self::XYZ}
}



// =================
// === Transform ===
// =================

/// Defines the order in which transformations (scale, rotate, translate) are
/// applied to a particular object.
pub enum TransformOrder {
    ScaleRotateTranslate,
    ScaleTranslateRotate,
    RotateScaleTranslate,
    RotateTranslateScale,
    TranslateRotateScale,
    TranslateScaleRotate
}

impl Default for TransformOrder {
    fn default() -> Self { Self::ScaleRotateTranslate }
}



// =============================
// === HierarchicalTransform ===
// =============================

pub struct HierarchicalTransform<OnChange> {
    transform        : Transform,
    transform_matrix : Matrix4<f32>,
    origin           : Matrix4<f32>,
    matrix           : Matrix4<f32>,
    pub dirty        : dirty::SharedBool<OnChange>,
    pub logger       : Logger,
}

impl<OnChange> HierarchicalTransform<OnChange> {
    pub fn new(logger:Logger, on_change:OnChange) -> Self {
        let logger_dirty     = logger.sub("dirty");
        let transform        = default();
        let transform_matrix = Matrix4::identity();
        let origin           = Matrix4::identity();
        let matrix           = Matrix4::identity();
        let dirty            = dirty::SharedBool::new(logger_dirty,on_change);
        Self {transform,transform_matrix,origin,matrix,dirty,logger}
    }
}


// === Getters ===

impl<OnChange> HierarchicalTransform<OnChange> {
    pub fn position(&self) -> &Vector3<f32> {
        &self.transform.position
    }

    pub fn rotation(&self) -> &Vector3<f32> {
        &self.transform.rotation
    }

    ...
}


// === Setters ===

impl<OnChange:Callback0> HierarchicalTransform<OnChange> {
    pub fn position_mut(&mut self) -> &mut Vector3<f32> {
        self.dirty.set();
        &mut self.transform.position
    }

    pub fn rotation_mut(&mut self) -> &mut Vector3<f32> {
        self.dirty.set();
        &mut self.transform.rotation
    }

    ...
}
```

### Vertical Spacing
We use the following rules for the amount of vertical space separating various
constructs in the source:

- 3 blank lines after imports.
- 3 blank lines before each section.
- 2 blank lines before each sub-section.
- 1 blank line after each section / sub-section.
- 1 blank line before functions / structures / impls.
- 1 blank line at the end of the file.

Please note that the spacing 'overlaps', in that if multiple rules, you should
take the maximum of the spacings that apply. For example, if you have a section
following the imports, you only use three lines of spacing.

### Multi-Line Expressions
In an ideal world, all expressions in the code should be a single line. This is
because multi-line expressions are usually hard to read, and because they can
introduce lots of noise in the code. In the vast majority of cases, the presence
of a multi-line expression indicates that the code needs refactoring.

Please try to refactor portions of multi-line expressions to well-named
variables, and divide them up to a set of single-line expressions.

#### Multi-Line Expression Examples
The following is an example of poorly formatted code:

```rust
pub fn new() -> Self {
    let shape_dirty = ShapeDirty::new(logger.sub("shape_dirty"),
        on_dirty.clone());
    let dirty_flag = MeshRegistryDirty::new(logger.sub("mesh_registry_dirty"),
        on_dirty);
    Self { dirty_flag, dirty_flag }
}
```

The following is an example of the same code properly formatted:

```rust
pub fn new() -> Self {
    let sub_logger  = logger.sub("shape_dirty");
    let shape_dirty = ShapeDirty::new(sub_logger,on_dirty.clone());
    let sub_logger  = logger.sub("mesh_registry_dirty");
    let dirty_flag  = MeshRegistryDirty::new(sub_logger,on_dirty);
    Self {shape_dirty,dirty_flag}
}
```

### Vertical Alignment
In order to create a visual flow to our code that aids readability, the
following constructs should be aligned vertically where possible:

- Assignment operators (`=`)
- Type operators (`:`)
- Match arrows (`=>`)
- Similar parameters or types

#### A Vertical Alignment Example
The following is an example of a function that correctly uses the vertical
alignment rules above:

```rust
impl Printer for GlobalVarStorage {
    fn print(&self, builder:&mut Builder) {
        match self {
            Self::ConstStorage      => build!(builder,"const"),
            Self::UniformStorage    => build!(builder,"uniform"),
            Self::InStorage  (qual) => build!(builder,"in" ,qual),
            Self::OutStorage (qual) => build!(builder,"out",qual),
        }
    }
}
```

### Spacing
The following spacing rules are _also_ employed in order to create a visual flow
to our code to aid readability:

- The type operator is not spaced: `fn test(foo:String, bar:Int) { ... }`
- Commas between complex expressions (including the argument list) are spaced
- Commas between simple elements are not spaced: `Result<Self,Error>`
- Arguments to functions are not spaced: `build(builder,"out",qual)`
- Operators are always spaced: `let foo = a + b * c;`

#### Spacing Examples as Function Definitions
The following function definitions are all good examples of correct use of
spacing.

```rust
pub fn new<Dom:Str>(dom:Dom, logger:Logger) -> Result<Self,Error> {
    ...
}
```

```rust
pub fn new<Dom:Str>(dom:Dom, logger:Logger) -> Result<Self,Error> {
    ...
}
```

```rust
pub fn new<Dom:Str>
(dom:Dom, logger:Logger, on_dirty:OnDirty) -> Result<Self,Error> {
    ...
}
```

```rust
pub fn new<Dom:Str>
(dom:Dom, logger:Logger, on_dirty:OnDirty, on_remove:OnRemove)
-> Result<Self,Error> {
    ...
}
```

```rust
pub fn new<Dom:Str>
( dom        : Dom
, logger     : Logger
, on_dirty   : OnDirty
, on_remove  : OnRemove
, on_replace : OnReplace
) -> Result<Self,Error> {
    ...
}
```

Long `where` clauses are formatted like this:

```rust
pub fn new<D,L>(dom:D, logger:L) -> Result<Self,Error>
where D:AsRef<str>, L:IsLogger {
    ...
}
```

Or, in case they are really long, like this:

```rust
pub fn new<D,L>(dom:D, logger:L) -> Result<Self,Error>
where D:AsRef<str>
      L:IsLogger
      ... {
    ...
}
```

### Impl Definitions
In order to aid in fast discovery of the header of an impl definition, we use
the following style. In all cases, the `where` block should be placed after a
line break.


```rust
// No constraints
impl<T> Printer for Option<T> {
    ...
}
```

```rust
// Some constraints
impl<T:Printer>
Printer for Option<T> {
    ...
}
```

```rust
// Constraints in where block
impl<T> Printer for Option<T>
where T: Printer {
    ...
}
```

### Getters and Setters
We have the following rules for getters and setters in our codebase.

- Getters do not have the `get_` prefix, while setters do have the `set_`
  prefix.
- If a setter is provided, a `mut` accessor should be provided as well.

Correct examples for the definition of getters and setters can be found below:

```rust
fn field(&self) -> &Type {
    &self.field
}

fn field_mut(&mut self) -> &mut Type {
    &mut self.field
}

fn set_field(&mut self, val:Type) {
    *self.field_mut = val;
}
```

### Trait Exports
All names should be designed to be used in a qualified fashion. This does,
however, make one situation quite tricky. In order to use methods defined inside
a trait, that trait has to be in scope.

Consider a trait `display::Object`. We want to use it in a function definition
like the following `fn test<T:display::Object>(t:T) { ... }`, and we also want
the ability to use methods defined in the trait (and hence it has to be in
scope). Under these circumstances, `clippy` warns that `display::Object` is
being subject to unnecessary qualification, but we don't want to perform the
replacement.

In order to export traits, please rename them using the following convention:

```rust
/// Common traits.
pub mod traits {
    // Read the Rust Style Guide to learn more about the used naming.
    pub use super::Object    as TRAIT_Object;
    pub use super::ObjectOps as TRAIT_ObjectOps;
}
```

Once we have such a definition, we can import traits into scope using the simple
`use display::object::traits::*`, which will avoid any warnings about
unnecessary qualification.

## Naming
Enso has some fairly simple general naming conventions, though the sections
below may provide more rules for use in specific cases.

- Types are written using `UpperCamelCase`.
- Variables and function names are written using `snake_case`.
- If a name contains an initialism or acronym, all parts of that initialism
  should be lower-case: `make_http_request`, not `make_HTTP_request`.
- Short variable names such as `a` and `b` should only be used in the following
  contexts:
  - Where there is no other appropriate name.
  - Named lifetimes.
  They should _never_ be used to refer to temporary data in a function, as all
  temporaries should be given descriptive names.
- Names should be descriptive, even if this makes them longer.
- Any function that performs an unsafe operation that is not documented in its
  type (e.g. `fn head<T>(ts: Vec<T>) -> T`, which fails if the list is empty),
  must be named using the word 'unsafe' (e.g. `unsafeHead`). For more
  information on unsafe function usage, see the section on [safety](#safety).
- Naming should use American English spelling.

## Package Structure and Naming
Enso follows the standard rust convention for structuring crates, as provided
by `cargo new`. This is discussed more in depth
[here](https://learning-rust.github.io/docs/a4.cargo,crates_and_basic_project_structure.html#Project-Structure).

In order to match up with the project naming convention we use for Scala and
Java projects, any rust code must be in a directory named using `UpperCamelCase`
in the root of the project (e.g. `enso/BaseGL`).

### The Public API
Whereas Rust defaults to making module members _private_ by default, this is not
the philosophy used by the Enso codebases. We tend to want our codebase to be
flexible for consumers, so we tend to avoid making things private. Instead, we
use the concept of an `internal` module to separate public from private.

If you are writing code in a module `foo.bar.baz` and would like to signal that
a particular construct (e.g. a function) is for internal use in that package,
you should create a `foo.bar.baz.internal` package. You can then write the
relevant language construct in that package instead of the source package.

#### Using Access Modifiers
Given Rust's performance guarantees, making things `pub` has no impact on the
performance of the compiled code. As a result, the _only_ circumstance under
which things are allowed to not be `pub` is when doing so would allow consumers
of an API to break internal guarantees provided by that API (e.g. building an
immutable collection on top of a mutable buffer).

## Build Tooling
All Rust projects are built and managed using [cargo](https://doc.rust-lang.org/cargo/).

## Commenting
Comments in code are a tricky area to get right as we have found that comments
often expire quickly, and in absence of a way to validate them, remain incorrect
for long periods of time. In order to best deal with this problem, we make the
keeping of comments up-to-date into an integral part of our programming practice
while also limiting the types and kinds of comments we allow.

Comments across the Enso codebases fall into three main types:

- **Documentation Comments:** API documentation for all appropriate language
  constructs.
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
the doc comment. We use these comments to document the public API of a module,
as defined in [The Public API](#the-public-api). For constructs that _are_ part
of the public API, the following should be documented:

1. **Top-Level Type Definitions:** All top-level type definitions must have a
   doc comment.
2. **Functions:** Function documentation should provide at-a-glance intuition
   for how to use that function.

Documentation comments are intended for consumption by the users of the API, and
are written using the standard [rustdoc](https://doc.rust-lang.org/rustdoc/index.html)
syntax. Doc comments should contain:

1. **Summary:** A one-line summary of the construct's behaviour or purpose.
2. **Description (Optional):** Any useful information that would be necessary
   for a consumer of the API to know (that is not encoded in the types). This
   should be written in grammatically correct English.

Convention in rust is to not document function or return parameters, and so
rustdoc does not provide a way to do so.

An example of a valid set of comments for some rust code is as follows:

```rust
/// A representation of tree structures containing elements of type `T`.
pub trait Tree<T> {
  /// Provides a sequence representation of the tree.
  ///
  /// The function provides configurable behaviour for the order in which the
  /// tree is walked. See [WalkStrategy](org.enso.WalkStrategy.html) for
  /// the provided options.
  pub fn walk_to_sequence(self: &Self, order: WalkStrategy<T>) -> Vec<T> {
    // ...
  }

  fn getBuffer(self: &Self) -> Vec<T> {
    // ...
  }
}
```

Documentation comments should not reference internal implementation details, or
be used to explain choices made in the implementation. For this kind of info,
you should use [Source Notes](#source-notes) as described below.

You may document _more_ than what is specified here, but this is the _minimum_
required for acceptance at code-review time.

### Source Notes
Source Notes is a mechanism for moving detailed design information about a piece
of code out of the code itself. In doing so, it retains the key information
about the design while not impeding the flow of the code. They are used in the
following circumstances:

- **Design Information:** Documentation about _why_ something was written in a
  particular fashion, as well as information on the process that led to it being
  done this way.
- **Explaining Complexity:** If an implementation uses complex constructs or any
  elements that are non-obvious, these should be explained as part of a source
  note.
- **Knowledge Provenance:** Explaining where some knowledge (e.g. a mathematical
  formula or an algorithm) was obtained from. It is also useful to accompany
  these by some commentary on _why_ the choice was made.
- **Safety:** Any unsafe usage of a function must be accompanied by a source
  note that explains what makes this particular usage safe.

Source notes are detailed comments that, like all comments, explain both the
_what_ and the _why_ of the code being described. In very rare cases, it may
include some _how_, but only to refer to why a particular method was chosen to
achieve the goals in question.

A source note comment is broken into two parts:

1. **Referrer:** This is a small comment left at the point where the explanation
   is relevant. It takes the following form: `// Note [Note Name]`, where
   `Note Name` is a unique identifier across the codebase. These names should be
   descriptive, and make sure you search for it before using it, in case it is
   already in use.
2. **Source Note:** This is the comment itself, which is a large block comment
   placed after the first function in which it is referred to in the module. The
   first line names the note using the same referrer as above:
   `// Note [Note Name]`. The name(s) in the note are underlined using a string
   of the `=` (equals) character.

A source note may contain sections within it where necessary. These are titled
using the following syntax: `== Note [Note Name (Section Name)]`, and can be
referred to from a referrer much as the main source note can be.

Sometimes it is necessary to reference a source note in another module, but this
should never be done in-line. Instead, a piece of code should reference a source
note in the same module that references the other note while providing
additional context to that reference.

An example can be seen below:

```rust
/// A representation of tree structures containing elements of type `T`.
pub trait Tree<T> {
  /// Provides a sequence representation of the tree.
  ///
  /// The function provides configurable behaviour for the order in which the
  /// tree is walked. See [WalkStrategy](org.enso.WalkStrategy.html) for
  /// the provided options.
  pub fn walk_to_sequence(self: &Self, order: WalkStrategy<T>) -> Vec<T> {
    let mut output_vec = Vec.new(self.getBuffer().len()); // Note [Buffer Size]
    // ...
  }

  // Note [Buffer Size]
  // ==================
  // When working with the buffer for the tree walk, it is important that you
  // ensure....

  fn getBuffer(self: &Self) -> Vec<T> {
    // ...
  }
}
```

### TODO Comments
We follow a simple convention for `TODO` comments in our codebases:

- The line starts with `TODO` or `FIXME`.
- It is then followed by the author's initials `[ARA]`, or for multiple people
  `[ARA, MK]`, in square brackets.
- It is then followed by an explanation of what needs to be done.

For example:

```rust
// TODO [ARA] This is a bit of a kludge. Instead of X it should to Y, accounting
// for the fact that Z.
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

### Code Complexity
While we often have to write complex functionality, we want to ensure that the
code itself is kept as simple and easy to read as possible. To do this, please
use the following rules:

- Write single-line expressions wherever possible, rather than writing one
  complex chunk of code.
- Separate intermediate results out to their own variables with appropriate
  names. Even if they are temporaries, giving them a name is a great aid to code
  comprehension.

### Safety
Whereas most languages don't have a concept of _safety_, rust comes with a built
in notion of `unsafe`. When working with `unsafe` functions and code blocks, you
must account for the following:

- As unsafe functions are explicitly declared with the keyword `unsafe`, we do
  not need any special naming convention for them.
- Usage of unsafety should be confined to the smallest possible block.
- Usage of unsafety should be accompanied by a source note that explains why it
  is necessary, and any constraints on its usage.
- Unsafe function usage must be accompanied by a source note explaining how this
  usage of it is made safe.

Furthermore, we do not allow for code containing pattern matches that can fail.

### Testing and Benchmarking
New code should always be accompanied by tests. These can be unit, integration,
or some combination of the two, and they should always aim to test the new code
in a rigorous fashion.

- Testing should be performed as described in [the Rust book](https://doc.rust-lang.org/book/ch11-00-testing.html)
  and should use the functionality for testing built into the language.
- Tests should cover as much code as possible, and may be a combination of unit
  and integration tests.

Any performance-critical code should also be accompanied by a set of benchmarks.
These are intended to allow us to catch performance regressions as the code
evolves, but also ensure that we have some idea of the code's performance in
general.

- We use nightly rust in order to access the built-in [benchmarking](https://doc.rust-lang.org/unstable-book/library-features/test.html)
  functionality.
- We measure time, CPU, and memory usage where possible.
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

Sometimes it is impossible to fix a warning (often in situations involving the
use of macros). In such cases, you are allowed to suppress the warning locally,
but this must be accompanied by a source note explaining why you are doing so.
