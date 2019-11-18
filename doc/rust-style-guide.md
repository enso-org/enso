# Rust Style Guide
Like many style guides, this Rust style guide exists for two primary reasons.
The first is to provide guidelines that result in a consistent code style across
all of the Enso codebases, while the second is to guide people towards a style
that is expressive while still easy to read and understand.

In general, it aims to create a set of 'zero-thought' rules in order to ease the
programmer burden; there is usually only _one way_ to lay out code correctly.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Code Formatting](#code-formatting)
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

Primary formatting is dealt with using the Rust formatting tool
['rustfmt'](https://rust-lang.github.io/rustfmt/), which enforces rules around
whitespace, line-wrapping, and alignment. The Enso repository contains the main
[`.rustfmt.toml`](../.rustfmt.toml) configuration file, and this is what should
be used for all new Rust projects.

All files must be formatted using `rustfmt` before commit, and this should be
set up as either a precommit hook, or using functionality for automatic
formatting in your editor. To quickly format code, it can be run via `cargo`
using the `cargo fmt` command.

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

## Package Structure and Naming
Enso follows the standard rust convention for structuring crates, as provided
by `cargo new`. This is discussed more in depth [here](https://learning-rust.github.io/docs/a4.cargo,crates_and_basic_project_structure.html#Project-Structure).

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
