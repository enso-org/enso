---
layout: style-guide
title: TypeScript Style Guide
category: style-guide
tags: [style-guide]
order: 3
---

# TypeScript Style Guide

This TypeScript style guide exists for two primary reasons. The first is to
provide guidelines that result in a consistent code style across all of the Enso
codebases, while the second is to lead people towards a style that is expressive
while still easy to read and understand.

In general, it aims to create a set of 'zero-thought' rules in order to ease the
programmer burden; there is usually only _one way_ to lay out code correctly.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [TypeScript Style Guide](#typescript-style-guide)
  - [Code Formatting](#code-formatting)
    - [Line Width](#line-width)
    - [Imports](#imports)
    - [Sections](#sections)
      - [An Example of Using Sections](#an-example-of-using-sections)
    - [Vertical Spacing](#vertical-spacing)
    - [Multi-Line Expressions](#multi-line-expressions)
      - [Multi-Line Expression Examples](#multi-line-expression-examples)
    - [Spacing](#spacing)
      - [Spacing Examples as Function Definitions](#spacing-examples-as-function-definitions)
  - [Naming](#naming)
  - [Package Structure and Naming](#package-structure-and-naming)
    - [Using Access Modifiers](#using-access-modifiers)
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
      - [Warnings](#warnings)

<!-- /MarkdownTOC -->

## Code Formatting

Section explains the rules for visually laying out your code. The rules provide
a robust set of guidelines for creating a consistent visual to the code.

Code style is _far_ more than just the visual formatting of the code. Especially
as it can often be automated. According to the documentation of rustfmt,
"formatting code is a mostly mechanical task which takes both time and mental
effort." ([source]) While, in many cases, the programmer can be relieved of this
burden through use of an automated formatter. It is sometimes the case that such
a tool imposes _more_ cognitive load in programmers. With prettier, programmers
tend to have to refactor long lines to use variables and move code to specific
modules or sections. Otherwise, prettier produces code that is hard to read and
write. Thus, it is very important to write code in such a way that we can be
proud of its quality.

[source]:
  https://github.com/rust-lang/style-team/blob/02f3c00c06c6609e3e0add3f2f15f448e12b709a/guide/guide.md#motivation---why-use-a-formatting-tool

### Line Width

Each line in the source file must be of a maximum of 100 characters of text
(including comments).

The exception is Tailwind class lists in `.tsx` files, which must be on a single
line. If present, they must be placed after all other attributes.

### Imports

The imports section at the top of a file should be separated into four groups.
All imports must use namespace imports (`import * as name from 'module'`) or
default imports (`import name from 'module'`). The `.js` extension must be left
out from relative imports. These groups should be sorted in alphabetical order
and are divided as follows:

```ts
// Group 1: Node imports (with `node:` prefix).
// Group 2: React imports.
// Group 3: external imports.
// Group 4: imports from other local packages.
// Group 5: relative imports.
```

Please look at the following by way of example:

```ts
import * as fs from "node:fs/promises";

import * as react from "react";

import * as authentication from "enso-authentication";

import * as config from "./config";
```

### Sections

TypeScript source files should be divided into sections, with a header placed
before the definition of each new concept in a file.

By the term "concept," we are referring primarily to a structure with a set of
related implementations. However, if the related implementations rely on some
simple helper functions, these may also be defined in the same section. A
section should have a header as follows.

```ts
// ===================
// === SectionName ===
// ===================
```

Additionally, the code in each section should further be divided into
sub-sections that group relevant functionality within the section. The header
for a sub-section is as follows.

```ts
// === SubSectionName ===
```

At least one section should be defined in every file.

#### An Example of Using Sections

Here is a large-scale example of how sections should be used in source files.

```ts
// =================
// === AxisOrder ===
// =================

/** Defines the order in which particular axis coordinates are processed. Used
 * for example to define the rotation order in `DisplayObject`. */
export enum AxisOrder {
  XYZ,
  XZY,
  YXZ,
  YZX,
  ZXY,
  ZYX,
}

// =================
// === Transform ===
// =================

/** Defines the order in which transformations (scale, rotate, translate) are
 * applied to a particular object. */
export enum TransformOrder {
  ScaleRotateTranslate,
  ScaleTranslateRotate,
  RotateScaleTranslate,
  RotateTranslateScale,
  TranslateRotateScale,
  TranslateScaleRotate,
}

// =============================
// === HierarchicalTransform ===
// =============================

export class HierarchicalTransform<OnChange> {
  transform: Transform;
  transformMatrix: Matrix4<f32>;
  origin: Matrix4<f32>;
  matrix: Matrix4<f32>;
  dirty: dirty.SharedBool<OnChange>;
  logger: Logger;

  constructor(logger: Logger, onChange: OnChange) {
    this.transform = Transform.default();
    this.transformMatrix = Matrix4.identity();
    this.origin = Matrix4.identity();
    this.matrix = Matrix4.identity();
    const dirtyLogger = logger.sub("dirty");
    this.dirty = new dirty.SharedBool(dirtyLogger, onChange);
    this.logger = logger;
  }
}
```

### Vertical Spacing

There should be 1 empty line before and after every function, class, and
section, and 1 empty line at the end of the file.

However, this should be taken care of automatically if you have the code
formatter `prettier` set to run on every save, or added as a pre-commit hook.

### Multi-Line Expressions

In an ideal world, all expressions in the code should be a single line. This is
because multi-line expressions are usually hard to read, and because they can
introduce lots of noise in the code. In the vast majority of cases, the presence
of a multi-line expression indicates that the code needs refactoring.

Please try to refactor portions of multi-line expressions to well-named
variables, and divide them up to a set of single-line expressions.

#### Multi-Line Expression Examples

The following is an example of poorly formatted code:

```ts
constructor() {
    this.shapeDirty = new ShapeDirty(logger.sub('shape_dirty'),
        { ...onDirty })
    this.dirtyFlag = new MeshRegistryDirty(logger.sub('mesh_registry_dirty'),
        onDirty)
}
```

The following is an example of the same code properly formatted:

```ts
constructor() {
    const shapeDirtyLogger = logger.sub('shape_dirty')
    this.shapeDirty = new ShapeDirty(shapeDirtyLogger, { ...onDirty })
    const dirtyFlagLogger = logger.sub('mesh_registry_dirty')
    this.dirtyFlag = MeshRegistryDirty::new(dirtyFlagLogger, onOirty)
}
```

### Spacing

The following spacing rules are _also_ employed in order to create a visual flow
to our code to aid readability:

- The type operator is spaced: `function test(foo: string, bar: number) { ... }`
- Commas between complex expressions (including the argument list) are spaced
- Commas between simple elements are spaced: `Record<string, number>`
- Arguments to functions are spaced: `build(builder, "out", qual)`
- Operators are always spaced: `let foo = a + b * c;`

#### Spacing Examples as Function Definitions

The following function definitions are all good examples of correct use of
spacing.

```ts
function new<Dom extends string>(dom: Dom, logger: Logger): MyType | Error {
    ...
}
```

```ts
function new<Dom extends string>(dom: Dom, logger: Logger, onDirty: OnDirty): MyType | Error {
    ...
}
```

```ts
function new<Dom extends string>(
    dom: Dom, logger: Logger, onDirty: OnDirty, onRemove: OnRemove
): MyType | Error {
    ...
}
```

```ts
function new<Dom extends string>(
    dom: Dom,
    logger: Logger,
    onDirty: OnDirty,
    onRemove: OnRemove,
    onReplace: OnReplace
): MyType | Error {
    ...
}
```

Long generic constraints are formatted like this:

```ts
function new<
    D extends AsRef<string>, L extends IsLogger
>(dom: D, logger: L): MyType | Error {
    ...
}
```

Or, in case they are really long, like this:

```ts
function new<
    D extends AsRef<string>,
    L extends IsLogger,
    ...
>(dom: D, logger: L): MyType | Error {
    ...
}
```

## Naming

Enso has some fairly simple general naming conventions, though the sections
below may provide more rules for use in specific cases.

- Types are written using `UpperCamelCase`.
- Variables and function names are written using `camelCase`.
- If a name contains an initialism or acronym, all parts of that initialism
  should be lower-case: `makeHttpRequest`, not `makeHTTPRequest`.
- Short variable names such as `a` and `b` should only be used in the following
  contexts:
  - Where there is no other appropriate name.
  - Named lifetimes. They should _never_ be used to refer to temporary data in a
    function, as all temporaries should be given descriptive names.
- Names should be descriptive, even if this makes them longer.
- Any function that performs an unsafe operation that is not documented in its
  type (e.g. `head<T>(ts: T[]): T`, which fails if the list is empty), must be
  named using the word 'unsafe' (e.g. `unsafeHead`). For more information on
  unsafe function usage, see the section on [safety](#safety).
- Naming should use American English spelling.

## Package Structure and Naming

Enso follows Rust convention for structuring packages, as provided by
`cargo new`. This is discussed more in depth
[here](https://learning-rust.github.io/docs/a4.cargo,crates_and_basic_project_structure.html#Project-Structure).

### Using Access Modifiers

Given that access modifiers disappear upon compilation, making things `public`
(the default modifier, so it can be omitted) has no impact on the performance of
the compiled code. As a result, the _only_ circumstance under which things are
allowed to not be `public` is when doing so would allow consumers of an API to
break internal guarantees provided by that API (e.g. building an immutable
collection on top of a mutable buffer).

## Build Tooling

All TypeScript projects are managed using [npm](https://docs.npmjs.com/) and
built using [esbuild](https://esbuild.github.io/).

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
are written using the standard
[JSDoc](https://www.typescriptlang.org/docs/handbook/jsdoc-supported-types.html#documentation)
syntax. Doc comments should contain:

1. **Summary:** A one-line summary of the construct's behaviour or purpose.
2. **Description (Optional):** Any useful information that would be necessary
   for a consumer of the API to know (that is not encoded in the types). This
   should be written in grammatically correct English.

We are following the Rust convention to not document function parameters and
return types.

An example of a valid set of comments for some TypeScript code is as follows:

```ts
/** A representation of tree structures containing elements of type `T`. */
class Tree<T> {
  /** Provides a sequence representation of the tree.
   *
   * The function provides configurable behaviour for the order in which the
   * tree is walked. See {@link WalkStrategy} for
   * the provided options. */
  walkToSequence(order: WalkStrategy<T>): T[] {
    // ...
  }

  getBuffer(): T[] {
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

```ts
/** A representation of tree structures containing elements of type `T`. */
class Tree<T> {
  /** Provides a sequence representation of the tree.
   *
   * The function provides configurable behaviour for the order in which the
   * tree is walked. See {@link WalkStrategy} for
   * the provided options. */
  walkToSequence(order: WalkStrategy<T>): T[] {
    const outputArray = new Uint8Array(this.getBuffer().len()); // Note [Buffer Size]
    // ...
  }

  // Note [Buffer Size]
  // ==================
  // When working with the buffer for the tree walk, it is important that you
  // ensure....

  getBuffer(): T[] {
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

```ts
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

### Testing and Benchmarking

TBD - most TypeScript code consists of UI code, which is not yet set up for
testing.

### Warnings, and Lints

In general, we aim for a codebase that is free of warnings and lints, and we do
this using the following ideas:

#### Warnings

New code should introduce no new warnings onto main. You may build with warnings
on your own branch, but the code that is submitted as part of a PR should not
introduce new warnings. You should also endeavour to fix any warnings that you
come across during development.

Sometimes it is impossible to fix a warning (often in situations involving the
use of macros). In such cases, you are allowed to suppress the warning locally,
but this must be accompanied by a source note explaining why you are doing so.
