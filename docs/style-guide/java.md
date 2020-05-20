---
layout: style-guide
title: Java Style Guide
category: style-guide
tags: [style-guide]
order: 2
---

# Java Style Guide
Like many style guides, this Java style guide exists for two primary reasons.
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
- [Commenting](#commenting)
  - [Documentation Comments](#documentation-comments)
  - [Source Notes](#source-notes)
  - [TODO Comments](#todo-comments)
  - [Other Comment Usage](#other-comment-usage)
- [Program Design](#program-design)
  - [Code Complexity](#code-complexity)
  - [Testing and Benchmarking](#testing-and-benchmarking)
  - [Warnings, and Lints](#warnings-and-lints)

<!-- /MarkdownTOC -->

## Code Formatting
This section explains the rules for visually laying out your code. They provide
a robust set of guidelines for creating a consistent visual to the code.

Primary code formatting is done using the [Google Java Format](https://github.com/google/google-java-format)
tool, which enforces a clear and consistent style. This is a zero configuration
tool, and hence there is no project-level configuration for this tool. It should
be used for all new Java projects.

All files must be formatted using this tool before being committed, and this
should be set up as either a precommit hook, or using an integration in your
IDE.

### Naming
Enso has some fairly simple general naming conventions, though the sections
below may provide more rules for use in specific cases.

- Types are written using `UpperCamelCase`.
- Variables and function names are written using `camelCase`.
- If a name contains an initialism or acronym, all parts of that initialism
  should be of the same case: `httpRequest` or `makeHTTPRequest`.
- Short variable names such as `a` and `b` should only be used in contexts where
  there is no other appropriate name, and should _never_ be used to refer to
  temporary data in a function.
- Names should be descriptive, even if this makes them longer.
- Naming should use American English spelling.

## Package Structure and Naming
Enso follows the
[Java convention for naming packages](https://docs.oracle.com/javase/tutorial/java/package/namingpkgs.html):
package name components may contain only lower case characters and, if
necessary, an underscore character. All Enso package names should be prefixed
with `org.enso`. For example, the package for implementation of `File Manager`
project should be named `org.enso.filemanager`.

When the name of the file in the package is the same as the final component of
the package name, the file should be moved one level up. For examples, if
`File Manager` project contains `FileManager.java` file, then the file should
be placed directly in the `org.enso` package instead of `org.enso.filemanager`.
This is to avoid repetitious constructs like `org.enso.filemanager.FileManager`.

### The Public API
In order to produce as flexible a codebase as possible, we tend not to make use
of access modifiers in our code (`protected`, `private`, and so on). Instead, we
use the concept of `Internal` modules to separate public from private.

If you are writing code in a package `X.Y.MyType` and would like to signal that
a particular construct (e.g. a function) is for internal use in that package,
you should create a `X.Y.MyType.Internal` package. You can then write the
relevant language construct in that package instead of the source package.

#### Using Access Modifiers
There are, however, a few notable exceptions to the above:

- **Safety:** Privacy modifiers (e.g. `private` and `protected`) should be
  used to enforce an API contract around safety.
- **Reducing Overhead:** As the `Internal` module is a separate module, there
  can (under some circumstances) be some overhead for its use. If you are
  writing code on a performance-critical path, you may instead make use of
  access modifiers.
- **Enabling Optimisations:** The JVM is capable of performing optimisations by
  making use of visibility information (e.g. in the interpreter). If you are
  writing performance-critical code, you may use access modifiers to provide the
  JVM with additional information.

## Commenting
Comments in code are a tricky area to get right as we have found that comments
often expire quickly, and in absence of a way to validate them, remain incorrect
for long periods of time. In order to best deal with this problem, we make the
keeping of comments up-to-date into an integral part of our programming practice
while also limiting the types and kinds of comments we allow.

Comments across the Enso codebases fall into three main types:

- **Documentation Comments:** API documentation for all language constructs that
  can have it (classes, methods, and so on).
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

1. **Top-Level Type Definitions:** All type definitions must be accompanied by a
   doc comment. This includes nested classes.
2. **Functions:** Function documentation should provide at-a-glance intuition
   for how to use that function.

Documentation comments are intended for consumption by the users of the API, and
are written using the standard JavaDoc syntax. Doc comments should contain:

1. **Summary:** A one-line summary of the construct's behaviour. This should be
   a valid sentence with 'this X' (where X = class, method, etc) prepended to
   it.
2. **Description (Optional):** Any useful information that would be necessary
   for a consumer of the API to know (that is not encoded in the types). This
   should be written in grammatically correct English.
3. **Parameters and Returns:** The return value must always be described. If the
   parameters are _all_ obvious from their names, you must omit the `@param`
   annotations. If one or more parameters require explanation (for things not
   expressed in their name or type), then all parameters must be annotated.

An example of a valid set of doc comments is provided below:

```java
/**
 * Contains an implementation of a span tree.
 *
 * This tree implementation provides O(log(n)) average and worst-case complexity
 * for insertion, lookup and
 *
 * @param <T> the type of the tree's elements
 */
public class SpanTree<T> implements Tree<T> {

  /** Constructs an empty tree. */
  public Tree() {}

  /**
   * Inserts the provided element into the span tree.
   *
   * The complexity of insertion is O(log(n)) in both the average and worst
   * cases.
   *
   * @param element the element to be inserted into the tree
   */
  public void insert(T element) {
    // ...
  }

  private class Node<T> {
    // ...
  }
}
```

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
   placed after the first function in which it is referred to in the module. It
   uses the java block-comment syntax `/* ... */`, and the first line names
   the note using the same referrer as above: `/* Note [Note Name]`. The name(s)
   in the note are underlined using a string of the `~` (tilde) character.

A source note may contain sections within it where necessary. These are titled
using the following syntax: `== Note [Note Name (Section Name)]`, and can be
referred to from a referrer much as the main source note can be.

Sometimes it is necessary to reference a source note in another module, but this
should never be done in-line. Instead, a piece of code should reference a source
note in the same module that references the other note while providing
additional context to that reference.

```java
{
public SimplM<SimplEnv, OutExpr> prepRHS(SimplEnv env, OutExpr outExpr) {
  var ty1 = coercionKind(env); // Note [Float Coercions]

  if (!isUnliftedType(ty1)) {
    var newTy1 = convertTy(ty1) // Note [Float Coercions (Unlifted)]

    ...more code defining prepRHS...
  }
}

/* Note [Float Coercions]
 * ~~~~~~~~~~~~~~~~~~~~~~
 * When we find the binding
 *     x = cast(e, co)
 * we'd like to transform it to
 *         x' = e
 *         x = cast(x, co) // A trivial binding
 * There's a chance that e will be a constructor application or function, or
 * something like that, so moving the coercion to the usage site may well cancel
 * the coercions and lead to further optimisation.
 *         ...more stuff about coercion floating...
 *
 * Note [Float Coercions (Unlifted)]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *      ...explanations of floating for unlifted types...
 */
}
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
  `[ARA, MK]`, in square brackets.
- It is then followed by an explanation of what needs to be done.

For example:

```java
{
// TODO [ARA] This is a bit of a kludge. Instead of X it should to Y, accounting
// for the fact that Z.
}
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

### Testing and Benchmarking
New code should always be accompanied by tests. These can be unit, integration,
or some combination of the two, and they should always aim to test the new code
in a rigorous fashion.

- We tend to use ScalaTest, but also make use of ScalaCheck for property-based
  testing.
- Tests should be declared in the project configuration so they can be trivially
  run.
- A test file should be named after the module it tests.

Any performance-critical code should also be accompanied by a set of benchmarks.
These are intended to allow us to catch performance regressions as the code
evolves, but also ensure that we have some idea of the code's performance in
general.

- We use JMH for our benchmarks.
- We measure time, but also memory usage and CPU time where possible.
- Where relevant, benchmarks may set thresholds which, when surpassed, cause the
  benchmark to fail. These thresholds should be set for a release build, and not
  for a development build.

_Do not benchmark a development build_ as the data you get will often be
entirely useless.

### Warnings, and Lints
In general, we aim for a codebase that is free of warnings and lints, and we do
this using the following ideas.

#### Warnings
New code should introduce no new warnings onto master. You may build with
warnings on your own branch, but the code that is submitted as part of a PR
should not introduce new warnings. You should also endeavour to fix any warnings
that you come across during development.

Sometimes it is impossible to fix a warning (often in situations involving the
use of macros or code-generation). In such cases, you are allowed to suppress
the warning locally, but this must be accompanied by a source note explaining
why you are doing so.
