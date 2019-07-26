# Scala Style Guide
Like many style guides, this Scala style guide exists for two primary reasons.
The first is to provide guidelines that result in a consistent code style across
all of the Enso codebases, while the second is to guide people towards a style
that is expressive while still easy to read and understand.

In general, it aims to create a set of 'zero-thought' rules in order to ease the
programmer burden; there is usually only _one way_ to lay out code correctly.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Code Formatting](#code-formatting)
  - [Naming](#naming)
- [Build Tooling](#build-tooling)
- [Commenting](#commenting)
  - [Documentation Comments](#documentation-comments)
  - [Source Notes](#source-notes)
  - [TODO Comments](#todo-comments)
  - [Other Comment Usage](#other-comment-usage)
- [Program Design](#program-design)
  - [Safety](#safety)
  - [Testing and Benchmarking](#testing-and-benchmarking)
  - [Warnings, and Lints](#warnings-and-lints)

<!-- /MarkdownTOC -->

## Code Formatting
This section explains the rules for visually laying out your code. They provide
a robust set of guidelines for creating a consistent visual to the code.

Primary formatting is dealt with through use of the Scala formatting tool
[`scalafmt`](https://scalameta.org/scalafmt/), which enforces rules around
whitespace, line-wrapping, and alignment. The Enso repository contains the main
[`.scalafmt.conf`](../.scalafmt.conf) configuration file, and this is what
should be used for all new Scala projects.

All files must be formatted using `scalafmt` before commit, and this should be
set up as either a precommit hook, or using the integration in IntelliJ. If you
use the IntelliJ integration, please note that you need only have the official
[Scala Plugin](https://www.jetbrains.com/help/idea/discover-intellij-idea-for-scala.html)
installed, and be using IntelliJ 2019.1 or later. You should _not_ use the
independent Scalafmt plugin.

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

## Build Tooling
All Scala projects in the Enso organisation should manage their dependencies and
build setup using [SBT](hhttps://www.scala-sbt.org/1.x/docs/index.html).

If you are using IntelliJ, please ensure that you select to use the SBT shell
for both imports and builds.

## Commenting
Comments in code are a tricky area to get right as we have found that comments
often expire quickly, and in absence of a way to validate them, remain incorrect
for long periods of time. In order to best deal with this problem, we make the
keeping of comments up-to-date into an integral part of our programming practice
while also limiting the types and kinds of comments we allow.

Comments across the Enso codebases fall into three main types:

- **Documentation Comments:** API documentation for all language constructs that
  can have it (classes, objects, functions, and so on).
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
use the standard [scaladoc](https://docs.scala-lang.org/style/scaladoc.html)
syntax. Doc comments should:

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
   is relevant. It takes the following form: `// Note [Note Name]`, where
   `Note Name` is a unique identifier across the codebase. These names should be
   descriptive, and make sure you search for it before using it, in case it is
   already in use.
2. **Source Note:** This is the comment itself, which is a large block comment
   placed after the first function in which it is referred to in the module. It
   uses the scala block-comment syntax `/* ... */`, and the first line names
   the note using the same referrer as above: `/* Note [Note Name]`. The name(s)
   in the note are underlined using a string of the `~` (tilde) character.

A source note may contain sections within it where necessary. These are titled
using the following syntax: `== Note [Note Name (Section Name)]`, and can be
referred to from a referrer much as the main source note can be.

Sometimes it is necessary to reference a source note in another module, but this
should never be done in-line. Instead, a piece of code should reference a source
note in the same module that references the other note while providing
additional context to that reference.

An example, based on some code in the GHC codebase, can be seen below:

```scala
{
def prepRHS (env : SimplEnv, outExpr : OutExpr) : SimplM[SimplEnv, OutExpr] = {
  val (ty1, _ty2) = coercionKind(env) // Note [Float Coercions]

  if (!isUnliftedType(ty1)) {
    val newTy1 = convertTy(ty1) // Note [Float Coercions (Unlifted)]
    ...more expressions defining prepRHS...
  }
}

/* Note [Float Coercions]
 * ~~~~~~~~~~~~~~~~~~~~~~
 * When we find the binding
 *         x = cast(e, co)
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
  `[ARA, WD]`, in square brackets.
- It is then followed by an explanation of what needs to be done.

For example:

```scala
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

### Safety
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

- We use Caliper for our benchmarks.
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

Sometimes it is impossible to fix a warning (often in situations involving the
use of macros). In such cases, you are allowed to suppress the warning locally,
but this must be accompanied by a source note explaining why you are doing so.
