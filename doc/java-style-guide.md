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
- [Commenting](#commenting)
  - [Documentation Comments](#documentation-comments)
  - [Source Notes](#source-notes)
  - [TODO Comments](#todo-comments)
  - [Other Comment Usage](#other-comment-usage)
- [Program Design](#program-design)
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
the doc comment. Every language construct that can have an associated doc
comment should do so. These are intended to be consumed by users of the API,
whether internal or external, and use the standard Javadoc syntax. Doc comments
should:

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

An example, based on some code in the GHC codebase, can be seen below:

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
        x = cast(e, co)
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
