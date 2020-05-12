# Enso: The Syntax
When working with a programming language, the syntax is the first thing that a
user encounters. This makes it _utterly integral_ to how users experience the
language, and, in the case of Enso, the tool as a whole.

Enso is a truly novel programming language in that it doesn't have _one_ syntax,
but instead has two. These syntaxes are dual: visual and textual. Both are
first-class, and are truly equivalent ways to represent and manipulate the
program. To that end, the design of the language's syntax requires careful
consideration, and this document attempts to explain both the _what_, of Enso's
syntax, but also the _why_.

This document exists to expand on some of the design philosophy behind the
language's syntactic [specification](../specification/syntax.md).

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Naming](#naming)
    - [Identifiers](#identifiers)
    - [Operators](#operators)
    - [This vs. Self](#this-vs-self)

<!-- /MarkdownTOC -->

## Naming
This section discusses decisions made around the naming of various language
constructs.

### Identifiers
While, through much of the language's history, we have used `camelCase` (with
its disambiguating cousin `CamelCase`), this has been decided against for one
primary reason:

- Names using snake case are far easier to read, and optimising code for
  readability is _overwhelmingly_ important in a context where novice users are
  involved.

### Operators
While some languages allow use of unicode characters for naming operators, we
will not. The reasoning behind this is simple, and is best explained by
paraphrasing the [Idris wiki](https://github.com/idris-lang/Idris-dev/wiki/Unofficial-FAQ#will-there-be-support-for-unicode-characters-for-operators).

- Unicode operators are hard to type, making it far more difficult to use other
  peoples' code. Even if some editors provide input methods for such symbols,
  they do not provide a good UX.
- Not every piece of software has good support for Unicode. Even though this is
  changing, it is not there yet, thus raising barriers to entry.
- Many Unicode characters are hard to distinguish.

In essence, while the use of Unicode operators can make code look pretty, a font
with well-defined ligatures can do the same.

### This vs. Self
Though it varies greatly between programming languages, we have chosen `this` to
be the name of the 'current type' rather than `self`. This is a purely aesthetic
decision, and the final clincher was the ability to write `this` and `that`, as
opposed to `self` and `that`.
