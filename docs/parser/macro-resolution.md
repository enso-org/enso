---
layout: developer-doc
title: Macro Resolution
category: parser
tags: [parser, macro, resolution]
order: 4
---

# Macro Resolution

The Enso macro system is responsible for translating the structured token stream
from the [lexer](./lexer.md) and resolving it into the output [ast](./ast.md)
that represents the Enso language.

The macro system is of _key_ importance to the operation of Enso, providing the
IDE with detailed information on how users can interact with each and every
portion of the syntax. This has myriad benefits over a traditional parser,
allowing the IDE to handle all portions of the language's syntax in a seamless
way; the [macro matchers](#macro-matchers) describe the Enso syntax in a way
that allows the IDE to extract information about extension points and other
crucial metadata.

Nevertheless, the macro system _also_ serves as the language's parser,
implementing a sophisticated resolution algorithm that is able to turn the token
stream into the language AST.

Furthermore, the macro system can be extended in future to allow _users_ to
define their own hygienic macros in Enso itself. This will allow them to
manipulate their programs syntactically using _exactly_ the same mechanism as
used in the Enso parser itself.

<!-- MarkdownTOC levels="2,3" autolink="true" indent="  " -->

- [Resolving Macros](#resolving-macros)
  - [Macro Segments](#macro-segments)
  - [Matching](#matching)
- [Macro Matchers](#macro-matchers)
  - [Relative Precedence Matchers](#relative-precedence-matchers)
  - [Named Macros](#named-macros)
- [Macro Resolution Errors](#macro-resolution-errors)
  - [Macro Errors as Parser Errors](#macro-errors-as-parser-errors)
- [User-Defined Macros](#user-defined-macros)

<!-- /MarkdownTOC -->

## Resolving Macros

Conceptually, the macro resolution process builds a tree out of the user-defined
macros, segmenting the token-space based on the types of tokens expected by the
various definitions. Resolution then proceeds to walk the tree, pruning the
possible cases until either:

1. An unambiguous resolution is found.
2. An ambiguous resolution is found and an error emitted.

Resolution then recurses, continuing to apply the macro resolution process
against the non-matchable segments in the macro.

Consider, for example, the macros defined as follows:

```rust
let if_then      = "if" >> Matcher::Expr >> "then" >> Matcher::Expr
let if_then_else = "if" >> Matcher::Expr >> "then" >> Matcher::Expr >> "else" >> Matcher::Expr
```

These generate a tree as follows:

```
                       ┌──────┐
                       │ "if" │
                       └──────┘
                           │
                           ├─────────────────┐
                           ▼                 ▼
                   ┌───────────────┐     ┌───────┐
                   │ Matcher::Expr │     │ Other │
                   └───────────────┘     └───────┘
                           │                 │
                           │                 │
                           ▼                 ▼
                       ┌──────┐          ┌──────┐
                       │"then"│          │ Fail │
                       └──────┘          └──────┘
                           │
                           ├─────────────────┐
                           ▼                 ▼
                   ┌───────────────┐     ┌───────┐
                   │ Matcher::Expr │     │ Other │
                   └───────────────┘     └───────┘
                           │                 │
       ┌───────────────────┤                 │
       ▼                   ▼                 ▼
  ┌─────────┐          ┌──────┐          ┌──────┐
  │ Succeed │          │"else"│          │ Fail │
  └─────────┘          └──────┘          └──────┘
                           │
                           ├─────────────────┐
                           ▼                 ▼
                   ┌───────────────┐     ┌───────┐
                   │ Matcher::Expr │     │ Other │
                   └───────────────┘     └───────┘
                           │                 │
                           │                 │
                           ▼                 ▼
                      ┌─────────┐        ┌──────┐
                      │ Succeed │        │ Fail │
                      └─────────┘        └──────┘
```

Within each of the patterns, the macro resolution continues recursively, thus
allowing the macros to match nested uses of themselves (e.g.
`if a then if b then c`).

In reality, this is not how it _actually_ takes place. Applying one macro to
completion and then starting again on all remaining segments provides abysmal
complexity. As a result, the process instead operates on a _stack_ of resolution
contexts, performing its resolution in a linear scan over the token stream.

### Macro Segments

Macros are described in terms of _segments_. A segment consists of three main
components:

1.  An optional _preceding section_ that uses [matchers](#macro-matchers) to
    describe what they match until encountering the _literal_ in 2. This can
    only occur with the first segment.
2.  A _literal_ that must be matched for that segment to apply.
3.  An optional associated [matcher](#macro-matchers) that consumes certain
    _types_ of token.

Segments determine how a given macro matches on the input token stream, and are
used to generate the tree for performing resolution.

### Matching

When a match occurs, the resultant segments are passed to a function that can
perform some action based on those segments. This function is responsible for
manipulating the output of the macro resolver, and hence manipulates the AST.

As the function may assert _additional_ properties on the match not able to be
ascertained by the segment definitions themselves,

## Macro Matchers

The matchers describe the _kinds_ of tokens that can be reasoned about within a
macro definition. These fall across a set of categories as follows.

The boundary matchers are:

- `Start`: Matches the start of the line.
- `End`: Matches the end of the line.

Structural matchers:

- `Nothing`: Never matches on any token.
- `Any`: Matches on any token. This may become a family of tokens (e.g.
  `AnyExpr`), and so on, for performance.
- `Seq`: Matches on the contained matches in sequence.
- `Or`: Matches on one of the contained matches.
- `Many`: Matches on multiple of the given match.
- `Except`: Matches on everything but the given pattern.

Meta matchers:

- `Build`: Performs AST resolution on the match, but this must become implicit.
- `Err`: Allows ascribing a user-defined error to the failure of the contained
  matcher.
- `Tag`: Does nothing, but attaches string metadata to the result. This is very
  useful for syntax highlighting.
- `Exact`: Matches on an explicit AST.

Identifier matchers:

- `Referent`: Matches on a referent identifier.
- `Variable`: Matches on a variable identifier.
- `External`: Matches on an external identifier.
- `Blank`: Matches the blank identifier (`_`).
- `Operator`: Matches on an operator identifier.
- `Modifier`: Matches on a modifier identifier.
- `Annotation`: Matches on an annotation identifier.

Literal matchers:

- `Number`: Matches on a literal number.
- `Text`: Any text literal.
- `TextLine`: Matches a single-line textual literal.
- `TextInlineBlock`: Matches an inline-block textual literal.
- `TextBlock`: Matches a block text literal.

Comment matchers:

- `DisableComment`: Matches a standard disable comment.
- `DocComment`: Matches an Enso doc comment.

Structural matchers:

- `Block`: Matches an Enso block.

Error matchers:

- `DanglingBase`: Matches an erroneous base for a number literal.
- `TextError`: Matches tokens representing error states during textual lexing.
- `InvalidSuffix`: Matches an erroneous suffix token.
- `Unrecognized`: Matches an unrecognised token.

In addition, it is highly likely that some of these will be combined into
aggregate matchers that match one of many kinds of token.

### Relative Precedence Matchers

Where relevant, the matchers above may take an optional "maximum precedence" of
operator that they may match. This effectively serves to implement differing
precedences on either side of a literal operator being implemented via a macro,
and is important for correctly resolving the function arrow `->`.

### Named Macros

In addition to the basic matcher described above, we also need a `Named`,
matcher. The intent behind this matcher is that it allows you to refer to
existing macro definitions by name, and also provides for better error messages.

By convention, we aim for these names to be treated as qualified, based on the
package in which they're declared. This convention should be followed for the
built-in macros as well, even if they can't be overridden, as they may appear in
error messages.

## Macro Resolution Errors

Due to the incredibly structured nature of the resolver it is quite possible for
the macro system to produce very detailed errors when resolution fails. This is
because it knows the path it took to get to the failure, and the expected tokens
or kinds of tokens at the failure site.

In addition, the resolver allows users to define their own errors through the
use of the `Err` matcher mentioned [above](#macro-matchers).

### Macro Errors as Parser Errors

The wealth of information in a failed macro resolution brings the necessary
tools for the compiler to produce informative and detailed parsing errors.

> The actionables for this section are:
>
> - Determine how we ensure provision of failure information to the engine in a
>   useful format.

## User-Defined Macros

In the future we want to provide an Enso-level syntax that allows the user to
define syntactic macros for their programs, similar to how Rust exposes the
`macro_rules!` construct.

> The actionables for this section are:
>
> - Determine what this should look like in surface Enso.
> - Determine exactly how the round-trip mechanic between the runtime and parser
>   should function.
