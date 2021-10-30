---
layout: developer-doc
title: Flexer
category: syntax
tags: [parser, flexer, lexer, dfa]
order: 1
---

# Flexer

The flexer is a finite-automata-based engine for the definition and generation
of lexers. Akin to `flex`, and other lexer generators, the user may use it to
define a series of rules for lexing their language, which are then used by the
flexer to generate a highly-efficient lexer implementation.

Where the flexer differs from other programs in this space, however, is the
power that it gives users. When matching a rule, the flexer allows its users to
execute _arbitrary_ Rust code, which may even manipulate the lexer's state and
position. This means that the languages that can be lexed by the flexer extend
from the simplest regular grammars right up to unrestricted grammars (but please
don't write a programming language whose syntax falls into this category). It
also differs in that it chooses the first complete match for a rule, rather than
the longest one, which makes lexers much easier to define and maintain.

For detailed library documentation, please see the
[crate documentation](../../lib/rust/flexer/src/lib.rs) itself. This includes a
comprehensive tutorial on how to define a lexer using the flexer.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [The Lexing Process](#the-lexing-process)
- [Lexing Rules](#lexing-rules)
  - [Groups](#groups)
  - [Patterns](#patterns)
  - [Transition Functions](#transition-functions)
- [Code Generation](#code-generation)
  - [Automated Code Generation](#automated-code-generation)
- [Structuring the Flexer Code](#structuring-the-flexer-code)
  - [Supporting Code Generation](#supporting-code-generation)

<!-- /MarkdownTOC -->

## The Lexing Process

In the flexer, the lexing process proceeds from the top to the bottom of the
user-defined rules, and selects the first expression that _matches fully_. Once
a pattern has been matched against the input, the associated code is executed
and the process starts again until the input stream has been consumed.

This point about _matching fully_ is particularly important to keep in mind, as
it differs from other lexer generators that tend to prefer the _longest_ match
instead.

## Lexing Rules

A lexing rule for the flexer is a combination of three things:

1.  A group.
2.  A pattern.
3.  A transition function.

An example of defining a rule is as follows:

```rust
fn define() -> Self {
    let mut lexer     = TestLexer::new();
    let a_word        = Pattern::char('a').many1();
    let root_group_id = lexer.initial_state;
    let root_group    = lexer.groups_mut().group_mut(root_group_id);
    // Here is the rule definition.
    root_group.create_rule(&a_word,"self.on_first_word(reader)");
    lexer
}
```

### Groups

A group is a mechanism that the flexer provides to allow grouping of rules
together. The flexer has a concept of a "state stack", which records the
currently active state at the current time, that can be manipulated by the
user-defined [transition functions](#transition-functions).

A state can be made active by using `flexer::push_state(state)`, and can be
deactivated by using `flexer::pop_state(state)` or
`flexer::pop_states_until(state)`. In addition, states may also have _parents_,
from which they can inherit rules. This is fantastic for removing the need to
repeat yourself when defining the lexer.

When inheriting rules from a parent group, the rules from the parent group are
matched strictly _after_ the rules from the child group. This means that groups
are able to selectively "override" the rules of their parents. Rules are still
matched in order for each group's set of rules.

### Patterns

Rules are defined to match _patterns_. Patterns are regular-grammar-like
descriptions of the textual content (as characters) that should be matched. For
a description of the various patterns provided by the flexer, see
[pattern.rs](../../lib/rust/flexer/src/automata/pattern.rs).

When a pattern is matched, the associated
[transition function](#transition-functions) is executed.

### Transition Functions

The transition function is a piece of arbitrary rust code that is executed when
the pattern for a given rule is matched by the flexer. This code may perform
arbitrary manipulations of the lexer state, and is where the majority of the
power of the flexer stems from.

## Code Generation

While it would be possible to interpret the flexer definition directly at
runtime, this would involve far too much dynamicism and non-cache-local lookup
to be at all fast.

Instead, the flexer includes
[`generate.rs`](../../lib/rust/flexer/src/generate.rs), a library for generating
highly-specialized lexer implementations based on the definition provided by the
user. The transformation that it implements operates as follows for each group
of rules.

1.  The set of rules in a group is used to generate a
    [Nondeterministic Finite Automaton](https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton),
    (NFA).
2.  The NFA is ttransformed into a
    [Deterministic Finite Automaton](https://en.wikipedia.org/wiki/Deterministic_finite_automaton)
    (DFA), using a variant of the standard
    [powerset construction](https://en.wikipedia.org/wiki/Powerset_construction)
    algorithm. This variant has been modified to ensure that the following
    additional properties hold:
    - Patterns are matched in the order in which they are defined.
    - The associated transition functions are maintained correctly through the
      transformation.
    - The lexing process is `O(n)`, where `n` is the size of the input.
3.  The DFA is then used to generate the rust code that implements that lexer.

The generated lexer contains a main loop that consumes the input stream
character-by-character, evaluating what is effectively a big `match` expression
that processes the input to evaluate the user-provided transition functions as
appropriate.

### Automated Code Generation

In order to avoid the lexer definition getting out of sync with its
implementation (the generated engine), it is necessary to create a separate
crate for the generated engine that has the lexer definition as one of its
dependencies.

This separation enables a call to `flexer::State::specialize()` in the crate's
`build.rs` (or a macro) during compilation. The output can be stored in a new
file i.e. `engine.rs` and exported from the library as needed. The project
structure would therefore appear as follows.

```
- lib/rust/lexer/
  - definition/
    - src/
      - lib.rs
    - cargo.toml

  - generation/
    - src/
      - engine.rs <-- the generated file
      - lib.rs    <-- `pub mod engine`
    - build.rs    <-- calls `flexer::State::specialize()` and saves its output to
                      `src/engine.rs`
    - cargo.toml <-- lexer-definition is in dependencies and build-dependencies
```

With this design, `flexer.generate_specialized_code()` is going to be executed
on each rebuild of `lexer/generation`. Therefore, `generation` should contain
only the minimum amount of logic, and should endeavor to minimize any
unnecessary dependencies to avoid recompiling too often.

## Structuring the Flexer Code

In order to unify the API between the definition and generated usages of the
flexer, the API is separated into the following components:

- `Flexer`: The main flexer definition itself, providing functionality common to
  the definition and implementation of all lexers.
- `flexer::State`: The stateful components of a lexer definition. This trait is
  implemented for a particular lexer definition, allowing the user to store
  arbitrary data in their lexer, as needed.
- **User-Defined Lexer:** The user can then define a lexer that _wraps_ the
  flexer, specialised to the particular `flexer::State` that the user has
  defined. It is recommended to implement `Deref` and `DerefMut` between the
  defined lexer and the `Flexer`, to allow for ease of use.

### Supporting Code Generation

This architecture separates out the generated code (which can be defined purely
on the user-defined lexer), from the code that is defined as part of the lexer
definition. This means that the same underlying structures can be used to both
_define_ the lexer, and be used by the generated code from that definition.

For an example of how these components are used in the generated lexer, please
see [`generated_api_test`](../../lib/rust/flexer/tests/generated_api_test.rs).
