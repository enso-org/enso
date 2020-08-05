---
layout: developer-doc
title: Flexer
category: syntax
tags: [parser, flexer, lexer, dfa]
order: 3
---

# Flexer

The flexer is a finite-automata-based engine for generating lexers. Akin to
`flex` and other lexer generators, it is given a definition as a series of rules
from which it then generates code for a highly-optimised lexer.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Pattern Description](#pattern-description)
- [State Management](#state-management)
- [Code Generation](#code-generation)
  - [Automated Code Generation](#automated-code-generation)
  - [Notes on Code Generation](#notes-on-code-generation)
- [Structuring the Flexer Code](#structuring-the-flexer-code)
  - [Supporting the Definition of Lexers](#supporting-the-definition-of-lexers)
  - [Supporting Code Generation](#supporting-code-generation)
- [An Example](#an-example)

<!-- /MarkdownTOC -->

## Pattern Description

The definition of a lexer using the flexer library consists of a set of rules
for how to behave when matching portions of syntax. These rules behave as
follows:

- A rule describes a regex-like pattern.
- It also describes the code to be executed when the pattern is matched.

```rust
pub fn lexer_definition() -> String {
    // ...

    let chr = alphaNum | '_';
    let blank = Pattern::from('_')

    lexer.rule(lexer.root,blank,"self.on_ident(token::blank(self.start_location))");

    // ...
}
```

A pattern, such as `chr`, or `blank` is a description of the characters that
should be matched for that pattern to match. The flexer library provides a set
of basic matchers for doing this.

A `lexer.rule(...)` definition consists of the following parts:

- A state, used for grouping rules and named for debugging (see the section on
  [state management](#state-management) below).
- A pattern, as described above.
- The code that is executed when the pattern matches.

## State Management

States in the flexer engine provide a mechanism for grouping sets of rules
together known as `State`. At any given time, only rules from the _active_ state
are considered by the lexer.

- States are named for purposes of debugging.
- You can activate another state from within the flexer instance by using
  `state.push(new_state)`.
- You can deactivate the topmost state by using `state.pop()`.

## Code Generation

The patterns in a lexer definition are used to generate a highly-efficient and
specialised lexer. This translation process works as follows:

1.  All rules are taken and used to generate an NFA.
2.  A DFA is generated from the NFA using the standard
    [subset construction](https://en.wikipedia.org/wiki/Powerset_construction)
    algorithm, but with some additional optimisations that ensure the following
    properties hold:
    - Patterns are matched in the order that they are defined.
    - The associated code chunks are maintained properly.
    - Lexing is `O(n)`, where `n` is the size of the input.
3.  The DFA is used to generate the code for a lexer `Engine` struct, containing
    the `Lexer` definition.

The `Engine` generated through this process contains a main loop that consumes
the input stream character-by-character, evaluating a big switch generated from
the DFA using functions from the `Lexer`.

Lexing proceeds from top-to-bottom of the rules, and the first expression that
_matches fully_ is chosen. This differs from other common lexer generators, as
they mostly choose the _longest_ match instead. Once the pattern is matched, the
associated code is executed and the process starts over again until the input
stream has been consumed.

### Automated Code Generation

In order to avoid the lexer definition getting out of sync with its
implementation (the generated engine), it is necessary to create a separate
crate for the generated engine that has the lexer definition as one of its
dependencies.

This separation enables a call to `flexer.generate_specialized_code()` in
`build.rs` (or a macro) during compilation. The output can be stored in a new
file i.e. `lexer-engine.rs` and exported from the library with
`include!("lexer-engine.rs")`. The project structure therefore appears as
follows:

```
- lib/rust/lexer/
  - definition/
    - src/
      - lexer.rs
    - cargo.toml

  - generation/
    - src/
      - lexer.rs <-- include!("lexer-engine.rs")
    - build.rs   <-- calls `lexer_definition::lexer_source_code()`
                  -- and saves its output to `src/lexer-engine.rs`
    - cargo.toml <-- lexer-definition is in dependencies and build-dependencies
```

With this design, `flexer.generate_specialized_code()` is going to be executed
on each rebuild of `lexer/generation`. Therefore, `generation` should contain
only the minimum amount of logic (i.e. tests should be in separate crate) and
its dependencies should optimally involve only such code which directly
influences the content of generated code (in order to minimize the unnecessary
calls to expensive flexer specialization).

### Notes on Code Generation

The following properties are likely to hold for the code generation machinery.

- The vast majority of the code generated by the flexer is going to be the same
  for all lexers.
- The primary generation is in `consume_next_character`, which takes a `Lexer`
  as an argument.

## Structuring the Flexer Code

In order to unify the API between the definition and generated usages of the
flexer, the API is separated into the following components:

- **Flexer:** The main flexer definition itself, providing functionality common
  to the definition and implementation of all lexers.
- **FlexerState:** The stateful components of a lexer definition. This trait is
  implemented for a particular lexer definition, allowing the user to store
  arbitrary data in their lexer, as needed.
- **User-Defined Lexer:** The user can then define a lexer that _wraps_ the
  flexer, specialised to the particular `FlexerState` that the user has defined.
  It is recommended to implement `Deref` and `DerefMut` between the defined
  lexer and the `Flexer`, to allow for ease of use.

### Supporting the Definition of Lexers

> The actionables for this section are:
>
> - Fill it in as the generation solidifies.

### Supporting Code Generation

This architecture separates out the generated code (which can be defined purely
on the user-defined lexer), from the code that is defined as part of the lexer
definition. This means that the same underlying structures can be used to both
_define_ the lexer, and be used by the generated code from that definition.

For an example of how these components are used in the generated lexer, please
see [`generated_api_test`](../../lib/rust/flexer/tests/generated_api_test.rs).

## An Example

The following code provides a sketchy example of the intended API for the flexer
code generation using the definition of a simple lexer.

```rust
use crate::prelude::*;

use flexer;
use flexer::Flexer;



// =============
// === Token ===
// =============

pub struct Token {
    location : flexer::Location,
    ast      : TokenAst,
}

enum TokenAst {
    Var(ImString),
    Cons(ImString),
    Blank,
    ...
}

impl Token {
    pub fn new(location:Location, ast:TokenAst) -> Self {
        Self {location,ast}
    }

    pub fn var(location:Location, name:impl Into<ImString>) -> Self {
        let ast = TokenAst::Var(name.into());
        Self::new(location,ast)
    }

    ...
}



// =============
// === Lexer ===
// =============

#[derive(Debug,Default)]
struct Lexer<T:Flexer::State> {
    current : Option<Token>,
    tokens  : Vec<Token>,
    state   : T
}

impl Lexer {
    fn on_ident(&mut self, tok:Token) {
        self.current = Some(tok);
        self.state.push(self.ident_sfx_check);
    }

    fn on_ident_err_sfx(&mut self) {
        println!("OH NO!")
    }

    fn on_no_ident_err_sfx(&mut self) {
        let current = std::mem::take(&mut self.current).unwrap();
        self.tokens.push_back(current);
    }
}

impl Flexer::Definition Lexer {
    fn state     (&    self) -> &    flexer::State { &    self.state }
    fn state_mut (&mut self) -> &mut flexer::State { &mut self.state }
}

pub fn lexer_source_code() -> String {
    let lexer = Flexer::<Lexer<_>>::new();

    let chr     = alphaNum | '_';
    let blank   = Pattern::from('_');
    let body    = chr.many >> '\''.many();
    let var     = lowerLetter >> body;
    let cons    = upperLetter >> body;
    let breaker = "^`!@#$%^&*()-=+[]{}|;:<>,./ \t\r\n\\";

    let sfx_check = lexer.add(State("Identifier Suffix Check"));

    lexer.rule(lexer.root,var,"self.on_ident(Token::var(self.start_location,self.current_match()))");
    lexer.rule(lexer.root,cons,"self.on_ident(token::cons(self.start_location,self.current_match()))");
    lexer.rule(lexer.root,blank,"self.on_ident(token::blank(self.start_location))");
    lexer.rule(sfx_check,err_sfx,"self.on_ident_err_sfx()");
    lexer.rule(sfx_check,Flexer::always,"self.on_no_ident_err_sfx()");
    ...
    lexer.generate_specialized_code() // This code needs to become a source file, probably via build.rs
}
```

Some things to note:

- The function definitions in `Lexer` take `self` as their first argument
  because `Engine` implements `Deref` and `DerefMut` to `Lexer`.
