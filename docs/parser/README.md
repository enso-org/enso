---
layout: docs-index
title: Enso's Parser
category: summary
tags: [parser, readme]
order: 0
---

# Enso's Parser
The parser is one of the most crucial components of the Enso runtime in that
_all_ code that a user writes must be parsed. This means that a good parser is
fast, responsive, and lightweight; it shouldn't baulk at having thousands of
lines of code thrown at it.

Enso's parser, however, is very special. In order to support interactive use it
has to narrow down the scope of a syntax error as much as possible, while still
providing useful output for the compiler around the rest of the parse errors.
This feature makes it more complex than many common parsers, so making this work
while still preserving performance is of paramount importance.

The various components of the parser's design and architecture are described
below:

- [**Tech Analysis:**](./tech-analysis.md) A brief overview of the reasons for
  the implementation technologies for the parser.
- [**Parser Architecture:**](./architecture.md) An overview of the architecture
  of the parser as a whole.
- [**Flexer:**](./flexer.md) An overview of the design and architecture of the
  flexer, a generic, DFA-based lexing engine.
- [**Lexer:**](./lexer.md) The Enso lexer, responsible for tokenising the input
  stream of source code.
- [**Macro Resolution:**](./macro-resolution.md) The system for defining and
  resolving macros on the token stream.
- [**Operator Resolution:**](./operator-resolution.md) The system for resolving
  operator applications properly.
- [**Construct Resolution:**](./construct-resolution.md) The system for
  resolving higher-level language constructs in the AST to produce a useful
  output.
- [**Parser Driver:**](./parser-driver.md)
- [**AST:**](./ast.md) The parser AST.
- [**JVM Object Generation:**](./jvm-object-generation.md) The process for
  generating the ast representation on the JVM via reflection.
- [**Reading Source Code:**](./reader.md) The flexible architecture for reading
  source code into the lexer.
