---
layout: developer-doc
title: AST
category: parser
tags: [parser, ast]
order: 9
---

# AST
The parser AST describes the high-level syntactic structure of Enso, as well as
containing robust and descriptive parser errors directly in the AST.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Functionality](#functionality)

<!-- /MarkdownTOC -->

## Functionality
The parser AST needs to account for the following:

- A single `Name` type, removing the distinction between different names found
  in the [lexer](./lexer.md). This should provide functions `is_var`, `is_opr`,
  and `is_ref`.
- It should contain all of the language constructs that may appear in Enso's
  source.
- It should contain `Invalid` nodes, but these should be given a descriptive
  error as to _why_ the construct is invalid.
- It should also contain `Ambiguous` nodes, where a macro cannot be resolved in
  an unambiguous fashion.

Each node should contain:

- An identifier, attributed to it from the ID map.
- The start source position of the node, and the length (span) of the node.

> The actionables for this section are:
>
> - Flesh out the design for the AST based on the requirements of the various
>   parser phases.
