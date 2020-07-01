---
layout: developer-doc
title: Macro Resolution
category: parser
tags: [parser, macro, resolution]
order: 5
---

# Macro Resolution
Macro resolution is the process of taking the structured token stream from the
[lexer](./lexer.md), and resolving it into the [ast](./ast.md) through the
process of resolving macros. This process produces a chunked AST stream,
including spacing-unaware elements.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Functionality](#functionality)
- [Errors During Macro Resolution](#errors-during-macro-resolution)

<!-- /MarkdownTOC -->

## Functionality
The current functionality of the macro resolver is as follows:

- TBC

The current overview of the macro resolution process can be found in the scala
[implementation](../../lib/syntax/specialization/shared/src/main/scala/org/enso/syntax/text/Parser.scala).

> The actionables for this section are:
>
> - Discuss how the space-unaware AST should be handled as it is produced by
>   macros.
> - Handle precedence for operators properly within macro resolution (e.g.
>   `x : a -> b : a -> c` should parse with the correct precedence).
> - Create a detailed design for how macro resolution should work.

## Errors During Macro Resolution
It is very important that, during macro resolution, the resolver produces
descriptive errors for error conditions in the macro resolver.

> The actionables for this section are:
>
> - Determine how best to provide detailed and specific errors from within the
>   macro resolution engine.
