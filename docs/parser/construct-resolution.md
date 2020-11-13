---
layout: developer-doc
title: Construct Resolution
category: parser
tags: [parser, construct, resolution]
order: 7
---

# Construct Resolution

Construct resolution is the process of turning the low-level AST format into the
full high-level AST format that represents both all of Enso's language
constructs and contains rich error nodes.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Syntax Errors](#syntax-errors)

<!-- /MarkdownTOC -->

> The actionables for this section are:
>
> - Produce a detailed design for this resolution functionality, accounting for
>   all known current use cases.

## Syntax Errors

It is very important that Enso is able to provide descriptive and useful syntax
errors to its users. Doing so requires that it has a full understanding of the
language's syntax, but also that it is designed in such a fashion that it will
always succeed, regardless of any errors. Errors must be:

- Highly descriptive, so that it is easy for the runtime to explain to the user
  what went wrong.
- Highly localised, so that the scope of the error has as minimal an impact on
  parsing as possible.

> The actionables for this section are:
>
> - Determine how to design this parsing phase to obtain very accurate syntax
>   errors.
