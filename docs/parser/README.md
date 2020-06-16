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
