---
layout: developer-doc
title: Operator Resolution
category: parser
tags: [parser, operator, resolution]
order: 6
---

# Operator Resolution

Operator resolution is the process of resolving applications of operators into
specific nodes on the AST.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Resolution Algorithm](#resolution-algorithm)

<!-- /MarkdownTOC -->

## Resolution Algorithm

The operator resolution process uses a version of the classic
[shunting-yard algorithm](https://en.wikipedia.org/wiki/Shunting-yard_algorithm)
with modifications to support operator sections.

> The actionables for this section are:
>
> - Work out how to formulate this functionality efficiently in rust. The scala
>   implementation can be found
>   [here](../../lib/syntax/definition/src/main/scala/org/enso/syntax/text/prec/Operator.scala).
