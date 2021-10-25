---
layout: developer-doc
title: Operator Resolution
category: parser
tags: [parser, operator, resolution]
order: 5
---

# Operator Resolution

Operator resolution is the process of resolving applications of operators into
specific nodes on the AST.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Resolution Algorithm](#resolution-algorithm)

<!-- /MarkdownTOC -->

> The actionables for this section are:
>
> - Work out how to ensure that precedence and associativity isn't broken by the
>   macro resolution phase.
> - Work out how to handle the special case for `,`. We don't want comma to be
>   subject to the variable precedence functionality, as conventional spacing
>   for defining lists goes `[x, y, z]` and that should be allowed without the
>   variable precedence happening.
> - Work out how to handle the special case for `-`. The expression `-n` should
>   be treated as an application of the unary operator negate, while `- n`
>   should be treated as part of a larger expression (e.g. a section,
>   subtraction).
> - As Enso has no syntactic marker for the introduction of a lambda, we need to
>   have a special case for `->` so that it has appropriate precedence on its
>   left and right sides. Ideally, `map.fold 0 $ y -> foo $ y` is resolved as
>   `(map.fold 0) $ (y -> (foo $ y))`. This makes writing code much more
>   natural.

## Resolution Algorithm

The operator resolution process uses a version of the classic
[shunting-yard algorithm](https://en.wikipedia.org/wiki/Shunting-yard_algorithm)
with modifications to support operator sections.

> The actionables for this section are:
>
> - Work out how to formulate this functionality efficiently in rust. The scala
>   implementation can be found
>   [here](../../lib/syntax/definition/src/main/scala/org/enso/syntax/text/prec/Operator.scala).
