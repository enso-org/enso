---
layout: developer-doc
title: Automated Parallelism Analysis
category: types
tags: [types, parallelism]
order: 10
---

# Automated Parallelism Analysis
The nature of Enso's type system provides the compiler with a significant
amount of information about the program as it runs. This information can be
exploited by the compiler to automatically parallelise some sections of Enso
programs.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Supporting Parallelism Analysis with Types](#supporting-parallelism-analysis-with-types)
- [Constructs That Can Be Parallelised](#constructs-that-can-be-parallelised)

<!-- /MarkdownTOC -->

## Supporting Parallelism Analysis with Types

> The actionables for this section are:
>
> - Work out how the type checker can support parallelism analysis.

## Constructs That Can Be Parallelised

> The actionables for this section are:
>
> - Provide an analysis of the language constructs that could automatically be
>   parallelised, and the typing predicates that make them so.
