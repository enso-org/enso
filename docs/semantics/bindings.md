---
layout: developer-doc
title: Bindings
category: semantics
tags: [semantics, bindings]
order: 1
---

# Bindings

A "binding" is a portion of and Enso program that creates a new name and binds a
value to that name.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Binding Return Value](#binding-return-value)

<!-- /MarkdownTOC -->

## Binding Return Value

While some expression-based languages with bindings have the binding return the
value assigned to the binding, we feel that this is far too error prone.
Consider the following code as a demonstration:

```ruby
if x = someExprEvaluatingToBool then foo else bar
```

This is the perennially-discussed C++ bug where you fail to type `==` in an
if-statement.

Enso, instead, takes the approach where a binding expression returns the
singleton value of the type `Nothing`, making the above-written code a type
error.
