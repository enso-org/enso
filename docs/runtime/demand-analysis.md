---
layout: developer-doc
title: Demand Analysis
category: runtime
tags: [runtime, demand-analysis, execution]
order: 2
---

# Demand Analysis

Demand analysis is the process of deciding when to compute the value of a
suspended computation in a language which supports suspended computation.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Determining When To Force](#determining-when-to-force)
- [Avoiding Pathological Force-Thunk Chains](#avoiding-pathological-force-thunk-chains)
- [The Demand Analysis Algorithm](#the-demand-analysis-algorithm)

<!-- /MarkdownTOC -->

## Determining When To Force

As Enso supports dynamic dispatch for methods, we cannot always (even in the
presence of a typechecker), statically determine whether or not the function
that will eventually be called at any given call site. This means that we have
to come up with a more efficient way to handle suspended arguments.

This is done by making the _function application_ responsible for determining
when a value passed to it should be evaluated. It works as follows:

1.  _All_ function arguments are passed suspended.
2.  The function application is provided with knowledge of its argument
    declarations.
3.  As a result, the _application_ is responsible for deciding which arguments
    should be evaluated up front, and which should be deferred.
4.  In conjunction with this, the suspended arguments to the application need to
    be explicitly forced at their use sites in the function body.

## Avoiding Pathological Force-Thunk Chains

The above approach does, when done naively, result in a severe performance
pathology when passing suspended arguments into functions also expecting
suspended arguments. Without intervention, the suspended argument gets wrapped
in a chain of thunks that has a significant performance cost to resolve, when
necessary.

In order to avoid this issue, we do the following:

1.  Where a suspended argument is passed into a function, we pass it _without_
    explicitly forcing it.
2.  All other uses of that argument are forced.

## The Demand Analysis Algorithm

The algorithm for performing demand analysis on Enso code is as follows.

```
for (node <- ir):
    if (node is a usage):
        if (node is not used in a function application):
            node = force(node)
```

This, however, is not entirely sufficient to support codegen. At the time of
generating truffle code, we want to know whether a given usage in an argument to
a function application needs to be wrapped in a suspension or left alone (as is
the case for a suspended term passed to a function).

To this end, we instead explicitly mark the arguments to the application with
whether or not they should be suspended during codegen.
