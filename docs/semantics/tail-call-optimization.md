---
layout: developer-doc
title: Managed Resources
category: semantics
tags: [resources, finalization, cleanup]
order: 10
---

# Tail Call Optimization

Tail call optimization is a powerful technique for optimizing functional
programs. It allows transforming recursive functions of certain shapes into
loops, removing unnecessary intermediate function calls and saving stack space.
This document outlines the usage and semantics of tail call optimization in
Enso.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Tail Calls](#tail-calls)
- [Usage](#usage)

<!-- /MarkdownTOC -->

## Tail Calls

A Tail Call is a function call occurring as the last statement in a function
body, i.e. an expression whose value is guaranteed not to be depended upon by
the function itself. For example,

```hs
sum_1 n = if n == 0 then 0 else 1 + sum_1 n-1

sum_2 n acc = if n == 0 then acc else @Tail_Call sum_2 n-1 acc+n
```

In the code snippet above, only the `sum_2` function is tail recursive. The
result of calling `sum_2` recursively is not depended upon by `sum_2` itself or
the definition of `if_then_else` method on booleans. On the other hand, `sum_1`
needs to know the value of its recursive call in order to perform the addition
operation. It is advised that functions that can be expressed with tail-calls
are implemented that way. Using tail call optimization, will lead to `sum_2`
being orders of magnitude faster than `sum_1`. Moreover, for `n = 100000000`,
`sum_1` will allocate a hundred million stack frames (over a gigabyte, likely
resulting in a stack overflow error), while `sum_2` is an allocation-free loop.

## Usage

Enso does not currently perform automatic tail call detection and defers the
optimization decisions to the user. To mark a function call as a tail call, the
`@Tail_Call` annotation must be used. Note that if the annotation is placed
incorrectly, it may either be reported as a warning by the compiler, or silently
ignored if such analysis is impossible to perform due to the compiler's limited
static analysis capabilities. However, it is _guaranteed_ that a wrongly placed
`@Tail_Call` annotation will not lead to incorrect runtime behavior.

If the `@Tail_Call` annotation is not placed, the call will be treated as a
standard on-stack function call.

For example, the following code reverses a list in a tail recursive fashion:

```
reverse list =
    go list result = case list of
        Nil -> result
        Cons head tail -> @Tail_Call go tail (Cons head result)
    result = go list Nil
    result
```

Note the placement of `@Tail_Call` in the recursive branch of `go`. It is placed
correctly, marking the last operation in a function, and therefore `go` will be
interpreted as a loop rather than a chain of function calls.

> #### Debugging Tail Calls
>
> The way `go` is wrapped in the example above is recommended for most uses.
> Using the assignment and return of a variable, rather than a direct call,
> guarantees that calls to `reverse` won't themselves be removed from the call
> stack and therefore greatly aids debugging.
