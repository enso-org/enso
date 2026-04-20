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

Enso can detect tail call locations automatically while executing the code. A
deeply recursive function at _tail call position_ is profiled by Enso runtime
and _tail call optimizations_ get automatically enabled after few nested
invocations of the function. Should there be a need to enable such _tail call
optimizations_ immediatelly users (especially library providers) can explicitly
mark a function invocation as a tail call by adding the `@Tail_Call` annotation.
Then the _tail call optimization_ is enabled immediatelly without speculative
profiling. Should the `@Tail_Call` annotation be placed incorrectly, it may
either be reported as a warning by the compiler, or silently ignored.
Incorrectly placed `@Tail_Call` annotation however never leads to incorrect
runtime behavior.

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

The same example can be written without the `@Tail_Call` annotation. In such a
case few first invocations of the `go` function are performed as function calls.
Once the system detects the `go` invocation is eligible for _tail call
optimization_ it switches the function _tail status_ and interprets further
invocations as loop calls.
