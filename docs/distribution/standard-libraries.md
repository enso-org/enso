---
layout: developer-doc
title: Standard Libraries
category: distribution
tags: [distribution, stdlib]
order: 8
---

# Standard Libraries

At the current stage, Enso ships with a small set of libraries that compose the
language's "standard library". This document provides a brief explanation of
these libraries, as well as notes on how they should be used.

<!-- MarkdownTOC levels="2,3" autolink="true" indent="  " -->

- [Base](#base)
  - [Builtins](#builtins)
- [Table](#table)
- [Test](#test)
- [Documentation](#documentation)
- [General Libraries Notes](#general-libraries-notes)

<!-- /MarkdownTOC -->

## Base

`Base` is the core library of Enso. It contains core types and data structures,
as well as basic functionality for interacting with the outside world. It can be
found in
[`distribution/std-lib/Standard/src/`](https://github.com/enso-org/enso/tree/main/distribution/std-lib/Standard/src).

`Base` is intended to be imported unqualified at the top of the file:
`from Standard.Base import all`. Items not included in this unqualified import
are considered to be more specialist or internal, and should be intentionally
imported by users.

### Builtins

In addition to the functionalities exposed in the standard library source, the
interpreter also contains a set of definitions that are considered "primitive"
and are hence built into the interpreter.

For the purposes of documentation, there is a
[`Builtins.enso`](https://github.com/enso-org/enso/tree/main/engine/runtime/src/main/resources/Builtins.enso)
file that provides stub definitions for these builtin functions. It is used for
documentation purposes, and must be kept up to date as the builtins change.

> #### Note: Shadow Definitions
>
> In time this file will be replaced by true shadow definitions for the language
> builtins. It is only a stop-gap measure to allow documenting this
> functionality at this point in time.

## Table

`Table` is Enso's dataframes library, providing functionality for loading and
analysing tabular data. It is a core data-science toolkit, that integrates
deeply with Enso and its IDE. It can be found in
[`distribution/std-lib/Table`](https://github.com/enso-org/enso/tree/main/distribution/std-lib/Table).

`Table` is designed to be imported unqualified: `from Table import all`. Items
not included in this unqualified import are considered to be more specialist or
internal, and should be intentionally imported by users.

## Test

`Test` is a library for testing and benchmarking Enso code. At this point in
time it is _very_ rudimentary, and needs significant improvement before we can
consider it an "official" part of the Enso standard libraries. It can be found
in
[`distribution/std-lib/Test`](https://github.com/enso-org/enso/tree/main/distribution/std-lib/Test).

`Test` is intended to be imported qualified: `import Test`. This ensures that
there aren't spurious name clashes between user-defined functionality and the
testing library.

## Documentation

These libraries are comprehensively documented, with all functionality
accompanied by comprehensive documentation comments. These are located _above_
each definition, for example:

```
## Sort the Vector.

   Arguments:
   - `on`: A projection from the element type to the value of that element
     being sorted on.
   - `by`: A function that compares the result of applying `on` to two
     elements, returning an Ordering to compare them.
   - `order`: The order in which the vector elements are sorted.

   By default, elements are sorted in ascending order, using the comparator
   `compare_to`. A custom comparator may be passed to the sort function.

   This is a stable sort, meaning that items that compare the same will not
   have their order changed by the sorting process.

   The complexities for this sort are:
   - *Worst-Case Time:* `O(n * log n)`
   - *Best-Case Time:* `O(n)`
   - *Average Time:* `O(n * log n)`
   - *Worst-Case Space:* `O(n)` additional

   ? Implementation Note
     The sort implementation is based upon an adaptive, iterative mergesort
     that requires far fewer than `n * log(n)` comparisons when the vector
     is partially sorted. When the vector is randomly ordered, the
     performance is equivalent to a standard mergesort.

     It takes equal advantage of ascending and descending runs in the array,
     making it much simpler to merge two or more sorted arrays: simply
     concatenate them and sort.

   > Example
     Sorting a vector of numbers.
         [5, 2, 3, 45, 15].sort == [2, 3, 5, 15, 45]

   > Example
     Sorting a vector of `Pair`s on the first element, descending.
         [Pair 1 2, Pair -1 8].sort (_.first) (order = Sort_Order.Descending)
sort : (Any -> Any) -> (Any -> Any -> Ordering) -> Sort_Order -> Vector
sort (on = x -> x) (by = (_.compare_to _)) (order = Sort_Order.Ascending) = ...
```

Such documentation blocks describe:

- **Summary:** A basic summary of the behaviour of the method.
- **Arguments:** Descriptions of each of the arguments to the function.
- **Additional Information:** Additional exposition about the method.
- **Note (Optional):** Optional notes containing potentially important
  information for more experienced users.
- **Examples:** Examples of the method's usage, with descriptions.

In addition, a function will have a type signature that describes the expected
types of the function arguments. It may also have defaults for its arguments,
which will be shown in the

## General Libraries Notes

Some notes on the general structure of these libraries.

- All of these libraries are considered to be WIP as they are missing many
  pieces of functionality that they should have.
- As the language doesn't currently have built-in support for access modifiers
  (e.g. `private`), so we instead use `PRIVATE` annotations at the top of
  documentation blocks. Any functionality annotated in such a form is not for
  public consumption.
- The `Base.Meta` functionality is considered to be unstable as it is inherently
  tied to the internals of the compiler and the interpreter.
