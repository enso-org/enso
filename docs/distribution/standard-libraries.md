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
- [Database](#database)
- [Geo](#geo)
- [Image](#image)
- [Table](#table)
- [Test](#test)
- [Visualization](#visualization)
- [Documentation](#documentation)
  - [Examples](#examples)
- [General Libraries Notes](#general-libraries-notes)

<!-- /MarkdownTOC -->

## Base

`Base` is the core library of Enso. It contains core types and data structures,
as well as basic functionality for interacting with the outside world. It can be
found in
[`distribution/lib/Standard/Base/0.1.0/src/Main.enso`](https://github.com/enso-org/enso/tree/main/distribution/lib/Standard/Base/0.1.0/src/Main.enso).

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

These methods are re-exported from `Base` where appropriate, and should not be
imported directly.

> #### Note: Shadow Definitions
>
> In time this file will be replaced by true shadow definitions for the language
> builtins. It is only a stop-gap measure to allow documenting this
> functionality at this point in time.

## Database

`Database` is a library that provides utilities for accessing data in databases
and processing that data efficiently. It is part of the Enso standard libraries
and is located in
[`distribution/lib/Standard/Database/0.1.0/src/Main.enso`](https://github.com/enso-org/enso/tree/main/distribution/lib/Standard/Database/0.1.0/src/Main.enso).

It is designed to be imported _qualified_.

## Geo

`Geo` is a library that contains very basic functionality for working with
geographic data. We hope to expand it greatly in the future. It is located in
[`distribution/lib/Standard/Geo/0.1.0/src/Main.enso`](https://github.com/enso-org/enso/tree/main/distribution/lib/Standard/Geo/0.1.0/src/Main.enso).

## Image

`Image` is a library that contains bindings to [OpenCV](https://opencv.org/)
that allows users to work with image data. It is located in
[`distribution/lib/Standard/Image/0.1.0/src/Main.enso`](https://github.com/enso-org/enso/tree/main/distribution/lib/Standard/Image/0.1.0/src/Main.enso).

## Table

`Table` is Enso's dataframes library, providing functionality for loading and
analysing tabular data. It is a core data-science toolkit, that integrates
deeply with Enso and its IDE. It can be found in
[`distribution/lib/Standard/Table/0.1.0/src/Main.enso`](https://github.com/enso-org/enso/tree/main/distribution/lib/Standard/Table/0.1.0/src/Main.enso).

`Table` is designed to be imported qualified: `import Table`.

## Test

`Test` is a library for testing and benchmarking Enso code. At this point in
time it is _very_ rudimentary, and needs significant improvement before we can
consider it an "official" part of the Enso standard libraries. It can be found
in
[`distribution/lib/Standard/Test/0.1.0/src/Main.enso`](https://github.com/enso-org/enso/tree/main/distribution/lib/Standard/Test/0.1.0/src/Main.enso).

`Test` is intended to be imported qualified: `import Test`. This ensures that
there aren't spurious name clashes between user-defined functionality and the
testing library.

## Visualization

`Visualization` is a semi-internal library that provides visualization-specific
utilities for displaying data in the IDE. It is located in
[`distribution/lib/Standard/Visualization/0.1.0/src/Main.enso`](https://github.com/enso-org/enso/tree/main/distribution/lib/Standard/Visualization/0.1.0/src/Main.enso).

## Documentation

These libraries are comprehensively documented, with all functionality
accompanied by comprehensive documentation comments. These are located _above_
each definition, for example:

```enso
## Sort the Vector.

   Arguments:
   - on: A projection from the element type to the value of that element
     being sorted on.
   - by: A function that compares the result of applying `on` to two
     elements, returning an Ordering to compare them.
   - order: The order in which the vector elements are sorted.

   By default, elements are sorted in ascending order, using the comparator
   `compare_to`. A custom comparator may be passed to the sort function.

   This is a stable sort, meaning that items that compare the same will not
   have their order changed by the sorting process.

   ! Computational Complexity
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

### Examples

All documentation blocks in Enso should contain comprehensive examples for how
to use the associated functionality. These documentation blocks fall into two
types:

1. **Stand-Alone:** A stand-alone example is a single Enso expression that can
   be pasted into any method body and will execute.
2. **Example Method:** An example method is a method named `example_*` that
   provides the example. They are used when specific imports are necessary to
   run the example, or when multiple lines are needed to provide an effective
   example.

All examples assume that the prelude is imported using
`from Standard.Base import all` in the file into which it is being pasted.

The
[`Standard.Examples`](https://github.com/enso-org/enso/tree/main/distribution/lib/Standard/Examples/0.1.0/src/Main.enso)
file contains example data for use in examples. If an example requires
non-trivial data on which to operate, it should be placed here.

## General Libraries Notes

Some notes on the general structure of these libraries.

- As the language doesn't currently have built-in support for access modifiers
  (e.g. `private`), so we instead use `PRIVATE` annotations at the top of
  documentation blocks. Any functionality annotated in such a form is not for
  public consumption.
- The `Base.Meta` functionality is considered to be unstable as it is inherently
  tied to the internals of the compiler and the interpreter.
