---
layout: developer-doc
title: Goals for the Enso Type System
category: types
tags: [types, goals, design]
order: 1
---

# Goals for the Enso Type System

In our design for Enso, we firmly believe that the type system should be able to
aid the user in writing correct programs, far and above anything else. However,
with so much of our targeted user-base being significantly non-technical, it
needs to be as unobtrusive as possible.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [High-Level Goals](#high-level-goals)

<!-- /MarkdownTOC -->

## High-Level Goals

The high-level goals for the Enso type system are as follows:

> [!WARNING] _Not a goal anymore_: Enso is a dynamic language. Static type
> inference is _not needed for execution_. As such _static typing_ is an
> optional component - more a _linter_ than essential part of the system.
>
> Inference should have maximal power. We want users to be _forced_ to write
> type annotations in as few situations as possible. This means that, ideally,
> we are able to infer higher-rank types and make impredicative instantiations
> without annotations.

#### Error messages must be informative

Clear error messages are essential for figuring out what the user needs to
change in own program to eliminate the error. This is true for both:

- the runtime type errors
- the lints provided by _static type checker_

Ideally a type error shall contain proper identification of the error
origin/location and clear indication of what shall be changed to eliminate it.

#### Early Runtime Type Errors

Type checks shall be performed at the _"library boundary"_ rather than somewhere
deep inside of library code. Only then the user can under the context of the
error.

#### Powerful Enough Type System

Enso aim is to provide a powerful _enough_ type system to support development
done by _non-technical audience_. The type system shouldn't require master
degree in computer science to be used properly. As such the types are fully
optional and can be added gradually - usually only when a sample project is
turned into a reusable library.
