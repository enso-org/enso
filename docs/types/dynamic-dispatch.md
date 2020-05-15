---
layout: developer-doc
title: Dynamic Dispatch
category: types
tags: [types, dispatch]
order: 6
---

# Dynamic Dispatch
Enso is a language that supports pervasive dynamic dispatch. This is a big boon
for usability, as users can write very flexible code that still plays nicely
with the GUI.

The current implementation of Enso supports single dispatch (dispatch purely on
the type of `this`), but there are broader visions afoot for the final
implementation of dynamic dispatch in Enso.

> The actionables for this section include:
>
> - Determining whether we want to support proper multiple dispatch in the
>   future. This is important to know as it has implications for the type
>   system, and the design of the dispatch algorithm.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Specificity](#specificity)
- [Multiple Dispatch](#multiple-dispatch)

<!-- /MarkdownTOC -->

## Specificity
In order to determine which of the potential dispatch candidates is the correct
one to select, the compiler needs to have a notion of _specificity_, which is
effectively an algorithm for determining which candidate is more specific than
another.

- Always prefer a member function for both `x.f y` and `f y x` notations.
- Only member functions, current module's functions, and imported functions are
  considered to be in scope. Local variable `f` could not be used in the `x.f y`
  syntax.
- Selecting the matching function:
  1. Look up the member function. If it exists, select it.
  2. If not, find all functions with the matching name in the current module and
     all directly imported modules. These functions are the _candidates_.
  3. Eliminate any candidate `X` for which there is another candidate `Y` whose
     `this` argument type is strictly more specific. That is, `Y` this type is a
     substitution of `X` this type but not vice versa.
  4. If not all of the remaining candidates have the same this type, the search
     fails.
  5. Eliminate any candidate `X` for which there is another candidate `Y` which
     type signature is strictly more specific. That is, `Y` type signature is a
     substitution of `X` type signature.
  6. If exactly one candidate remains, select it. Otherwise, the search fails.

> The actionables for this section are as follows:
>
> - THE ABOVE VERSION IS OLD. NEEDS UPDATING.
> - The definition of specificity for dispatch candidates (including how it
>   interacts with the subsumption relationship on typesets and the ordering of
>   arguments).

## Multiple Dispatch
It is an open question as to whether we want to support proper multiple dispatch
in Enso. Multiple dispatch refers to the dynamic dispatch target being
determined based not only on the type of the `this` argument, but the types of
the other arguments to the function.

To do multiple dispatch properly, it is very important to get a rigorous
specification of the specificity algorithm. It must account for:

- The typeset subsumption relationship.
- The ordering of arguments.
- How to handle defaulted and lazy arguments.
- Constraints in types. This means that for two candidates `f` and `g`, being
  dispatched on a type `t` with constraint `c`, the more specific candidate is
  the one that explicitly matches the constraints. An example follows:

  ```ruby
  type HasName
    name : String

  greet : t -> Nothing in IO
  greet _ = print "I have no name!"

  greet : (t : HasName) -> Nothing in IO
  greet t = print 'Hi, my name is `t.name`!'

  type Person
    Pers (name : String)

  main =
    p1 = Person.Pers "Joe"
    greet p1 # Hi, my name is Joe!
    greet 7  # I have no name
  ```

  Here, because `Person` conforms to the `HasName` interface, the second `greet`
  implementation is chosen because the constraints make it more specific.
