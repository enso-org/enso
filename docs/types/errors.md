---
layout: developer-doc
title: Errors
category: types
tags: [types, errors]
order: 12
---

# Errors
Enso supports two notions of errors. One is the standard asynchronous exceptions
model, while the other is a theory of 'broken values' that propagate through
computations.

> The actionables for this section are:
>
> - Greatly expand on the reasoning and theory behind the two exception models.
> - Explain why broken values serve the GUI well.
> - Explain how this can all be typed.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Async Exceptions](#async-exceptions)
- [Broken Values](#broken-values)

<!-- /MarkdownTOC -->

## Async Exceptions

> The actionables for this section are:
>
> - Formalise the model of async exceptions as implemented.

## Broken Values
In Enso we have the notion of a 'broken' value: one which is in an invalid state
but not an asynchronous error. While these may initially seem a touch useless,
they are actually key for the display of errors in the GUI.

Broken values can be thought of like checked monadic exceptions in Haskell, but
with an automatic propagation mechanism:

- Broken values that aren't handled explicitly are automatically promoted
  through the parent scope. This is trivial inference as no evidence discharge
  will have occurred on the value.

  ```ruby
  open : String -> String in IO ! IO.Exception
  open = ...

  test =
    print 'Opening the gates!'
    txt = open 'gates.txt'
    print 'Gates were opened!'
    7
  ```

  In the above example, the type of test is inferred to
  `test : Int in IO ! IO.Exception`, because no evidence discharge has taken
  place as the potential broken value hasn't been handled.
- This allows for very natural error handling in the GUI.

> The actionables for this section are:
>
> - Determine what kinds of APIs we want to use async exceptions for, and which
>   broken values are more suited for.
> - Ensure that we are okay with initially designing everything around async
>   exceptions as broken values are very hard to support without a type checker.
> - Initially not supported for APIs.
