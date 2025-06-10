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
the type of `self`) when calling function. When calling (binary) operators Enso
may perform more complicated dispatch when searching for the right operator
implementation to invoke.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Specificity](#specificity)
- [Multiple Dispatch](#multiple-dispatch)
- [Resolving Clashes on `Any`](#resolving-clashes-on-any)

<!-- /MarkdownTOC -->

Another page related to [dispatch](../semantics/dispatch.md) exists.

## Specificity

In order to determine which of the potential dispatch candidates is the correct
one to select, the compiler needs to have a notion of _specificity_, which is
effectively an algorithm for determining which candidate is more specific than
another.

> [!WARNING] Static compiler selects nothing. The right method to invoke is
> _selected in the runtime_.
>
> - Always prefer a member function for both `x.f y` and `f y x` notations.
> - Only member functions, current module's functions, and imported functions
>   are considered to be in scope. Local variable `f` could not be used in the
>   `x.f y` syntax.
> - Selecting the matching function:
>   1. Look up the member function. If it exists, select it.
>   2. If not, find all functions with the matching name in the current module
>      and all directly imported modules. These functions are the _candidates_.
>   3. Eliminate any candidate `X` for which there is another candidate `Y`
>      whose `this` argument type is strictly more specific. That is, `Y` this
>      type is a substitution of `X` this type but not vice versa.
>   4. If not all of the remaining candidates have the same this type, the
>      search fails.
>   5. Eliminate any candidate `X` for which there is another candidate `Y`
>      which type signature is strictly more specific. That is, `Y` type
>      signature is a substitution of `X` type signature.
>   6. If exactly one candidate remains, select it. Otherwise, the search fails.

The runtime system of Enso identifies the type of a value in `obj.method_name`
invocation. It checks the _table of virtual methods_ for given type and finds
proper implementation of `method_name` to invoke. Should there be no method of
given name in the value's type (or its supertypes like `Any`) to invoke, a
`No_Such_Method` panic is raised.

There is a special dispatch for
[broken values & warnings](../semantics/errors.md).

## Multiple Dispatch

Multiple dispatch is currently used for
[binary operators](../syntax/functions.md#type-ascriptions-and-operator-resolution).

Multiple dispatch is also used on `from` conversions, because in expression
`T.from x` the function to use is based on both `T` and `x`.

> [!WARNING] Supporting general _multiple dispatch is unlikely_
>
> Supporting it for general functions remains an open question as to whether we
> want to support proper multiple dispatch in Enso. Multiple dispatch refers to
> the dynamic dispatch target being determined based not only on the type of the
> `this` argument, but the types of the other arguments to the function.
>
> To do multiple dispatch properly, it is very important to get a rigorous
> specification of the specificity algorithm. It must account for:
>
> - The typeset subsumption relationship.
> - The ordering of arguments.
> - How to handle defaulted and lazy arguments.
> - Constraints in types. This means that for two candidates `f` and `g`, being
>   dispatched on a type `t` with constraint `c`, the more specific candidate is
>   the one that explicitly matches the constraints. An example follows:
>
> ```ruby
>   type HasName
>     name : String
>
>   greet : t -> Nothing in IO
>   greet _ = print "I have no name!"
>
>   greet : (t : HasName) -> Nothing in IO
>   greet t = print 'Hi, my name is `t.name`!'
>
>   type Person
>     Pers (name : String)
>
>   main =
>     p1 = Person.Pers "Joe"
>     greet p1 # Hi, my name is Joe!
>     greet 7  # I have no name
> ```
>
> Here, because `Person` conforms to the `HasName` interface, the second `greet`
> implementation is chosen because the constraints make it more specific.

## Resolving Clashes on `Any`

Special attention must be paid to `Any` and its methods and extension methods.
`Any` is a super type of all objects in Enso. As such the methods available on
`Any` are also available on every object - including special objects like those
representing `type` and holding its _static methods_ (discussed at
[types page](types.md) as well).

There is a `to_text` _instance method_ defined on `Any` - what does it mean when
one calls `Integer.to_text`? Should it by treated as:

```ruby
Any.to_text Integer # yields Integer text
```

or should be a static reference to `Integer.to_text` method without providing
the argument? In case of _regular types_ like `Integer` the following code:

```ruby
main = Integer.to_text
```

is considered as invocation of _instance method_ `to_text` on object `Integer`
and yields `Integer` text.

The situation is different when a _module static_ method together with `Any`
_extension method_ is defined:

```ruby
# following function makes sure `simplify` can be called on any object
Any.simplify self = "Not a Test module, but"+self.to_text
# following "overrides" the method on Test module
simplify = "Test module"
```

With such a setup following code invokes `Any.simplify` extension method

```ruby
main = "Hi".simplify
```

and yields `Not a Test module, but Hi` text. On contrary the following code
yields `Test module` value:

```ruby
main = Test.simplify
```

When invoking a method on _module object_ its _module static methods_ take
precedence over _instance methods_ defined on `Any`. Thus a module serves
primarily as a _container for module (static) methods_.
