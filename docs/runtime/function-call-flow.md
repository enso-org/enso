---
layout: developer-doc
title: Function Calling Flow
category: runtime
tags: [runtime, execution, interpreter]
order: 3
---

# Function Calling Flow
With Enso being a functional language, it is crucial for function calls to be
efficient and easily inlined. At the same time, the logic of calling functions
is involved – there's a plethora of features this system needs to support,
making the functions the most involved and somewhat convoluted part of the
interpreter's design.

The following is an overview of the logic, outlining the most important features
supported by this system and a diagram of how they fit into the Truffle
framework.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Tail Call Optimization](#tail-call-optimization)
- [Named Application Arguments](#named-application-arguments)
- [Definition-Site Argument Suspension](#definition-site-argument-suspension)
- [Currying and Eta-Expansion](#currying-and-eta-expansion)
- [Dynamic Dispatch](#dynamic-dispatch)
- [Defaulted Arguments and Application](#defaulted-arguments-and-application)
- [Flow Diagram](#flow-diagram)

<!-- /MarkdownTOC -->

## Tail Call Optimization
It is very important for Enso to perform TCO well, since there are no intrinsic
looping constructs in the language.

The idea behind Enso's TCO is simple, even if the implementation is confusing.
Whenever we call a possibly-tail-recursive function in a tail position, a tail
call exception containing the function and its arguments is thrown. This
exception is then caught in a loop, effectively translating recursion into a
loop. With the use of Truffle's `ControlFlowException`, this code is optimized
like a builtin language loop construct.

In pseudocode, a tail recursive function, like:

```ruby
foo x = if x == 0 then print "foo" else foo (x - 1)
```

becomes:

```ruby
foo x = if x == 0 then Y else throw (TailCallException foo [x-1])
```

Then, any non-tail call site like:

```ruby
z = foo 1000000
```

is translated into

```ruby
z = null
_fun = foo
_args = [1000000]
while true
  try
    z = _fun.call(args)
    break
  catch (TailCallException e)
    _fun = e.function
    _args = e.args
```

This logic is encapsulated in the various subclasses of `CallOptimiserNode`.

## Named Application Arguments
Enso allows applying function arguments by name, e.g.

```ruby
pow base exp = x ** y
z = foo exp=10 base=3
```

While certainly useful for the user, it requires some non-trivial facilitation
in the interpreter. An easy solution, would be to simply pass arguments in
a dictionary-like structure (e.g. a HashMap), but that has unacceptable
performance characteristics.

Therefore, the approach of choice is to compute and _cache_ a translation
table – a recipe for reordering arguments for a given function at a given
call site.

Based on the knowledge of the call-site schema (i.e. a description like
"3 arguments are provided, the first is named foo, the second is named bar and
the third is applied positionally") and the definition-site schema ("the
function has 3 parameters, named [baz, foo, bar]"), a mapping ("move the first
argument to the second position, the second becomes third and the third becomes
the first") is computed.

This mapping is then memoized and used to efficiently reorder arguments on each
execution, without the need to employ any more involved data structures. A
standard Truffle Polymorphic Inline Cache is used to store these mappings,
therefore it may be subject to the standard problems – storing rarely accessed
paths in the cache, as well as cache misses for highly polymorphic call sites.

This logic is encapsulated in the `ArgumentSorterNode`.

## Definition-Site Argument Suspension
Enso allows functions to define certain arguments as `Suspended`, so that when
these are passed to a function, the corresponding expressions are not evaluated
at the call site, but are instead passed to the function as closures for
evaluation at the function's discretion.

Therefore, all application arguments are actually treated as thunks and only
evaluated at call-site when the function signature defines them as eager.

Argument execution is happening inside the `ArgumentSorterNode`.

## Currying and Eta-Expansion
Functions can also be applied partially (i.e. with less arguments than required
by the signature, in which case the result is a function with certain arguments
fixed) or over-saturated (i.e. if a function returns another function, in which
case arguments for 2 consecutive calls can be passed in a single application
expression).

This logic is handled inside the `CurryNode`.

## Dynamic Dispatch
Functions can be dispatched dynamically, meaning a name can denote different
functions, based on the (runtime) type of the first argument.

This logic is fairly straightforward. It is triggered from `InvokeCallableNode`
and performed (with caching) in the `MethodResolverNode`.

## Defaulted Arguments and Application
As we want to provide a consistent semantics in the language (especially with
the use of multi-argument lambdas internally), there is one specific situation
that arises when handling applications with default arguments. As we expect a
returned lambda to be applied to any additional arguments (in keeping with
currying), a returned lambda that is _otherwise_ fully saturated (by default
arguments) should also be executed.

To this end, we make sure that the callsite checks if the return value from a
function is a function, and if it is fully saturated with defaults it will call
it.

## Flow Diagram
The following diagram summarizes all the nodes participating in a function
call. The entry points to this system are `ApplicationNode` (for in-language
function calls) and `InteropLibrary<Function>` (for polyglot function calls).

![Function Call Flow](https://user-images.githubusercontent.com/5780639/84035237-5c2d9800-a993-11ea-826d-72f3ddffcb54.png)
