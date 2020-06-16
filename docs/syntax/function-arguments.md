---
layout: developer-doc
title: Function Arguments
category: syntax
tags: [syntax, functions]
order: 11
---

# Function Arguments
One of the biggest usability innovations of Enso is the set of argument types
that it supports. The combination of named and defaulted arguments with a
curried language creates a tool in which it is very clear to express even
complex APIs.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Positional Arguments](#positional-arguments)
- [Named Arguments](#named-arguments)
- [Defaulted Arguments](#defaulted-arguments)
- [Optional Arguments](#optional-arguments)
- [Splats Arguments \(Variadics\)](#splats-arguments-variadics)
- [Type Applications](#type-applications)
- [Underscore Arguments](#underscore-arguments)

<!-- /MarkdownTOC -->

## Positional Arguments
Much like most programming languages, functions in Enso can be called with their
arguments provided positionally. This is the simple case that everybody is
familiar with.

## Named Arguments
All arguments in Enso are defined with a name. Like all programming languages,
this is necessary for that argument to be used. However, what Enso allows is for
users to then _call_ those arguments by name.

- An argument is called by name using the syntax `(name = value)` (or one may
  also take advantage of the operator precedence to write `name=value`).
- Named arguments are applied in the order they are given. This means that if
  you positionally apply to an argument `foo` and then try to later apply to it
  by name, this will fail due to currying of functions.
- Named arguments _cannot_ be used while using operator syntax. This means that
  an expression of the form `a + b` cannot apply arguments by name. However,
  when calling the operator as a method (`a.+ b`), the call-by-name syntax may
  indeed be used (`a.+ (that = b)`).

This is a great usability boon as in complex APIs it can often be difficult to
remember the order or arguments.

## Defaulted Arguments
Enso also allows users to define their functions with _defaults_ for the
function's arguments. This is very useful for complex APIs as it allows users to
experiment and iterate quickly by only providing the arguments that they want to
customise.

- An argument is defined with a default using the syntax `(name = default_val)`,
  which, as above, accounts for precedence rules.
- Argument defaults are applied to the function if no argument value is provided
  by position or name for that argument.
- Argument defaults are evaluated lazily if the function is lazy in that
  argument.
- We provide a `...` operator which suspends application of the default
  arguments for the purposes of currying.

## Optional Arguments
There are certain cases where the type information for an argument may be able
to be inferred by the compiler. This is best explained by example. Consider the
implementation of a `read` function that reads text and outputs a value of a
particular type.

```ruby
read : Text -> t -> t
read text this = t.fromText text
```

You can use this function by explicitly providing the type information in either
of the following ways:

```ruby
val1 = read '5' Int
val2 = Int.read '5'
```

This, however, is often tedious, especially in contexts where this information
could be inferred by the compiler. We can re-write `read` as follows:

```ruby
read : Text -> (t=t) -> t
read text (this=this) = t.fromText text
```

This allows users both to provide the argument explicitly or leave it out. In
the case where it is not provided, the compiler will attempt to infer it from
usage. If this is impossible, an error would be raised.

Enso provides a syntactic sugar for the `t=t` syntax. The above code can be
written instead using `?`.

```ruby
read : Text -> t? -> t
read text this? = t.fromText text
```

## Splats Arguments (Variadics)
Enso provides users with the ability to define variadic functions, or _splats_
functions in our terminology. These are very useful for defining expressive APIs
and flexible code.

- These work for both positional and keyword arguments.
- They are defined using the syntax `name...`, where `name` is an arbitrary
  argument name.

> The actionables for this section are:
>
> - Work out how (and if) this can interact with currying.
> - Do we even want this?

## Type Applications
There are sometimes cases where the user wants to explicitly refine the type of
an argument at the _call_ site of a function. This can be useful for debugging,
and for writing ad-hoc code. Much like the named-arguments in applications
above, Enso also provides a syntax for refining types at the application site.

- To refine an argument type by name at the application site, use the `:=`
  operator (e.g. `arg_name := T`).
- This _will_ be type-checked by the compiler, and so `T` must be a valid
  subtype for the type inferred for (or defined for) the function being called.

## Underscore Arguments
Enso provides the `_` argument as a quick way to create a lambda from a function
call. It obeys the following rules.

- Replacing any function argument with `_` will create a lambda that accepts an
  argument and passes it in the place of the underscore. All other function
  arguments are applied as normal.
- This works both by name and positionally.
- When a function is provided multiple `_` arguments, they are desugared left to
  right as the arguments would be applied to the function definition, creating
  nested lambdas.
