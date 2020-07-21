---
layout: developer-doc
title: Defining Functions
category: syntax
tags: [syntax, functions]
order: 10
---

# Defining Functions

Enso is a purely-functional programming language. As a result it has support for
[first-class and higher-order functions](https://en.wikipedia.org/wiki/Functional_programming#First-class_and_higher-order_functions),
meaning that you can pass functions as arguments to other functions, return
functions from functions, assign them to variables, store them in data
structures and so on.

Functions in Enso are curried by default, meaning that all functions are
actually functions in one argument, but may return functions accepting further
arguments.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Lambdas](#lambdas)
- [Defining Functions](#defining-functions)
- [Methods](#methods)
- [Universal Call Syntax](#universal-call-syntax)
- [Code Blocks](#code-blocks)
- [Operators](#operators)
  - [Precedence](#precedence)
  - [Sections](#sections)
- [Mixfix Functions](#mixfix-functions)

<!-- /MarkdownTOC -->

## Lambdas

The most primitive non-atom construct in Enso is the lambda. This is an
anonymous function in one argument. A lambda is defined using the `->` operator,
where the left hand side is an argument, and the right hand side is the body of
the function (containing arbitrary code).

Some functional languages such as Haskell allow for the definition of a lambda
with multiple arguments, but in Enso the type signature use of `->`and the
lambda use of `->` are one and the same. We do not want to have to put the
components of a type signature in parentheses, so we only allow one argument
before each arrow.

- Lambdas can close over variables in their surrounding scope.
- If you want to define a multi-argument lambda, you can do it by having a
  lambda return another lambda (e.g. `a -> b -> a + b`).

Additionally, lambdas in Enso have the following properties:

- The lambda introduces a new scope shared by the left and right operands.
- The left operand introduces a pattern context.
- If a lambda occurs in a pattern context, its left-hand-side identifiers are
  introduced into the scope targeted by the outer pattern context. For example,
  the following is valid `(a -> b) -> a.default + b`.
- Lambdas cannot currently occur in a matching context.

Please note that if a later lambda in a chain shadows an earlier lambda (e.g.
`a -> a -> a`), the shadowed arguments by that name are inaccessible. If you
want to unify later arguments with previous ones, you must employ the scope
reference rule and write (in this case) `a -> A -> a`.

> The actionables for this section are:
>
> - In the future we want to be able to match on function types, so this
>   restriction should be relaxed.
> - Do we want any automated unification to take place in the shadowing case?

## Defining Functions

A function definition is just syntactic sugar for the definition of a lambda,
and hence has all the properties that a lambda does. Syntactically, functions
are defined in a similar way to variables. The only difference is that the
function name is followed by one or more parameters.

```ruby
sum x y = x + y
```

Under the hood, functions are desugared to a lambda assigned to a variable that
binds the function name. This means that:

- Like any variable, you can use the `:` type ascription operator to provide a
  user-defined type for the function.

  ```ruby
  sum : (a: Monoid) -> a -> a
  sum : x -> y -> x + y
  sum x y = x + y
  ```

- Functions have an _arity_. Unlike a single lambda which always has an arity of
  one, function arity refers to the number of arguments in the function
  definition, which may not always be deduced from the type signature, but may
  still be inferred.

## Methods

Enso makes a distinction between functions and methods. In Enso, a method is a
function where the first argument (known as the `this` argument) is associated
with a given atom. Methods are dispatched dynamically based on the type of the
`this` argument, while functions are not.

Methods can be defined in Enso in two ways:

1. **In the Body of a Type:** A function defined in the body of a `type`
   definition is automatically converted to a method on all the atoms defined in
   the body of that type definition.

```ruby
type Maybe a
    Nothing
    type Just (value : a)

    isJust = case this of
        Nothing -> False
        Just _ -> True
```

2. **As an Extension Method:** A function defined _explicitly_ on an atom counts
   as an extension method on that atom. It can be defined on a typeset to apply
   to all the atoms within that typeset.

```ruby
Number.floor = case this of
    Integer -> ...
    ...
```

3. **As a Function with an Explicit `this` Argument:** A function defined with
   the type of the `this` argument specified to be a type.

```ruby
floor (this : Number) = case this of
    Integer -> ...
```

If the user does not explicitly specify the `this` argument by name when
defining a method (e.g. they use the `Type.name` syntax), it is implicitly added
to the start of the argument list.

## Universal Call Syntax

Calling a function or method is, in general, as simple as applying it to some
arguments. However, as Enso supports both methods and functions, it is very
important that users do not have to think about which of the two they are using
when calling it. To that end, Enso supports what is known as Uniform Call Syntax
(UCS).

- Where the syntax for calling methods differs from the syntax for calling
  functions, there are needless constraints on writing generic code.
- This is a needless constraint as both notations have their advantages.
- Enso has two notations, but one unified semantics.

The rules for the uniform syntax call translation in Enso are as follows:

1. For an expression `t.fn <args>`, this is equivalent to `fn t <args>`.
2. For an expression `fn t <args>`, this is equivalent to `t.fn <args>`.

## Code Blocks

Top-level blocks in the language are evaluated immediately. This means that the
layout of the code has no impact on semantics of the code:

- This means that the following `a` and `b` are equivalent.

  ```ruby
  a = foo x y

  b =
    foo x y
  ```

- To suspend blocks, we provide a `suspend` function in the standard library.
- This function takes any expression as an argument (including a block), and
  suspends the execution of that expression such that it is not evaluated until
  forced later.

  ```ruby
  susp = suspend
    x = foo x y z
    x.do_thing
  ```

  Alternatively, it is sufficient to type the binding for the block as
  `Suspended a` where `a` is the type of the block.

  ```ruby
  susp : Suspended a =
    x = foo x y z
    x.do_thing
  ```

  It should be noted that this does not yet work.

The following rules apply to code blocks:

- Code blocks are desugared into in-order applications of monadic bind (as in
  keeping with the fact that all blocks are monadic contexts).
- If an expression that returns a value is not assigned to an identifier, this
  will issue a warning.
- To suppress this warning you can assign it to a blank (`_`).

```ruby
test =
    _ = expr1
    expr2

# Becomes
test =
    expr1 >>= (_ -> expr2)

# Equivalent to
test =
    expr1 >> expr2
```

- If the trailing line of the block (the return value) is an assignment, it will
  return `Nothing` as all assignments do.

```ruby
foo =
    pat1 = expr1

# Becomes
foo =
    expr1 >>= (pat1 -> Nothing)
```

## Operators

In Enso, an operator is a function with a non-alphanumeric name (e.g. `+`). We
only support binary operators, with left and right arguments.

Enso provides a significant amount of flexibility for developers who want to
define custom operators. Formally, any sequence of the following characters
forms an operators `.!$%&*+-/<>?^~\`. Operator definitions have three main
parts:

- **Definition:** This defines a function that is called on the arguments
  provided to the operator.
- **Precedence:** This is an optional block that defines the
  [precedence relation](https://en.wikipedia.org/wiki/Order_of_operations) for
  the operator. Precedence in Enso is specified _in relation_ to existing
  operators. If you do not provide this information, no precedence relations
  will be defined.
- **Associativity:** This is an optional block that defines the
  [operator associativity](https://en.wikipedia.org/wiki/Operator_associativity)
  to be either `left`, `right`, or `none`. If you do not provide this, the
  operator's associativity will default to `left`.

```ruby
@prec  [> *, < $]
@assoc left
^ a n = a * a ^ (n-1)
```

### Precedence

Operator precedence in Enso is a collection of rules that reflect conventions
about which operations to perform first in order to evaluate a given expression
that contains operators. However, operator precedence in Enso differs from many
other programming languages.

- Precedence is not set at fixed levels, but is instead defined in relation to
  the precedence of other operators.
- Precedence of an operator in Enso depends on whether a particular operator is
  surrounded by spaces or not. This means that the precedence of _any_ operator
  not surrounded by spaces is always higher than the precedence of any operator
  surrounded by spaces. The only exception to this rule is the `,` operator,
  which retains the same precedence level regardless of whether it is surrounded
  by spaces or not.

This space-based precedence may seem strange coming from other languages, but it
allows for writing _far_ cleaner code than other functional languages. This is
best demonstrated by example. Consider the following code:

```ruby
list       = 1 .. 100
randomList = each random list
headOfList = take 10 randomList
result     = sort headOfList
```

This could easily be refactored to the following one-liner, and then transformed
using UCS to an expression that reads left to right:

```ruby
result = sort (take 10 (each random (1 .. 100)))

result = (((1 .. 100).each random).take 10).sort
```

This is still quite noisy, however, so using the whitespace-sensitive operator
precedence rules, combined with the fact that the operator `.` is a regular
operator, we get the following.

```ruby
result = 1..100 . each random . take 10 . sort
```

### Sections

An operator section is a nice shorthand for partially applying an operator. It
works as follows.

- Where an argument is not applied to an operator, the missing argument is
  replaced by an implicit `_`.
- The application is then translated based upon the rules for
  [underscore arguments](./function-arguments.md#underscore-arguments) described
  later.
- The whitespace-based precedence rules discussed above also apply to operator
  sections.

## Mixfix Functions

A mixfix function is a function that is made up of multiple sections. They are
defined using a special syntax, and operate as follows:

- They are defined using a 'split snake case'. The first section is written as
  normal, but subsequent sections are prefixed by an underscore (`if c _then a`,
  for example).
- The layout rules applied to mixfix functions operate as if each section was a
  separate operator, allowing you to write an indented block of code after each
  section.

Probably the best-known example of a mixfix function is `if-then-else`, which is
indeed defined in the Enso standard library.

```ruby
if foo == bar then frob else
    thing1
    thing2
```
