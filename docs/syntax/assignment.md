---
layout: developer-doc
title: Assignment Expressions
category: syntax
tags: [syntax, assignment]
order: 6
---

# Assignment Expressions

Assignment syntax in Enso is fairly magical, given that it is the language's
syntax for monadic bind.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [How Assignment Works](#how-assignment-works)
- [Function Definitions](#function-definitions)
- [Pattern Match Bindings](#pattern-match-bindings)
- [Extension Methods](#extension-methods)
  - [Method Syntax](#method-syntax)
  - [Function Syntax](#function-syntax)
- [Top-Level Assignments](#top-level-assignments)

<!-- /MarkdownTOC -->

## How Assignment Works

Assignment in Enso operates as follows:

- An assignment is an _expression_.
- The left-hand-side introduces a pattern context.
- The pattern on the left-hand-side is matched against (unified with) the value
  that occurs on its right-hand-side.
- A single line must contain at most one assignment.
- An assignment may only appear as the _root expression_ of a line of code in a
  file.
- An assignment returns the value `Nothing`, and does not return the value that
  is assigned to it.

The assignment operator has myriad uses, and is used to define variables,
functions, extension methods, and to perform pattern matching. Each different
case will see an appropriate desugaring applied (see below).

Please note that not _all_ occurrences of the `=` operator are assignments in
the general sense. The above rules do not apply when using said operator to pass
arguments by name.

## Function Definitions

If the left hand side of an assignment is syntactically a prefix application
chain, where the left-most name is a _variable_ name, the assignment is
considered to introduce a _function definition_ (the syntax sugared version).

For a prefix chain `a b c = ...`, this operates as follows:

- The name `a` is bound in the enclosing scope, and is called the 'function
  name'.
- The names `b` and `c` (the 'function arguments') are converted into nested
  lambda arguments in the function body.

In essence, the above example is equivalent to:

```ruby
a = b -> c -> ...
```

Please note that by the rules of naming specified previously, if an operator
occurs in the same position as `a` it will also be defined.

## Pattern Match Bindings

If the left hand side of an assignment is syntactically a prefix application
chain, where the left-most name is a _type_ name, the assignment is considered
to introduce a pattern match binding.

It operates as follows for code consisting of a prefix chain `A b c = expr` and
trailing code `tail...`.

```ruby
A b c = expr
tail...
```

- A case expression is created with scrutinee `expr`.
- The matching names `A`, `b`, and `c` are used in a case expression branch's
  pattern. The branch's expression is `tail...`.
- A catch-all branch is created that has expression `error`.

As each branch in a case expression has its own scope, this desugaring means
that the names `b` and `c` are made visible in the scope where the pattern match
binding occurs. This is due to the fact that pattern match branches are lambda
expressions, and reuse the same scoping rules.

This also works for operators in an infix position, where its operands will be
matched against.

## Extension Methods

There are two cases where an assignment creates an extension method:

1. **Method Syntax:** If the left-hand-side of an assignment is syntactically a
   prefix application chain where the left-most expression is an infix
   application of `.`, this assignment is considered to introduce an extension
   method.
2. **Function Syntax:** If the left hand side of an assignment is syntactically
   a prefix application chain where the left-most expression is a variable
   identifier and the second expression from the left is a variable named `this`
   with an explicit type ascription, this is also considered to introduce an
   extension method.

### Method Syntax

This syntax for extension methods works as follows:

- The target of the method syntax (left argument to `.`) defines the type on
  which the extension method is created.
- An implicit `this` argument is inserted with that type at the start of the
  arguments list.
- All arguments are desugared to lambda arguments.

```ruby
My_Type.method_name a b c = ...
```

### Function Syntax

This syntax for extension methods works as follows:

- The `this` argument type is used to define the type on which the extension
  method is created.
- `this` and all remaining arguments are desugared to lambda arguments.

```ruby
method_name (this : My_Type) a b c = ...
```

## Top-Level Assignments

In order to aid with disambiguation, any binding made in the root scope without
an explicit target is implicitly defined on a type representing the current
module. For example, a binding `main = ...` is implicitly `here.main = ...`.

This works as follows:

- All non-extension methods defined at the top level are augmented with an
  implicit first parameter `here`.
- They are callable by `name` if not ambiguous, but can be disambiguated by
  using `here.name` where necessary.
