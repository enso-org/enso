---
layout: developer-doc
title: Layout Rules
category: syntax
tags: [syntax, layout]
order: 3
---

# Layout Rules

Enso is a layout-aware programming language, in that layout rules are used to
determine code structure. The layout rules in Enso are intended to provide for
an intuitive way to format code.

This document describes the layout rules for Enso's white-space.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Maximum Line Length](#maximum-line-length)
- [Indented Blocks](#indented-blocks)
  - [Trailing Operator on the Parent Line](#trailing-operator-on-the-parent-line)
  - [Leading Operator on All Child Lines](#leading-operator-on-all-child-lines)
  - [No Leading or Trailing Operators](#no-leading-or-trailing-operators)
  - [Debug Line Breaks](#debug-line-breaks)

<!-- /MarkdownTOC -->

## Maximum Line Length

The maximum length of a line in an Enso source file is restricted to 80
characters outside of text blocks. If your code exceeds this limit, the compiler
will emit a warning message.

There is no option to change this limit in order to enforce visual consistency
in user code. The reasoning behind this is as follows:

- The default soft-wrapping of long lines in editors is highly disruptive to the
  visual structure of code, making it harder to understand.
- Code should still be understandable on smaller screens or with multiple-column
  views.

## Indented Blocks

Indentation in Enso is used to start a block. Every indented line is considered
to be a sub-structure of the nearest previous line with lower indentation. We
refer to these as the 'child' and the 'parent' lines respectively. This means
that any region at the same indentation is considered to be part of the same
block, and blocks may contain blocks.

```ruby
block =
    x = 2 . replicate 7 . map show . intercalate ","
    IO.println x
```

In addition, we have a set of custom layout rules that impact exactly how blocks
are defined. These are described in the following subsections.

### Trailing Operator on the Parent Line

If a line ends with an operator then all of its child lines form a
[_code_ block](./functions.md/#code-blocks). The most common usage of this kind
of indented block is a function definition body (following the `=` or `->`).

```ruby
test = a -> b ->
    sum = a + b
```

### Leading Operator on All Child Lines

If all the child lines in a block begin with an operator, the lines in the block
are considered to form a single expression.

This expression is built as follows:

1. Every line in the block is built as a standard inline expression, ignoring
   the leading operator.
2. The final expression is built top to bottom, treating the leading operators
   as left-associative with the lowest possible precedence level.

Please note that the operator at the _beginning_ of each child line is used
_after_ the line expression is formed.

```ruby
nums = 1..100
    . each random
    . sort
    . take 100
```

### No Leading or Trailing Operators

In the case where neither the parent line ends with a trailing operator, or the
child lines begin with an operator, every child line is considered to form a
separate expression passed as an argument to the parent expression. The most
common usage of this is to split long expressions across multiple lines.

```ruby
geo1 = sphere (radius = 15) (position = vector 10 0 10) (color = rgb 0 1 0)
geo2 = sphere
    radius   = 15
    position = vector 10 0 10
    color    = rgb 0 1 0
```

### Debug Line Breaks

In certain cases it may be useful to debug line breaks in code. To this end, we
provide a debug line-break operator `\\` which, when placed at the beginning of
a child line tells Enso to glue that line to the end of the previous one.

This should be avoided in production code and its use will issue a warning.

```ruby
debugFunc = v -> v2 ->
    print (v2.normalize * ((v.x * v.x) + (v.y * v.y)
      \\ + (v.z * v.z)).sqrt)

validFunc = v -> v2 ->
    len = ((v.x * v.x) + (v.y * v.y) + (v.z * v.z)).sqrt
    v2  = v2.normalize * len
    print v2
```
