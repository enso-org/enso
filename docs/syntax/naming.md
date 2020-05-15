---
layout: developer-doc
title: Naming
category: syntax
tags: [syntax, naming]
order: 2
---

# Naming
This file describes the syntax for naming language constructs in Enso, as well
as the various rules that names follow.

Names in Enso are restricted to using ASCII characters. This arises from the
simple fact that all names should be easy to type without less common input
methods. Furthermore, we enforce a rigid style for naming. This is in aid of
giving Enso code a uniform identity.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Naming Constructs](#naming-constructs)
- [Pattern Contexts](#pattern-contexts)
- [Localised Naming](#localised-naming)
- [Operator Naming](#operator-naming)
- [Reserved Names](#reserved-names)

<!-- /MarkdownTOC -->

## Naming Constructs
Given that Enso is dependently-typed, with no artificial separation between the
type and value-level syntaxes, an arbitrary name can refer to both types and
values. This means that naming itself can become a bit of a concern. At first
glance, there is no meaningful syntax-based disambiguation in certain contexts
(such as patterns and type signatures) between introducing a fresh variable, or
an occurrence of one already in scope.

As we still want to have a minimal syntax for such use-cases, Enso enforces the
following rules around naming:

- All identifiers are named as follows. This is known as 'variable' form.
  + Each 'word' in the identifier must be lower-case or a number.
  + Words in the identifier are separated using `_`.
  + Numbers may not occur as the first 'word' in an identifier.
- An identifier named as above can be referred to by capitalizing the first
  letter of each 'word' in the identifier. This is known as 'referent' form.
- No mixed-format names are allowed (e.g. `HTTP`, `foO`, `make_New`, or
  `Make_new`). These should be rejected by the compiler.
- We _strongly encourage_ using capitalised identifiers to refer to atoms.

Name resolution obeys the following rules:

- Contexts where it is _ambiguous_ as to whether a name is fresh or should refer
  to an identifier in scope are known as _pattern contexts_.
- In a [pattern context](#pattern-contexts), an identifier in referent form will
  _always_ refer to a name in scope, whereas an identifier in variable form is
  interpreted as the creation of a fresh identifier.
- This behaviour _only_ occurs in pattern contexts. In all other contexts,
  both conventions refer to that name already in scope.
- Operator names behave as variable names when placed in a prefix position
  (e.g. `+ a b`).
- Operator names behave as referent names when placed in an infix position (e.g.
  `a + b`).
- All literals (e.g. `1` and `"Hello"`) are treated as referent names.

Identifiers are introduced by:

- Naming them in a binding (assignments and function arguments).
- Using them in a pattern matching context (free variables).
- Using them in a type ascription (free variables).

## Pattern Contexts
A pattern context is a span in the code where variable identifiers (as described
above) can be used to introduce new identifiers into the scope containing the
pattern context. The following spans are pattern contexts:

- The left-hand-side of the assignment operator (`=`).
- The right-hand-side of the ascription operator (`:`).
- The left-hand-side of the arrow operator (`->`).

The following behaviours occur within a pattern context:

- Variable names are matched against corresponding portions of the expression
  and are introduced into scope.
- Type names require that the matched value is of a given structure (be that
  matching a typeset, atom, or some combination thereof), and allows for
  matching these fields recursively.
- Any literals (e.g. numbers) behave as type names.
- In any place where a variable identifier may be introduced in a pattern
  context, an `_` (known as an ignore) may be substituted. This does _not_ bind
  a new name, and hence cannot be used later.

In the core language, it should be noted that all non-trivial constructs are
desugared into the set of above constructs plus `case ... of` expressions. This
means that these are the _only_ constructs which introduce pattern contexts.

> Actionables for this section are:
>
> - Clarify exactly what "corresponding portions of the expression" actually
>   means in a formal sense.

## Localised Naming
We do, however, recognise that there is sometimes a need for unicode characters
in names (e.g. designing a high-level visual library that targets a narrow
domain in a specific country). To that end, Enso allows users to specify
optional localised names as part of a function's documentation.

Special support is provided for providing completions based on these localised
names in the language server, and in Enso Studio.

## Operator Naming
Operator names are those built solely from operator symbols (e.g. `+` or `<*>`).
Operator symbols are defined as characters in the following set.

```
!$%&*+-/<>?^~|:\,.()[]{}=
```

Please note that not every sequence that can be created from the above is a
_valid_ operator name, as some may collide with built-in language constructs
(e.g. `[` and `]`, which start and end a vector literal respectively).

## Reserved Names
Even though we do not intend to reserve any names at the level of the lexer or
parser, there are a number of constructs so core to the operation of Enso as a
language that we do not want to let them be overridden or redefined by users.
These constructs are known as reserved names, and these restrictions are
enforced in the compiler.

We reserve these names because allowing their redefinition would severely hinder
the readability and consistency of Enso code. They are as follows:

- `type`: This reserved name is used to define new atoms and typesets.
- `->`: This reserved name is the 'function' type, and represents a mapping from
  the type of its first operand to the type of its second operand.
- `:`: This reserved name is the type attribution operator. It ascribes the type
  described by its right operand to its left operand.
- `=`: This reserved name is the assignment operator, and assigns the value of
  its right operand to the name on its left. Under the hood this desugars to the
  relevant implementation of monadic bind.
- `.`: This is the forward function chaining operator.
- `case ... of`: This reserved name is the case expression that is fundamental
  to the operation of control flow in the language.
- `this`:  This reserved name is the one used to refer to the enclosing type in
  a method or type definition.
- `here`: This reserved name is the one used to refer to the enclosing module.
- `in`: Used to specify the monadic context(s) in a type signature.

Many of these reserved words are implemented as macros in the parser, but these
macros are always in scope and cannot be overridden, hidden, or redefined.

> The actionables for this section are as follows:
>
> - In the future, we need to determine if we need `all` and `each` explicit
>   keywords in the case of dependency. Explicit examples are required.
