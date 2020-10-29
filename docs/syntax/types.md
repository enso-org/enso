---
layout: developer-doc
title: Types and Type Signatures
category: syntax
tags: [syntax, types]
order: 7
---

# Types and Type Signatures

Enso is a statically typed language, meaning that every variable is associated
with information about the possible values it can take. In Enso, the type
language is the same as the term language, with no artificial separation. For
more information on the type system, please see the [types](../types/README.md)
design document.

This section will refer to terminology that has not been defined in _this_
document. This is as this document is a specification rather than a guide, and
it is expected that you have read the above-linked document on the type-system
design as well.

Additionally, this document will colloquially refer to the left and right hand
sides of the type ascription operator `:` as the 'term' and 'type' levels,
respectively. In reality, there is no separation between the two in Enso, but it
is a useful way of thinking about things when discussing type signatures.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Type Signatures](#type-signatures)
  - [Type Operators](#type-operators)
  - [Typeset Literals](#typeset-literals)
  - [Writing Type Signatures](#writing-type-signatures)
  - [Behaviour of Type Signatures](#behaviour-of-type-signatures)
- [Operations on Types](#operations-on-types)
- [Type Definitions](#type-definitions)
  - [Body Atom Definitions](#body-atom-definitions)
  - [Visibility and Access Modifiers](#visibility-and-access-modifiers)

<!-- /MarkdownTOC -->

## Type Signatures

Enso allows users to provide explicit type signatures for values through use of
the type ascription operator `:`. The expression `a : b` says that the value `a`
has the type `b` attributed to it.

```ruby
foo : (m : Monoid) -> m.this
```

Type signatures in Enso have some special syntax:

- The reserved name `in` is used to specify the monadic context in which a value
  resides. The Enso expression `a in IO` is equivalent to the Haskell
  `MonadIO a`.

  ```ruby
  foo : Int -> Int in IO
  ```

- The operator `!` is used to specify the potential for an _error_ value in a
  type. The expression `a ! E` says that the type is either an `a` or an error
  value of type `E`.

  ```ruby
  / : Number -> Number -> Number ! ArithError
  ```

### Type Operators

Please note that `:`, `in`, and `!` all behave as _standard operators_ in Enso.
This means that you can section them, which is incredibly useful for programming
with types. In addition, Enso supports a number of additional operators for
working with types. These are listed below.

|                                           Operator                                            |     Precedence Relations     | Level | Assoc. | Description                                                                 |
| :-------------------------------------------------------------------------------------------: | :--------------------------: | :---: | :----: | :-------------------------------------------------------------------------- |
|                                              `:`                                              |            `> =`             |   0   |  Left  | Ascribes the type (the right operand) to the value of the left operand.     |
|                                             `in`                                              |         `> :`, `> !`         |   3   |  Left  | Ascribes the context (the right operand) to the value of the left operand.  |
|                                              `!`                                              |        `> :`, `> ->`         |   2   |  Left  | Combines the left operand with the right operand as an error value.         |
|                                             `->`                                              |            `> :`             |   1   |  Left  | Represents a mapping from the left operand to the right operand (function). |
|                                             `<:`                                              |    `> !`, `< \|`, `> in`     |   4   |  Left  | Asserts that the left operand is structurally subsumed by the right.        |
|                                              `~`                                              |           `== <:`            |   4   |  Left  | Asserts that the left and right operands are structurally equal.            |
|                                              `;`                                              |         `< :`, `> =`         |  -2   |  Left  | Concatenates the left and right operand typesets to create a new typeset.   |
|                                             `\|`                                              | `> <:`, `> !`, `> in`, `> :` |   5   |  Left  | Computes the union of the left and right operand typesets.                  |
|                                              `&`                                              |            `> \|`            |   6   |  Left  | Computes the intersection of the left and right operand typesets.           |
| `\` | `> &` | 7 | Left | Computes the subtraction of the right typeset from the left typeset. |
|                                             `:=`                                              |     `< :`, `> =`, `> ;`      |  -1   |  Left  | Creates a typeset member by assigning a value to a label.                   |

Solving this set of inequalities produces the _relative_ precedence levels for
these operators shown in the table above. In order to check this, you can use
the following formula as an input to an SMTLib compatible solver. For reference,
bind (`=`) has a relative level of -3 in this ordering.

```lisp
(declare-fun ascrip () Int)   ; `:`
(declare-fun bind () Int)     ; `=`
(declare-fun in () Int)       ; `in`
(declare-fun err () Int)      ; `!`
(declare-fun fn () Int)       ; `->`
(declare-fun sub () Int)      ; `<:`
(declare-fun eq () Int)       ; `~`
(declare-fun tsConcat () Int) ; `;`
(declare-fun tsUnion () Int)  ; `|`
(declare-fun tsInter () Int)  ; `&`
(declare-fun minus () Int)    ; `\`
(declare-fun tsMember () Int) ; `:=`

(assert (> ascrip bind))
(assert (> in ascrip))
(assert (> in err))
(assert (> err ascrip))
(assert (> err fn))
(assert (> fn ascrip))
(assert (> sub err))
(assert (< sub tsUnion))
(assert (> sub in))
(assert (= eq sub))
(assert (< tsConcat ascrip))
(assert (> tsConcat bind))
(assert (> tsUnion sub))
(assert (> tsUnion err))
(assert (> tsUnion in))
(assert (> tsUnion ascrip))
(assert (> tsInter tsUnion))
(assert (> minus tsInter))
(assert (< tsMember ascrip))
(assert (> tsMember bind))
(assert (> tsMember tsConcat))

(check-sat)
(get-model)
(exit)
```

A permalink to the program using an online Z3 console can be found
[here](https://rise4fun.com/Z3/e99K).

> The actionables for this section are:
>
> - Decide which of these should be exposed in the surface syntax.

### Typeset Literals

Sometimes it is useful or necessary to write a typeset _literal_ in your code.
These work as follows.

- **Typeset Member:** Syntax for typeset members have three components:

  - **Label:** The name of the member. This must always be present.
  - **Type:** The type of the member. This need not be present.
  - **Value:** A value for the member. This need not be present.

  This looks like the following:

  ```ruby
  label : Type := value
  ```

  Please note that the right-hand-side of the `:=` operator is _not_ a pattern
  context.

- **Member Concatenation:** Members can be combined into a typeset using the
  concatenation operator `;`.

  ```ruby
  x ; y
  ```

- **Typeset Literals:** A typeset literal consists of zero or more typeset
  member definitions concatenated while surrounded by curly braces `{}`. The
  braces are necessary as they delimit a pattern context to allow the
  introduction of new identifiers.

  ```ruby
  { x: T ; y: Q }
  ```

Typeset literals are considered to be a
[pattern context](./naming.md#pattern-contexts), and hence the standard rules
apply.

### Writing Type Signatures

When ascribing a type to a value, there are two main ways in which it can be
done. Both of these ways are _semantically_ equivalent, and ascribe the type
given by the signature (to the right of the `:`) to the expression to the left
of the `:`.

1.  **Inline Ascription:** Using the type ascription operator to associate a
    type signature with an arbitrary expression.

    ```ruby
    my_expr : Type
    ```

2.  **Freestanding Ascription:** Using the type ascription operator to associate
    a type with a name. The name must be defined on _the line below_ the
    ascription.

    ```ruby
    a : Type
    a = ...
    ```

3.  **Binding Ascription:** Using the type ascription operator to associate a
    type with a binding at the binding site.

    ```ruby
    a : Type = ... # this is equivalent to the above example

    (a : Type) -> ... # use in a lambda
    ```

> The actionables for this section are:
>
> - In the future do we want to support freestanding ascription that isn't
>   directly adjacent to the ascribed value?

### Behaviour of Type Signatures

In Enso, a type signature operates to constrain the values that a given variable
can hold. Type signatures are _always_ checked, but Enso may maintain more
specific information in the type inference and checking engines about the type
of a variable. This means that:

- Enso will infer constraints on types that you haven't necessarily written.
- Type signatures can act as a sanity check in that you can encode your
  intention as a type.
- If the value is of a known type (distinguished from a dynamic type),
  constraints will be introduced on that type.
- Where the type of the value is known, ascription can be used to constrain that
  type further.
- It is legal to add constraints to an identifier using `:` in any scope in
  which the identifier is visible.

From a syntactic perspective, the type ascription operator `:` has the following
properties:

- The right hand operand to the operator introduces a pattern context.
- The right hand side may declare fresh variables that occur in that scope.
- It is not possible to ascribe a type to an identifier without also assigning
  to that identifier.
- Introduced identifiers will always shadow other identifiers in scope due to
  the fact that `:` introduces a new scope on its RHS.
- Constraint implication is purely feed-forward. The expression `b:A` only
  introduces constraints to `b`.

> The actionables for this section are:
>
> - How do signatures interact with function bodies in regards to scoping?
> - Does this differ for root and non-root definitions?

## Operations on Types

Enso also provides a set of rich operations on its underlying type-system notion
of typesets. Their syntax is as follows:

- **Union - `|`:** The resultant typeset may contain a value of the union of its
  arguments.
- **Intersection - `&`:** The resultant typeset may contain values that are
  members of _both_ its arguments.
- **Subtraction - `\`:** The resultant typeset may contain values that are in
  the first argument's set but not in the second.

> The actionables for this section are:
>
> - Unify this with the types document at some point. The types document
>   supersedes this section while this actionable exists.

## Type Definitions

Types in Enso are defined by using the `type` reserved name. This works in a
context-dependent manner that is discussed properly in the
[type system design document](../types/README.md), but is summarised briefly
below.

- **Name and Fields:** When you provide the keyword with only a name and some
  field names, this creates an atom.

  ```ruby
  type Just value
  ```

- **Body with Atom Definitions:** If you provide a body with atom definitions,
  this defines a smart constructor that defines the atoms and related functions
  by returning a typeset.

  ```ruby
  type Maybe a
      Nothing
      type Just (value : a)

      isJust = case this of
          Nothing -> False
          Just _ -> True

      nothing = not isJust
  ```

  Please note that the `type Foo (a : t)` is syntax only allowable inside a type
  definition. It defines an atom `Foo`, but constrains the type variable of the
  atom _in this usage_.

- **Body Without Atom Definitions:** If you provide a body and do not define any
  atoms within it, this creates an interface that asserts no atoms as part of
  it.

  ```ruby
  type HasName
  name: String
  ```

In addition, users may write explicit `this` constraints in a type definition,
using the standard type-ascription syntax:

```ruby
type Semigroup
    <> : this -> this

type Monoid
    this : Semigroup
    use Nothing
```

### Body Atom Definitions

When defining an atom in the body of a type (as described above), there are two
ways in which you can define an atom:

1.  **Create a New Atom:** Using the `type` keyword inside the body of a type
    will define a _new_ atom with the fields you specify. The syntax for doing
    this is the same as that of a bare atom.

```ruby
type Just value
```

2.  **Include an Atom:** You can also use a type body to define methods on an
    already existing atom. To do this, you can _include_ the atom in the type's
    body by naming it explicitly. This will introduce it into the scope of your
    type and define any methods you define in your type on the included atom.

```ruby
Nothing
```

### Visibility and Access Modifiers

While we don't usually like making things private in a programming language, it
sometimes the case that it is necessary to indicate that certain fields should
not be touched (as this might break invariants and such like). To this end, we
propose an explicit mechanism for access modification that works as follows:

- We have a set of access modifiers, namely `private` and `unsafe`.
- We can place these modifiers before a top-level definition.

  ```ruby
  type MyAtomType
      type MyAtom a

      is_foo : Boolean
      is_foo = ...

      private private_method a b = ...

      unsafe unsafe_method a b = ...
  ```

- By default, accessing any member under an access modifier is an error when
  performed from another module.
- To use members protected by an access modifier, you must _import_ that access
  modifier from the file in which you want to access those elements.

  ```ruby
  import private Base.Data.Vector
  import unsafe Base.Data.Atom
  ```

- These modified imports are available in _all_ scopes, so it is possible to
  limit the scope in which you have access to the modified definitions.

  ```ruby
  function_using_modifiers v x =
      import private Base.Data.Vector
      import unsafe Base.Data.Atom

      v.mutate_at_index 0 (_ -> x)
      x = MyAtom.mutate_field name="sum" (with = x -> x + 20)
      x + 20
  ```

While `private` works as you might expect, coming from other languages, the
`unsafe` annotation has additional restrictions:

- It must be explicitly imported from `Base.Unsafe`.
- When you use `unsafe`, you must write a documentation comment on its usage
  that contains a section `Safety` that describes why this usage of unsafe is
  valid.
