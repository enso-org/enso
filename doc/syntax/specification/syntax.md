# Enso: The Syntax
When working with a programming language, the syntax is the first thing that a
user encounters. This makes it _utterly integral_ to how users experience the
language, and, in the case of Enso, the tool as a whole.

Enso is a truly novel programming language in that it doesn't have _one_ syntax,
but instead has two. These syntaxes are dual: visual and textual. Both are
first-class, and are truly equivalent ways to represent and manipulate the
program. To that end, the design of the language's syntax requires careful
consideration, and this document attempts to explain both the _what_, of Enso's
syntax, but also the _why_.

Furthermore, Enso is novel in the fact that it does not enforce any artificial
restriction between the syntaxes of its type and value levels: they are one and
the same. This enables a staggering level of uniformity when programming in the
language, allowing arbitrary computations on types, because in a
dependently-typed world, they are just values.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Encoding](#encoding)
- [Naming](#naming)
  - [Localised Naming](#localised-naming)
  - [Operator Naming](#operator-naming)
  - [Pattern Contexts](#pattern-contexts)
  - [Reserved Names](#reserved-names)
- [Layout Rules](#layout-rules)
  - [Maximum Line Length](#maximum-line-length)
  - [Indented Blocks](#indented-blocks)
- [Literals](#literals)
  - [Numeric Literals](#numeric-literals)
  - [Text Literals](#text-literals)
  - [Vector Literals](#vector-literals)
- [Assignment](#assignment)
  - [Function Definitions](#function-definitions)
  - [Pattern Match Bindings](#pattern-match-bindings)
  - [Extension Methods](#extension-methods)
  - [Top-Level Assignments](#top-level-assignments)
- [Types and Type Signatures](#types-and-type-signatures)
  - [Type Signatures](#type-signatures)
  - [Operations on Types](#operations-on-types)
  - [Type Definitions](#type-definitions)
- [Macros](#macros)
  - [Annotations](#annotations)
- [Top-Level Syntax](#top-level-syntax)
  - [Main](#main)
  - [Against Top-Level Evaluation](#against-top-level-evaluation)
- [Functions](#functions)
  - [Lambdas](#lambdas)
  - [Defining Functions](#defining-functions)
  - [Methods](#methods)
  - [Calling Functions and UCS](#calling-functions-and-ucs)
  - [Code Blocks](#code-blocks)
  - [Operators](#operators)
  - [Mixfix Functions](#mixfix-functions)
- [Function Arguments](#function-arguments)
  - [Positional Arguments](#positional-arguments)
  - [Named Arguments](#named-arguments)
  - [Defaulted Arguments](#defaulted-arguments)
  - [Optional Arguments](#optional-arguments)
  - [Splats Arguments](#splats-arguments)
  - [Type Applications](#type-applications)
  - [Underscore Arguments](#underscore-arguments)
- [Field Access](#field-access)
  - [Pattern Matching](#pattern-matching)
  - [Projections / Lenses](#projections--lenses)
- [Comments](#comments)
  - [Documentation Comments](#documentation-comments)

<!-- /MarkdownTOC -->

## Encoding
While many modern programming languages are moving in a direction of being
liberal with the input they accept, we find that this often leads to the
resultant code being more difficult to use.

- **Source Encoding:** All input source code to Enso is UTF-8 encoded.
- **Indentation:** Indentation is four spaces, and all tabs are converted to 4
  spaces. This is not configurable on purpose

> The actionables for this section are:
>
> - Should the indentation rules be enforced by the parser / a later error
>   detection pass?

## Naming
Names in Enso are restricted to using ASCII characters. This arises from the
simple fact that all names should be easy to type without less common input
methods. Furthermore, we enforce a rigid style for naming. This is in aid of
giving Enso code a uniform identity.

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
- In a pattern context, an identifier in referent form will _always_ refer to a
  name in scope, whereas an identifier in variable form is interpreted as the
  creation of a fresh identifier.
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

### Localised Naming
We do, however, recognise that there is sometimes a need for unicode characters
in names (e.g. designing a high-level visual library that targets a narrow
domain in a specific country). To that end, Enso allows users to specify
optional localised names as part of a function's documentation.

Special support is provided for providing completions based on these localised
names in the language server, and in Enso Studio.

### Operator Naming
Operator names are those built solely from operator symbols (e.g. `+` or `<*>`).
Operator symbols are defined as characters in the following set.

```
!$%&*+-/<>?^~|:\,.()[]{}=
```

Please note that not every sequence that can be created from the above is a
_valid_ operator name, as some may collide with built-in language constructs
(e.g. `[` and `]`, which start and end a vector literal respectively).

### Pattern Contexts
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

### Reserved Names
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

## Layout Rules
Enso is a layout-aware programming language, in that layout rules are used to
determine code structure. The layout rules in Enso are intended to provide for
an intuitive way to format code.

### Maximum Line Length
The maximum length of a line in an Enso source file is restricted to 80
characters outside of text blocks. If your code exceeds this limit, the compiler
will emit a warning message.

There is no option to change this limit in order to enforce visual consistency
in user code. The reasoning behind this is as follows:

- The default soft-wrapping of long lines in editors is highly disruptive to the
  visual structure of code, making it harder to understand.
- Code should still be understandable on smaller screens or with multiple-column
  views.

### Indented Blocks
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

#### Trailing Operator on the Parent Line
If a line ends with an operator then all of its child lines form a
[_code_ block](#code-blocks). The most common usage of this kind of indented
block is a function definition body (following the `=` or `->`).

```ruby
test = a -> b ->
    sum = a + b
```

#### Leading Operator on All Child Lines
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

#### No Leading or Trailing Operators
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

#### Debug Line Breaks
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

## Literals
Enso supports a small set of literals that allow the expression of some common
types in literal form in the source code.

### Numeric Literals
Enso provides rich support for numeric literals, including literals that use
different numeric bases. It does, of course, support floating point numerals as
well.

A numeric literal takes the form:

```ebnf
digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
hex = "a" | "b" | "c" | "d" | "e" | "f";
number-digit = digit | hex;
decimal-point = ".";

float-digit = number-digit | decimal-point;

base-specifier = { digit };

numeric-literal = [base-specifier, "_"], { number-digit };
```

If no base is specified, it is inferred to be a standard base-10 numeral.

Some examples of numeric literals follow:

```ruby
decimal          = 12345.39
decimal_explicit = 10_1029301
octal            = 8_122137
hex              = 16_ae2f14
binary           = 2_10011101010
```

> Actionables for this section are:
>
> - Think about whether we want to support explicit fractional and complex
>   literals, or whether these should be relegated to type constructors.

### Text Literals
Enso provides rich support for textual literals in the language, supporting both
raw and interpolated strings natively.

- **Raw Strings:** Raw strings are delimited using the standard double-quote
  character (`"`). Raw strings have support for escape sequences.

  ```ruby
  raw_string = "Hello, world!"
  ```

- **Interpolated Strings:** Interpolated strings support the splicing of
  executable Enso expressions into the string. Such strings are delimited using
  the single-quote (`'`) character, and splices are delimited using the backtick
  (`` ` ``) character. Splices are run, and then the result is converted to a
  string using `show`. These strings also have support for escape sequences.

  ```ruby
  fmt_string = 'Hello, my age is `time.now.year - person.birthday.year`'
  ```

#### Inline Text Literals
In Enso, inline text literals are opened and closed using the corresponding
quote type for the literal. They may contain escape sequences but may _not_ be
broken across lines.

```ruby
inline_raw = "Foo bar baz"
inline_interpolated = 'Foo `bar` baz'
```

#### Text Block Literals
In Enso, text block literals rely on _layout_ to determine the end of the block,
allowing users to only _open_ the literal. Block literals are opened with three
of the relevant quote type, and the contents of the block are determined by the
following layout rules:

- The first child line of the block sets the baseline left margin for the block.
  Any indentation up to this margin will be removed.
- Any indentation further than this baseline will be retained as part of the
  text literal.
- The literal is _closed_ by the first line with a _lower_ level of indentation
  than the first child lineand will not contain the final blank line.

```
block_raw = '''
    part of the string
        still part of the string

    also part of the string

not_string_expr = foo bar
```

### Vector Literals
Enso also supports vector literals, which allow users to create literal vectors
of elements.

```ruby
literal = [elem_1, elem_2, elem_3, ...]
```

A vector literal works as follows:

- It is begun by the `[` character.
- It is ended by the `]` character.
- Elements in vector literals are concatenated using the `,` operator, which
  acts as `cons` on vectors.

## Assignment
The assignment operator in Enso is a fairly magical construct, being the
language's syntax for monadic bind. In essence, it operates as follows:

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
the general sense. The above rules do not apply when using said operator to
pass arguments by name.

### Function Definitions
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

### Pattern Match Bindings
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

### Extension Methods
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

#### Method Syntax
This syntax for extension methods works as follows:

- The target of the method syntax (left argument to `.`) defines the type on
  which the extension method is created.
- An implicit `this` argument is inserted with that type at the start of the
  arguments list.
- All arguments are desugared to lambda arguments.

```ruby
My_Type.method_name a b c = ...
```

#### Function Syntax
This syntax for extension methods works as follows:

- The `this` argument type is used to define the type on which the extension
  method is created.
- `this` and all remaining arguments are desugared to lambda arguments.

```ruby
method_name (this : My_Type) a b c = ...
```

### Top-Level Assignments
In order to aid with disambiguation, any binding made in the root scope without
an explicit target is implicitly defined on a type representing the current
module. For example, a binding `main = ...` is implicitly `here.main = ...`.

This works as follows:

- All non-extension methods defined at the top level are augmented with an
  implicit first parameter `here`.
- They are callable by `name` if not ambiguous, but can be disambiguated by
  using `here.name` where necessary.

## Types and Type Signatures
Enso is a statically typed language, meaning that every variable is associated
with information about the possible values it can take. In Enso, the type
language is the same as the term language, with no artificial separation. For
more information on the type system, please see the
[types](../../types/design/types.md) design document.

This section will refer to terminology that has not been defined in _this_
document. This is as this document is a specification rather than a guide, and
it is expected that you have read the above-linked document on the type-system
design as well.

Additionally, this document will colloquially refer to the left and right hand
sides of the type ascription operator `:` as the 'term' and 'type' levels,
respectively. In reality, there is no separation between the two in Enso, but it
is a useful way of thinking about things when discussing type signatures.

### Type Signatures
Enso allows users to provide explicit type signatures for values through use of
the type ascription operator `:`. The expression `a : b` says that the value
`a` has the type `b` attributed to it.

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

### Operations on Types
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

### Type Definitions
Types in Enso are defined by using the `type` reserved name. This works in a
context-dependent manner that is discussed properly in the
[type system design document](../../types/design/types.md), but is summarised
briefly below.

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

#### Visibility and Access Modifiers
While we don't usually like making things private in a programming language, it
sometimes the case that it is necessary to indicate that certain fields should
not be touched (as this might break invariants and such like). To this end, we
propose an explicit mechanism for access modification that works as follows:

- We provide explicit access modifiers that, at the definition site, start an
  indented block. These are `private` and `unsafe`.
- All members in the block have the access modifier attributed to them.
- By default, accessing any member under an access modifier will be an error.
- To use members under an access modifier, you use the syntax `use <mod>`, where
  `<mod>` is a modifier. This syntax 'takes' an expression, including blocks,
  within which the user may access members qualified by the modifier `<mod>`.

While `private` works as you might expect, coming from other languages, the
`unsafe` annotation has additional restrictions:

- It must be explicitly imported from `Std.Unsafe`.
- When you use `unsafe`, you must write a documentation comment on its usage
  that contains a section `Safety` that describes why this usage of unsafe is
  valid.

## Macros
Enso provides a macro system that allows users to perform AST to AST
transformations on the provided pieces of code. While many languages' macros
provide their users with access to the compilation and type-checking phases
(scala, for example), there are a few reasons that we don't want to:

- The power of a dependently-typed language obviates the need for the ability to
  manipulate types at compile time.
- Syntactic macros are far more predictable than those that can perform type
  manipulation and compute values.
- We do not want to introduce a metaprogramming system that is too complex.

> The actionables for this section are:
>
> - Fully specify the macro system.
> - Fully specify the interactions between the parser-based macro system and the
>   runtime.

### Annotations
Much like annotations on the JVM, annotations in Enso are tags that perform a
purely syntactic transformation on the entity to which they are applied. The
implementation of this requires both parser changes and support for
user-defined macros, but for now it would be possible to work only with a set
of hard-coded annotation macros.

Annotations can be arbitrarily nested, so a set of annotation macros become
implicitly nested inside each other:

```ruby
@derive Eq Debug
@make_magic
type Maybe a
    use Nothing
    type Just
```

The above example is logically translated to:

```ruby
derive Eq Debug
    make_magic
        type Maybe a
            use Nothing
            type Just (value : a)
```

In the presence of annotations and macros, it becomes more and more important
that we are able to reserve words such as `type` to ensure that users can
always have a good sense of what the most common constructs in the language
mean, rather than allowing them to be overridden outside of the stdlib.

#### Automatic Deriving
In order to make the language easier to debug, we have all types automatically
derive an interface `DebugShow`. This interface provides a function that will
print all the significant information about the value (e.g. locations, types,
source information, etc).

## Top-Level Syntax
Like almost all statically-typed programming languages, the top-level of an Enso
file is non-executable. The top level may contain the following constructs:

- Type definitions (both complex and simple)
- Method definitions
- Function definitions

> The actionables for this section are as follows:
>
> - Fully specify the top-level syntax for Enso.

### Main
The entry point for an Enso program is defined in a special top-level binding
called `main` in the file `Main.enso`. However, we also provide for a scripting
workflow in the form of `enso run`, which will look for a definition of `main`
in the file it is provided.

```ruby
main = IO.println "Hello, World!"
```

### Against Top-Level Evaluation
At points during Enso's development it was up for debate as to whether we wanted
the language to have an executable top-level (akin to scripting languages like
Python). In order to make a decision, we listed the following use cases, and the
corresponding analysis is provided here for posterity.

|  Label  | Meaning |
| --------| ------- |
| `[?,_]` | We don't know how to implement it, but it may be possible.
| `[-,_]` | Not possible to implement using purely syntactic macros.
| `[M,_]` | Possible to implement using purely syntactic macros.
| `[_,H]` | High priority. This will be used often.
| `[_,M]` | Medium priority. This will be used with a medium frequency.
| `[_,L]` | Low priority. Nice to have, but we can likely live without it.
| `[_,!]` | Something that we never want to have in the language.

The use-cases we have considered are as follows:

|  Label  | Description |
| ------- | ----------- |
| `[-,L]` | Creating top-level constructs in `IO`, such as `IORef`. This is, in general, considered to be bad style, but can sometimes be useful. |
| `[-,L]` | Using enso files like python is able to be for scripting work. The ability to write constructs at the top-level and just evaluate them. |
| `[M,H]` | The ability to generate structures and / types for a dataframe at compilation time, or the automatic generation of an API for a library. A key recognition is that dependent types and type-level execution replace much of the need to be able to query the type-checker and runtime while writing a syntactic macro. |
| `[M,H]` | Static metaprogramming (transformations from `AST -> AST`) to let users generate types and functions based on existing AST. There is the potential to want to be able to evaluate actions in `IO` while doing this, but it may not be necessary. |
| `[-,!]` | Dynamic metaprogramming to let users mutate program state at runtime (e.g. changing atom shapes, function definitions), also known as 'monkey patching'. This is not something we want in the language, but we do perhaps want the ability to do so on values of type `Dynamic`. |
| `[M,H]` | 'Remembering' things when compiling a file, such as remembering all structures marked by an `AST` annotation. An example use case for a mechanism like this is to generate pattern matches for all possible `AST` types. This can be done by letting macros write to a per-file peristent block of storage that could be serialised during precompilation. |
| `[M,H]` | Grouping of macros (e.g. `deriveAll = derive Ord Debug Show`). This can be easily handled by doing discovery on functions used as macros, and treating it as a macro as well. |
| `[?,M]` | Method-missing magic, akin to ruby. This is likely able to be handled using other, existing language mechanisms. |

In summary and when considering the above use-cases, it seems that there is
little need for top-level expression evaluation in Enso. We can support all of
the above-listed important use-cases using syntactic (`AST -> AST`) macros,
while allowing for top-level evaluation would enable users to write a lot of
overly-magical code, which will always be a code-smell.

Syntactic macros, however, do not easily support a scripting workflow, but the
solution to this problem is simple. We can just provide an `enso run <file>`
command which will search for and execute the `main` function in the provided
file.

## Functions
Enso is a purely-functional programming language. As a result it has support for
[first-class and higher-order functions](https://en.wikipedia.org/wiki/Functional_programming#First-class_and_higher-order_functions),
meaning that you can pass functions as arguments to other functions, return
functions from functions, assign them to variables, store them in data
structures and so on.

Functions in Enso are curried by default, meaning that all functions are
actually functions in one argument, but may return functions accepting further
arguments.

### Lambdas
The most primitive non-atom construct in Enso is the lambda. This is an
anonymous function in one argument. A lambda is defined using the `->` operator,
where the left hand side is an argument, and the right hand side is the body of
the function (containing arbitrary code).

Some functional languages such as Haskell allow for the definition of a lambda
with multiple arguments, but in Enso the type signature use of `-> `and the
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

### Defining Functions
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

### Methods
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

### Calling Functions and UCS
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

### Code Blocks
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

### Operators
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

#### Precedence
Operator precedence in Enso is a collection of rules that reflect conventions
about which operations to perform first in order to evaluate a given expression
that contains operators. However, operator precedence in Enso differs from
many other programming languages.

- Precedence is not set at fixed levels, but is instead defined in relation to
  the precedence of other operators.
- Precedence of an operator in Enso depends on whether a particular operator is
  surrounded by spaces or not. This means that the precedence of _any_ operator
  not surrounded by spaces is always higher than the precedence of any operator
  surrounded by spaces. The only exception to this rule is the `,` operator,
  which retains the same precedence level regardless of whether it is surrounded
  by spaces or not.

This space-based precedence may seem strange coming from other languages, but
it allows for writing _far_ cleaner code than other functional languages. This
is best demonstrated by example. Consider the following code:

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

#### Sections
An operator section is a nice shorthand for partially applying an operator. It
works as follows.

- Where an argument is not applied to an operator, the missing argument is
  replaced by an implicit `_`.
- The application is then translated based upon the rules for
  [underscore arguments](#underscore-arguments) described later.
- The whitespace-based precedence rules discussed above also apply to operator
  sections.

### Mixfix Functions
A mixfix function is a function that is made up of multiple sections. They are
defined using a special syntax, and operate as follows:

- They are defined using a 'split snake case'. The first section is written as
  normal, but subsequent sections are prefixed by an underscore (`if c _then a`,
  for example).
- The layout rules applied to mixfix functions operate as if each section was a
  separate operator, allowing you to write an indented block of code after each
  section.

Probably the best-known example of a mixfix function is `if-then-else`, which
is indeed defined in the Enso standard library.

```ruby
if foo == bar then frob else
    thing1
    thing2
```

## Function Arguments
One of the biggest usability innovations of Enso is the set of argument types
that it supports. The combination of named and defaulted arguments with a
curried language creates a tool in which it is very clear to express even
complex APIs.

### Positional Arguments
Much like most programming languages, functions in Enso can be called with their
arguments provided positionally. This is the simple case that everybody is
familiar with.

### Named Arguments
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

### Defaulted Arguments
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

### Optional Arguments
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

### Splats Arguments
Enso provides users with the ability to define variadic functions, or _splats_
functions in our terminology. These are very useful for defining expressive APIs
and flexible code.

- These work for both positional and keyword arguments.
- They are defined using the syntax `name...`, where `name` is an arbitrary
  argument name.

### Type Applications
There are sometimes cases where the user wants to explicitly refine the type of
an argument at the _call_ site of a function. This can be useful for debugging,
and for writing ad-hoc code. Much like the named-arguments in applications
above, Enso also provides a syntax for refining types at the application site.

- To refine an argument type by name at the application site, use the `:=`
  operator (e.g. `arg_name := T`).
- This _will_ be type-checked by the compiler, and so `T` must be a valid
  subtype for the type inferred for (or defined for) the function being called.

### Underscore Arguments
Enso provides the `_` argument as a quick way to create a lambda from a function
call. It obeys the following rules.

- Replacing any function argument with `_` will create a lambda that accepts an
  argument and passes it in the place of the underscore. All other function
  arguments are applied as normal.
- This works both by name and positionally.
- When a function is provided multiple `_` arguments, they are desugared left to
  right as the arguments would be applied to the function definition, creating
  nested lambdas.

## Field Access
Enso provides multiple ways for users to access data from their types. It has
the old functional stalwart of pattern matching, but it also has an inbuilt
notion of accessors based on lenses.

### Pattern Matching
Pattern matching in Enso works similarly to as you would expect in various other
functional languages. Typing information is _always_ refined in the branches of
a case expression, which interacts well with dependent typing and type-term
unification. There are a few main ways you can pattern match:

1.  **Positional Matching:** Matching on the scrutinee by structure. This works
    both for atoms and typesets (for typesets it is a subsumption judgement).

    ```ruby
    type Vector a
      V2 x:a y:a
      V3 x:a y:a z:a

    v = Vector.V3 x y z

    case v of
      Vector.V3 x y z -> print x
    ```

2.  **Type Matching:** Matching purely by the types involved, and not matching
    on structure.

    ```ruby
    case v of
      Vector.V3 -> print v.x
    ```

3.  **Name Matching on Labels:** Matching on the labels defined within a type
    for both atoms and typesets, with renaming.

    ```ruby
    case v of
      Vector.V3 {x y} -> print x
      {x}             -> print x
    ```

4.  **Naming Scrutinees in Branches:** Ascribing a name of a scrutinee is done
    using the standard typing judgement. This works due to the type-term
    unification present in Enso.

    ```ruby
    case _ of
      v : Vector.V3 -> print v,x
    ```

> The actionables for this section :
>
> - Refine the syntax for the name-based case.
> - Provide code examples for why the renaming use-case is important (e.g.
>   cases where there are clashing field names).
> - Function-resolution matching.

#### The Underscore in Pattern Matching
An underscore `_` passed as an argument to a syntactic pattern does not behave
like the function argument shorthand. Instead it acts as a positional match that
is given no name.

### Projections / Lenses
Unlike the simple accessors defined by most programming language, Enso's
accessors are far more powerful. This is because they are based on lenses.

- Field accessors are standard lenses. As such, they can be used for both the
  getting and setting of properties.
- As a lens (e.g. `.field`) is a first-class function, they can be curried and
  passed around like any other function.

```ruby
type Engine
    type Combustion
        power:          Int
        cylinder_count: Int

    type Electric
        power:   Int
        is_blue: Bool

type Vehicle
    type Car
        color:     String
        max_speed: Int
        engine:    Engine

    type Bike
        color: String

type Person
    type Cons
        name:    String
        vehicle: Vehicle

main =
  p1 = Person.Cons "Joe" (Vehicle.Car 'pink' 300 (Engine.Combustion 500 8))
  print $ p1.name                   # -> Joe
  print $ p1.vehicle.color          # -> pink
  print $ p1.vehicle.max_speed      # -> Some 300
  print $ p1.vehicle.engine.power   # -> Some 500
  print $ p1.vehicle.engine.is_blue # -> None
  p1.vehicle.color     = 'red'      # OK
  p1.vehicle.max_speed = 310        # FAIL: security reasons. Allowing this
                                    #       in Haskell was the worst decision
                                    #       ever. After refactoring it
                                    #       silently does nothing there.

  p2 = p1.vehicle.max_speed    ?= 310 # OK
  p3 = p1.vehicle.engine.power  = 510 # FAIL
  p4 = p1.vehicle.engine.power ?= 510 # OK

  lens_name      = .name
  lens_color     = .vehicle.color
  lens_max_speed = .vehicle.max_speed
  lens_power     = .vehincle.engine.power

  ## Function like usage:
  print $ lens_name      p1
  print $ lens_color     p1
  print $ lens_max_speed p1
  print $ lens_power     p1

  p1 . at lens_name = ... # OK
```

> The actionables for this section are:
>
> - Fix the example above. It isn't correct.

## Comments
Enso supports a variety of types of comments:

- **Disable Comments:** TODO
- **Documentation Comments:** Documentation comments allow users to attach
  documentation to language constructs. This documentation can later be rendered
  to produce user-accessible HTML documentation, similar to tools included with
  most programming languages.

> The actionables for this section are:
>
> - Solidify exactly how each type of comment should behave.

### Documentation Comments
Documentation comments allow users to attach documentation to Enso language
constructs that can later be displayed in a rich format for users of the API.
Such comments are automatically connected to the language construct, and can be
used both for displaying static documentation as well as providing dynamic help
to the user in Enso Studio itself.

The tool that generates this documentation aims to be fairly robust, and tries
to assign produce sensible results even if the user makes a mistake. Such
mistakes will be highlighted to the user.

The documentation syntax is broken down into the following elements.

#### Tags
Tags allow users to annotate their construct with information about its usage
state. The documentation syntax supports the following tags:

- `DEPRECATED`: Used for constructs that should no longer be used and that may
  be removed in the future.
- `MODIFIED`: Used for constructs that have had their behaviour change after a
  certain version of the library.
- `ADDED`: Used to describe when a given construct was added to the library.
- `UPCOMING`: Used to describe constructs that will be added in future versions
  of the library.
- `REMOVED`: Used to describe constructs that have been removed and are no
  longer functional.

Tags are added at the _top_ of the documentation block, and may also be
accompanied by a description. This description directly follows the tag
declaration with one space.

```ruby
# DEPRECATED Use `seeFoo` instead
```

If the user provides an unknown tag the documentation will contain that tag, but
it will be undefined.

#### Sections
Documentation comments can be broken up into sections, with each section
delineated by significant whitespace.

The first section that the user writes will be attributed to the 'synopsis' part
of the documentation, and the second section becomes the 'body'. They should be
used as follows:

- **Synopsis:** A brief summary of the function's behaviour.
- **Body:** More in-depth documentation where details of usage can be provided.

Sections may also have a title. If the whitespace before the section is _three_
newlines instead of _two_, then the first line of the section will be understood
to be a title.

The body can be broken down into multiple sections, with support for four
different types of section:

- **Raw:** A block of text, delineated purely by two blank lines before it.
- **Important:** A block of text describing important details about the
  functionality of the construct. To create an important section, prefix the
  title with `!`.
- **Info:** An information section that should be used to provide non-crucial
  details about the construct's usage. To create an info section, prefix the
  title with `?`.
- **Example:** For providing usage examples to the user. To create an example
  section, prefix the title with `>`.

#### Links
Users are able to embed links and images into their documentation. These links
can serve to provide access to external resources or demonstrations, and also
link between various program constructs.

- **URLs:** `[Link title](URI)`
- **Images:** `![Image name](URI)`

Linked images are rendered in the generated documentation, and URLs will be
displayed like standard hyperlinks.

> The actionables for this section are:
>
> - We probably want a construct that lets you reference other API constructs.

#### Lists
The Enso documentation syntax also supports ordered and unordered lists. These
can be nested, and the nesting may swap the types. Both list types must be
intended some multiple of 2 spaces from the left margin of the documentation
comment.

- **Unordered:** List items are indicated by the `-` character.
- **Ordered:** List items are indicated by the `*` character.

To nest a list inside another list, add another 2-character indent to the nested
list.

#### Code
The Enso documentation syntax allows users to write code that will be displayed
as code rather than prose. It supports two types of code.

- **Inline Code:** Text enclosed in `` ` `` will be formatted as inline code.
- **Multi-Line Code:** A block that is indented from the baseline of the current
  section will be formatted as a code block.

#### Text Formatting
Enso's documentation syntax also supports some basic syntax for adding rich
text formatting to the documentation.

- **Italics:** Enclosing text in `_` (e.g. `_Italics_`).
- **Bold:** Enclosing text in `*` (e.g. `*Bold*`).
- **Strikethrough:** Enclosing text in `~` (e.g. `~Strikethrough~`).

These syntaxes may be combined, and the order of opening need not equal the
order of closing. However, if the formatting syntaxes are not closed, this will
result in an error.
