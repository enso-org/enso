---
layout: developer-doc
title: AST
category: parser
tags: [parser, ast]
order: 9
---

# AST

The parser AST describes the high-level syntactic structure of Enso, as well as
containing robust and descriptive parser errors directly in the AST.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Functionality](#functionality)
- [Generation](#generation)

<!-- /MarkdownTOC -->

## Functionality

The parser AST needs to account for the following:

- A single `Name` type, removing the distinction between different names found
  in the [lexer](./lexer.md). This should provide functions `is_var`, `is_opr`,
  and `is_ref`.
- It should contain all of the language constructs that may appear in Enso's
  source.
- It should contain `Invalid` nodes, but these should be given a descriptive
  error as to _why_ the construct is invalid.
- It should also contain `Ambiguous` nodes, where a macro cannot be resolved in
  an unambiguous fashion.

Each node should contain:

- An identifier, attributed to it from the ID map.
- The start source position of the node, and the length (span) of the node.

> The actionables for this section are:
>
> - Flesh out the design for the AST based on the requirements of the various
>   parser phases.

## Generation

The single source of truth for the AST is its Rust implementation. Therefore, in
order to not get out of sync, the Scala AST implementation is generated during
compilation directly from the Rust ast source file.

The command for generating the Scala ast and storing it in the file
`foo/ast.scala` is following:
`cargo run -p ast -- --generate-scala-ast foo/ast.scala`.

Since there isn't 1:1 mapping between Rust and Scala, only a subset of Rust's
structures is supported. These are follows.

##### Primitives

```
u32   | i32   | u16 | i16 | i8 => Int,
usize | isize | u64 | i64      => Long,
u8                             => Byte,
char                           => Char,
Vec                            => Vector,
Uuid                           => UUID,
```

> Note: It is assumed, that Enso runs on 64bit platforms. Therefore, `usize` and
> `isize` are converted to `Long`.

##### Structures With Named Fields

```
struct Foo<X> { x: X<z::Y>, .. }
```

Is converted into:

```
case class Foo[X](x: X[Z.Y], ..)
```

##### Enums With Named Fields

```
enum Foo{ Bar{x:X}, Baz{y:Y, z:Z} }
```

Is converted into:

```
sealed trait Foo
case class Bar(x:X) extends Foo
case class Baz(y:Y, z:Z) extends Foo
```

##### Enums With One Unnamed Qualified Field

```
enum Enum { Foo(x::Foo), Bar(x::Bar), Baz(y::Baz) }

mod x {
    pub struct Foo { .. }
    pub struct Bar { .. }
}

mod y {
    pub struct Baz { .. }
}
```

Is converted into:

```
sealed trait Enum

object X {
    sealed trait X extends Enum
    case class Foo(..) extends X
    case class Bar(..) extends X
}

object Y {
    sealed trait Y extends Enum
    case class Baz(..) extends Y
}
```

##### Modules

```
mod foo { .. }
```

Is converted into:

```
object Foo { .. }
```

Furthermore, the content of `ast.rs` is wrapped inside additional `object Ast`,
in order to support top level type aliases.

##### Type Aliases

```
type A<X> = B<X,Y>;
```

Is converted into:

```
type A[X] = B[X, Y]
```

Note that in contrast to Rust, Scala doesn't support types outside objects.
