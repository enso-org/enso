---
layout: developer-doc
title: The Enso Macro Syntax
category: syntax
tags: [syntax, macro]
order: 8
---

# The Enso Macro Syntax

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

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Annotations](#annotations)
  - [Annotation Naming](#annotation-naming)
- [Automatic Deriving](#automatic-deriving)

<!-- /MarkdownTOC -->

## Annotations

Much like annotations on the JVM, annotations in Enso are tags that perform a
purely syntactic transformation on the entity to which they are applied. The
implementation of this requires both parser changes and support for user-defined
macros, but for now it would be possible to work only with a set of hard-coded
annotation macros.

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
that we are able to reserve words such as `type` to ensure that users can always
have a good sense of what the most common constructs in the language mean,
rather than allowing them to be overridden outside of the stdlib.

### Annotation Naming

The naming of annotations follows the standard rules that Enso uses for naming
its [identifiers](./naming.md#naming-constructs). This means that they can be in
both referent or variable form as the annotation head is _not_ a
[pattern context](./naming.md#pattern-contexts).

## Automatic Deriving

In order to make the language easier to debug, we have all types automatically
derive an interface `DebugShow`. This interface provides a function that will
print all the significant information about the value (e.g. locations, types,
source information, etc).
