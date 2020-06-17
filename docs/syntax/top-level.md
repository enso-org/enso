---
layout: developer-doc
title: Enso Top-Level Syntax
category: syntax
tags: [syntax, top-level, file]
order: 9
---

# Enso Top-Level Syntax
Like almost all statically-typed programming languages, the top-level of an Enso
file is non-executable. The top level may contain the following constructs:

- Type definitions (both complex and simple)
- Method definitions
- Function definitions

> The actionables for this section are as follows:
>
> - Fully specify the top-level syntax for Enso.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Main](#main)
- [Against Top-Level Evaluation](#against-top-level-evaluation)

<!-- /MarkdownTOC -->

## Main
The entry point for an Enso program is defined in a special top-level binding
called `main` in the file `Main.enso`. However, we also provide for a scripting
workflow in the form of `enso run`, which will look for a definition of `main`
in the file it is provided.

```ruby
main = IO.println "Hello, World!"
```

## Against Top-Level Evaluation
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
