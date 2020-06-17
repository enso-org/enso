---
layout: developer-doc
title: Modules
category: types
tags: [types, modules]
order: 7
---

# Modules
With such a flexible type system in Enso, the need for making modules
first-class is obviated. Instead, a module is very much its own entity, being
simply a container for bindings (whether they be functions, methods, atoms, or
more generic typesets).

> The actionables for this section are:
>
> - Characterise modules in more depth as we need them.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Resolving Name Clashes](#resolving-name-clashes)
- [Scoping and Imports](#scoping-and-imports)

<!-- /MarkdownTOC -->

## Resolving Name Clashes
Enso modules employ the following rules in order to avoid name clashes:

- Where the module name clashes with a member contained in the module, the
  member is preferred. If you need the module you must import it qualified under
  another name.
- We provide the alias `here` as a way to access the name of the current module.

## Scoping and Imports
To use the contents of a module we need a way to bring them into scope. Like
most languages, Enso provides an _import_ mechanism for this. Enso has four
different kinds of imports that may be combined freely, all of which take a
module path as their first argument.

1.  **Unqualified Imports:** These import all symbols from the module into the
    current scope (`import M`).
2.  **Qualified Imports:** These import all symbols from the module into the
    current scope with symbols qualified under a name _different_ from the
    module name (`import M as T`).
3.  **Restricted Imports:** These import only the specific symbols from the
    module into the current scope (`import M only sym1 sym2`).
4.  **Hiding Imports:** These are the inverse of restricted imports, and import
    _all_ symbols other than the named ones into the current scope
    (`import M hiding sym1 sym2`),

Imports may introduce ambiguous symbols, but this is not an error until one of
the ambiguous symbols is used in user code.

When importing a module `X` into the current module `Y`, the bindings in `X`
become available in `Y` (modified by the import type). However, these bindings
are _not_ available in `Y` externally. This means that we need a re-export
mechanism. Similarly to imports, this has four kinds, all of which take a module
path as their first argument, and all of which _may_ introduce the module it
exports into scope (if it is not already imported).

1.  **Unqualified Exports:** These export all symbols from the module as if they
    were defined in the exporting module (`export X`).
2.  **Qualified Exports:** These export all symbols from the module as if they
    were defined in another module accessible in the exporting module
    (`export X as Y`).
3.  **Restricted Exports:** These export only the specified symbols from the
    module as if they were defined in the exporting module (`export X only sym`)
4.  **Hiding Exports:** These export all symbols from the module except those
    explicitly specified (`export X hiding sym1 sym2`).

Exports effectively act to 'paste' the contents of the exported module into the
module declaring the export. This means that exports that create name clashes
must be resolved at the source.

> The actionables for this section are:
>
> - Are we _really, really_ sure we want unqualified by default?
> - Think about how to handle imports properly in the type checker. What, if
>   they have any, are the impacts of imports on inference and checking?
