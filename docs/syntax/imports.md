---
layout: developer-doc
title: Imports and Exports
category: syntax
tags: [syntax, imports, modules]
order: 4
---

# Imports and Exports

In order to properly modularise and work with Enso code, the language provides a
robust mechanism for importing code from modules, and also re-exporting that
code from modules.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Import Syntax](#import-syntax)
  - [Qualified Imports](#qualified-imports)
  - [Unqualified Imports](#unqualified-imports)
- [Export Syntax](#export-syntax)
  - [Qualified Exports](#qualified-exports)
  - [Unqualified Exports](#unqualified-exports)
  - [Visibility of Export Bindings](#visibility-of-export-bindings)

<!-- /MarkdownTOC -->

## Import Syntax

There are two main ways of importing a module into the current scope.

### Qualified Imports

These imports consist of the word `import` followed by a qualified name of a
module. This can by optionally followed by the `as` word, and a referent name of
the module as it should be visible in the importing scope.

The only name brought into scope by such an import is the name of the module (or
the name provided after the `as` keyword, if provided).

### Unqualified Imports

Unqualified imports are broken up into three main categories:

1. **Unrestricted Imports:** These import all symbols from the module into the
   current scope. They consist of the keyword `from`, followed by a qualified
   module name, followed by an optional rename part (using the `as` keyword),
   then the keywords `import all`. For example:
   ```
   from Base.Data.List as Builtin_List import all
   ```
2. **Restricted Imports:** These import a specified set of names for use as
   automatically resolved referent names. They consist of the keyword `from`,
   followed by a qualified module name (with optional `as`-rename), then the
   word `import` followed by a coma-separated list of referent names to be
   imported. For example:
   ```
   from Base.Data.List import Cons, Nil
   ```
3. **Hiding Imports:** These are the inverse of restricted imports, and import
   _all_ symbosl other than the named ones. They consist of the `from` keyword,
   followed by a qualified module name (with optional `as`-rename), then the
   words `import all hiding`, followed by a coma-separated list of referent
   names to be excluded from the import. For example:
   ```
   from Base.Data.List import all hiding Cons, Nil
   ```

Imports in Enso _may_ introduce ambiguous symbols, but this is not an error
until one of the ambiguous symbols is _used_ in Enso code.

## Export Syntax

In order to allow for easy composition and aggregation of code, Enso provides
its users with a mechanism to _export_ imported elements from modules. They
appear in Enso as follows:

### Qualified Exports

These exports consist of the word `export` followed by a qualified name of a
module. This can by optionally followed by the `as` word, and a referent name of
the module as it should be visible in the exporting scope.

The only name brought into scope by such an export is the name of the module (or
the name provided after the `as` keyword, if provided).

### Unqualified Exports

Unqualified exports are broken up into three main categories:

1. **Unrestricted Exports:** These export all symbols from the module into the
   current scope. They consist of the keyword `from`, followed by a qualified
   module name, followed by an optional rename part (using the `as` keyword),
   then the keywords `export all`. For example:
   ```
   from Base.Data.List as Builtin_List export all
   ```
2. **Restricted Exports:** These export a specified set of names, behaving as
   though they were redefined in the current scope. They consist of the keyword
   `from`, followed by a qualified module name (with optional `as`-rename), then
   the word `export` followed by a coma-separated list of names to be exported.
   For example:
   ```
   from Base.Data.List export Cons, Nil, from_vector
   ```
3. **Hiding Exports:** These are the inverse of restricted exports, and export
   _all_ symbols other than the named ones. They consist of the `from` keyword,
   followed by a qualified module name (with optional `as`-rename), then the
   words `export all hiding`, followed by a coma-separated list of names to be
   excluded from the export. For example:
   ```
   from Base.Data.List export all hiding from_vector, Nil
   ```

In essence, an export allows the user to "paste" the contents of the module
being exported into the module declaring the export. This means that exports
that create name clashes must be resolved at the _export_ site.

### Visibility of Export Bindings

Bindings exported from a module `X` are available in an identical fashion to
bindings that are _defined_ in the module `X`.
