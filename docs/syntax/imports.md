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
  - [Visibility of Imported Bindings](#visibility-of-imported-bindings)
- [Export Syntax](#export-syntax)
  - [Visibility of Export Bindings](#visibility-of-export-bindings)

<!-- /MarkdownTOC -->

## Import Syntax

Importing a module is a way to bring its contents into scope in the current
module. Imports in Enso appear as follows:

- They start with the `import` keyword.
- The `import` keyword is followed by a module path (e.g. `Base.Vector.Unsafe`).

From there, Enso imports are broken up into four main categories:

1.  **Unqualified Imports:** These import all symbols from the module into the
    current scope, and consist only of the `import` keyword and a module path.
    Imported items are imported accessible without qualification.
2.  **Qualified Imports:** These import all symbols from the module into the
    current scope with symbols qualified under a name _different_ from the
    module name. It consists of the `import` keyword followed by a module path
    followed by the `as` keyword, followed by a referent name.
3.  **Restricted Imports:** These import only the specific symbols from the
    module into the current scope. They consist of the `import` keyword,
    followed by a module path, followed by the `only` keyword, followed by a
    space-separated list of symbols.
4.  **Hiding Imports:** These are the inverse of restricted imports, and import
    _all_ symbols other than the named ones into the current scope. They consist
    of the `import` keyword, followed by a module path, followed by the keyword
    `only`, followed by a space-separated list of symbols.

The qualified import syntax can be combined with the restricted and hiding
import syntaxes.

By way of example, the following code uses a variety of import types:

```ruby
import A                          # unqualified
import B as T                     # qualified
import C only symbol_1 symbol_2   # restricted
import D hiding symbol_1 symbol_2 # hiding
import E as U only symbol_3       # qualified + restricted
import F as V hiding symbol_4     # qualified + hiding
```

Imports in Enso _may_ introduce ambiguous symbols, but this is not an error
until one of the ambiguous symbols is _used_ in Enso code.

### Visibility of Imported Bindings

When importing a module `X` into the current module `Y`, the bindings from `X`
made available by the import (see above) become available in `Y`. However, Enso
does not re-export imported bindings from a module by default, so the imported
bindings from `X` are not visible in a module _importing_ `Y`.

## Export Syntax

In order to allow for easy composition and aggregation of code, Enso provides
its users with a mechanism to _export_ imported elements from modules. They
appear in Enso as follows:

- They start with the `export` keyword.
- The `export` keyword is followed by a module name (e.g. `My_Module`) that is
  available in the current scope.
- The _current_ module is implicitly exported unqualified.

From there, Enso exports are broken up into four main categories:

1.  **Unqualified Exports:** These export all symbols from the named module as
    if they were defined in the exporting module. They consist of the `export`
    keyword, followed by a visible module name.
2.  **Qualified Exports:** These export all symbols from the module as if they
    were defined in a module nested within the exporting module. It consists of
    the `export` keyword, followed by a module name, followed by the `as`
    keyword, followed by another module name.
3.  **Restricted Exports:** These export only the specified symbols from the
    exporting module, as if they were defined in the exporting module. It
    consists of the `export` keyword, followed by a module name, followed by the
    `only` keyword, and then a space-separated list of symbols.
4.  **Hiding Exports:** These export all symbols from the module _except_ those
    explicitly specified. It consists of the `export` keyword, followed by a
    module name, followed by the `hiding` keyword, and then a space-separated
    list of symbols.

The qualified export syntax can be combined with the restricted and hiding
export syntaxes.

By way of example, the following code uses a variety of export types:

```ruby
export A
export B as X
export C only symbol_1
export D hiding symbol_1
export E as Y only symbol_1
export F as Y hiding symbol_1
```

In essence, an export allows the user to `paste` the contents of the module
being exported into the module declaring the export. This means that exports
that create name clashes need to be resolved at the _export_ site.

### Visibility of Export Bindings

Bindings exported from a module `X` are available in an identical fashion to
bindings that are _defined_ in the module `X`.
