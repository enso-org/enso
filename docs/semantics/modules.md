---
layout: developer-doc
title: Modules
category: semantics
tags: [semantics, modules, runtime]
order: 6
---

# Modules

Modules are the basic way to organize code into smaller, semantically-relevant
parts. The following is the description of the Enso modules semantics.

## Introduction

There's a one-to-one correspondence between source files and modules. Moreover,
a module name is the same as that of the corresponding source file.
Modules are first-class objects, meaning they can be assigned to variables
and passed as function parameters.

## Referring to Modules

Modules can be imported into the current scope by using the `import` statement.
To import a module, one must provide it's full qualified path. A qualified path
of a module is a number of `.`-separated segments, where:
1. The first segment is the name of the project.
2. The following segments are the names of directories between `src` and the
   source file being imported.
3. The last segment is the name of the source file, with the `.enso` extension
   stripped.
   
For example, a qualified name of a module corresponding to the file
`src/Foo/Bar/Baz.enso` in project `Project` is `Project.Foo.Bar.Baz`.

Whenever a module is imported, its name (either the last segment of the
qualified name or the name it was renamed to), becomes available in the
importing scope as a value.

Moreover, the current module is always visible in the scope under its
file-based name and as the special variable `here`.

## Module-Level Methods

A method defined without an explicit `this` reference or with `this` explicitly
set to the current module, becomes a module-level method. Such a method can be
called by passing the module as the `this` parameter. Such methods can also be
called by importing modules, using the imported module as `this`.

## Types

There is no difference from the perspective of access between a `type` 
definition and a module-level method. Both can be accessed in exactly the same
ways both from inside the module and from importing modules.

## Referent Name Resolution

Referent names are resolved specially, by finding the relevant name among
imported modules and items explicitely imported from them. Imported items may
be one of the following: modules, types, module-level methods and polyglot
symbols.

The order of resolving names is as follows:
1. Symbols defined locally.
2. Names of imported modules.
3. Names explicitly imported from modules.

If there's an ambiguity at any level, an error is reported.

## Referent Names in Method Calls

If a referent name is used as the method name in method call syntax, it is not
subject to Referent Name Resolution and is instead treated as though it was not
referent in this position.

## Importing & Exporting Methods

Methods defined both on the module and on other types are always imported and
exported automatically whenever the module is imported or exported. There is
currently no plan to allow hiding methods from imported / re-exported modules,
but this decision will be revised after a sufficiently large corpus of Enso
code exists.

## Export Statement Semantics

An export statement always exports the name of the exported module (possibly
renamed). Moreover, any items explicitly mentioned in a `from` export, become
available as though they were defined in the exporting module.

## Project Main Module

The module `Main` in any project is treated specially, as the main entry-point
to the library. Therefore, using the bare project name in an import statement 
(e.g. `import My_Project`) has the same effect as importing the `Main` module
with the proper rename (e.g. `import My_Project.Main as My_Project`). This
works with any kind of import or export statement.