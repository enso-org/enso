---
layout: developer-doc
title: Diagnostics
category: semantics
tags: [semantics, diagnostics, runtime]
order: 11
---

# Encapsulation

_Encapsulation_ is the system of hiding certain internal **entities** (modules,
types, methods, constructors, fields) in one project/library from other
projects/libraries. This document is an excerpt from the discussion held at
https://github.com/orgs/enso-org/discussions/7088.

## Requirements

- Be able to hide an entity on demand. By hiding, we mean that the entity cannot
  be directly imported, and that it cannot be used via FQN.
  - i.e. an entity shall be hidden both during compile time (project
    compilation), and during runtime.
  - Entity being hidden at runtime implies that it does not have any entry in
    the Suggestion database, therefore, no entry in the Component browser.
- Be able to import all public symbols from a library with
  `from Library import all`.
- Be able to import a selected set of symbols from a library with
  `from Library import Symbol_1, Symbol_2, ...`.
- Import a public symbol directly with
  `import Library.Public_Module.Public_Type`.
- Use a public symbol via FQN: `Library.Public_Module.Public_Type`.

## Implementation

Let's introduce a `private` keyword. By prepending (syntax rules discussed
below) `private` keyword` to an entity, we declare it as **project private**. A
project-private entity is an entity that can be imported and used in the same
project, but cannot be imported nor used in different projects. Note that it is
not desirable to declare the entities as _module private_, as that would be too
restrictive, and would prevent library authors using the entity within the
project.

From now on, let's consider _project-private_ and _private_ synonymous, and
**public** as an entity that is not private.

## Syntax

All the entities, except modules, shall be declared private by prepending them
with `private` keyword. Declaring a module as private shall be done be writing
the `private` keyword at the very beginning of the module, before all the import
statements, ignoring all the comments before. Fields cannot have `private`
keyword, only constructors. Types cannot have `private` keyword as well - only
methods and constructors.

## Semantics

### Modules

Modules can be specified as private. Private modules cannot be imported from
other projects. Private modules can be imported from the same project.

A hierarchy of submodules can mix public and private modules. By _hierarchy_, we
mean a parent-child relationship between modules. It does not make sense to
create a public submodule of a private module and export it, but it is allowed.
Note that this is because of current limitations of the implementation, this
might be more strict in the future.

### Types

_Types cannot be specified as private_, only constructors and methods. A type
must have all the constructors private or all the constructors public. This is
to prevent a situation when a pattern match can be done on public constructor,
but cannot be done on a private constructor from a different project. Mixing
public and private constructors in a single type is a compilation error. A type
with all constructors public is called an _open_ type and a type with all
constructors private is called a _closed_ type.

Methods on types (or on modules) can be specified private. To check whether a
private method is accessed only from within the same project, a runtime check
must be performed, as this cannot be checked during the compilation.

## Example

Lib/src/Pub_Type.enso:

```
type Pub_Type
  Constructor field
  private priv_method self = ...
  pub_method self = self.field.to_text

private type Priv_Type
```

Lib/src/Methods.enso:

```
pub_stat_method x y = x + y
private priv_stat_method x y = x - y
```

Lib/src/Internal/Helpers.enso:

```
# Mark the whole module as private
private

# OK to import private types in the same project
import project.Pub_Type.Priv_Type
```

Lib/src/Main.enso:

```
import project.Pub_Type.Pub_Type
export project.Pub_Type.Pub_Type

import project.Pub_Type.Priv_Type # OK - we can import private types in the same project.
export project.Pub_Type.Priv_Type # Failes at compile time - re-exporting private types is forbidden.
```

tmp.enso:

```
from Lib import Pub_Type
import Lib.Pub_Type.Priv_Type # Fails during compilation
import Lib.Methods

main =
  # This constructor is not private, we can use it here.
  obj = Pub_Type.Constructor field=42
  obj.field # OK - Constructor is public, therefore, field is public
  obj.priv_method # Runtime failure - priv_method is private
  Pub_Type.priv_method self=obj # Runtime failure
  obj.pub_method # OK

  Lib.Pub_Type.Priv_Type # Fails at runtime - accessing private types via FQN is forbidden

  Methods.pub_stat_method 1 2 # OK
  Methods.priv_stat_method # Fails at runtime
```

## Checks

There shall be two checks. One check during **compilation**, that can be
implemented as a separate compiler pass, and that will ensure that no private
entity is _re-exported_ (exported from a module that is different from the
module inside which the entity is defined) and that for every type it holds that
either all the constructors are public or all the constructors are private

The second check shall be done during the **method/name resolution** step. This
step happens at runtime, before a method is called. After the method is
resolved, there shall be no further checks, so that the peak performance is not
affected.

## Performance impact

The performance hit on compilation time is minimal, as there are already dozens
of different compiler passes. Moreover, in the new compiler pass we shall check
only imports and exports statements, no other IR.

The performance hit on runtime, during method resolution, is minimal as well,
because it can be as easy as additional lookup in a hash map. Peak performance
will not be affected at all, as there are no further checks after method
resolution.

## Overcoming encapsulation

Sometimes it is useful to be able to access internal entities. Testing is the
most obvious example. Let's introduce a new CLI flag to the Engine launcher
called `--disable-private-check`, which will disable all the private checks
during compilation and method resolution.

## Other notes

- A private module implies that all the entities defined within are private
- A private type implies that all the constructors, methods and fields defined
  within are private
- A private constructor implies private fields defined in that constructor.
