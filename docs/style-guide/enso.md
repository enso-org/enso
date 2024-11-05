---
layout: style-guide
title: Enso Style Guide
category: style-guide
tags: [style-guide]
order: 7
---

# Enso Style Guide

This style guide is aimed at developers writing Enso Libraries Code in the Enso
language. It is not aimed at users writing Enso code via the graphical editor.

# Encapsulation

Encapsulation allows developers to present a consistent interface that is
independent of its internal implementation. It is the foundation of reuseable
"pieces" of code and funcationality. It allows other users (both internal and
external) effectivley re-use components of your code.

# Types of Code Components in the Enso Langauge

In order to be able to talk about how we encapsulate components of code we first
need to define what those components are. Enso has four levels of code
components that can be built and shared with other developers. They are:

- Libaries
- Modules
- Types
- Methods

TODO - defintion of each of these.

# Public verus Private

Now we have defined our components we need to look at how we define our public
APIs for each of these and how we hide our private immplementations.

## Public

By default the Enso langauge is a very public one. If you don't use any of the
private keywords and/or conventions described belown then everything is public.
This is bad for encapsulation as other users and developers trying to re-use the
code you wrote won't have a clean view of your code's API.

## Types of Private

There are a number of different ways to mark things Private in Enso

### Hidden from the Graphical Editor Private

This is acheived with the special PRIVATE keyword cotained in a comment above a
type, constructor or method

e.g.

```
## PRIVATE
type Random_Generator
```

or

```
## PRIVATE
Value (random_instance_holder:RandomInstanceHolder)
```

or

```
## PRIVATE
set_seed self seed = self.random_instance_holder.setSeed seed
```

This prevents a type, construstor ot method being used from the Component
Browser inside of the graphical editor.

For other Enso code users it does not prevent its use and these are part of your
components public API.

### Library Private

This is a langauge keyword `private` that can be used on modules, types, methods
and constructors.

e.g.

TODO Examples.

This keyword makes whatever it marks private at the library component level and
so is useful in defining the public and private parts of an Enso library. It is
enforced by the compliler, so has a strong guarentee of enforcing the Public and
Private parts of a library.

However for other developers working inside of the same library it does not
prevent their use and these components are still part of your internal public
API.

### Module Private

At the time of writing, the above is all the support the langauge has built-in
for encapsulation. However as libary developers it is still important that we
define Public APIs and Private implentations of our internal modules.

Which brings us to module private. This is "privacy by convention" and works by
prefixing a method name with an underscore.

e.g.

```
private _my_private_method
```

Methods named like so are "Module Private". That is to say they should only be
used within the module that they are defined in, and never from another module.

Only free methods should be named as such, and not methods on types.

Generally they should also be marked as Library Private using the ``private`
keyword.

### Type Private

We do not have the concept of type private in the form of private methods or
members that you might find in OO style languages. All members are public ouside
of the type. Any methods that you wwant to be private should be pulled out of
the type and made Module Private using the above convention.

# Some Concrete Examples

TODO
