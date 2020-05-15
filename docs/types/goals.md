---
layout: developer-doc
title: Goals for the Enso Type System
category: types
tags: [types, goals, design]
order: 1
---

# Goals for the Enso Type System
In our design for Enso, we firmly believe that the type system should be able to
aid the user in writing correct programs, far and above anything else. However,
with so much of our targeted user-base being significantly non-technical, it
needs to be as unobtrusive as possible.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [High-Level Goals](#high-level-goals)

<!-- /MarkdownTOC -->

## High-Level Goals
The high-level goals for the Enso type system are as follows:

- Inference should have maximal power. We want users to be _forced_ to write
  type annotations in as few situations as possible. This means that, ideally,
  we are able to infer higher-rank types and make impredicative instantiations
  without annotations.
- Error messages must be informative. This is usually down to the details of the
  implementation, but we'd rather not employ an algorithm that discards
  contextual information that would be useful for crafting useful errors.
- Dependent types are a big boon for safety in programming languages, allowing
  the users that _want to_ to express additional properties of their programs
  in their types. We would like to introduce dependent types in future, but
  would welcome insight on whether it is perhaps easier to do so from the get
  go. If doing so, we would prefer to go with `Type : Type`.
- Our aim is to create a powerful type system to support development, rather
  than turn Enso into a research language. We want users to be able to add
  safety gradually.
