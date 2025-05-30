---
layout: developer-doc
title: Comments
category: syntax
tags: [syntax, comments]
order: 13
---

# Comments

Enso supports a variety of types of comments:

- **Disable Comments:** TODO
- **Documentation Comments:** Documentation comments allow users to attach
  documentation to language constructs. This documentation can later be rendered
  to produce user-accessible HTML documentation, similar to tools included with
  most programming languages.

> The actionables for this section are:
>
> - Solidify exactly how each type of comment should behave.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Disable Comments](#disable-comments)
- [Freeze Comments](#freeze-comments)
- [Documentation Comments](#documentation-comments)
  - [Tags](#tags)
  - [Sections](#sections)
  - [Links](#links)
  - [Lists](#lists)
  - [Code](#code)
  - [Text Formatting](#text-formatting)

<!-- /MarkdownTOC -->

## Disable Comments

Disable comments are the standard form of comment seen in a programming language
in that they prevent a given piece of code from executing. In Enso, they are
created by prefixing the expression to disable with the `#` character.

Disable comments in Enso do not have their contents validated, and continue from
the `#` character to the end of the line.

```ruby
x = y + z # here is some commented text
```

Disable comments are _not_ allowed inside textual interpolations.

## Freeze Comments

Freeze comments are a special type of comment used to enable the 'freezing' or
caching of expensive computations in Enso. When used, they cache the result of
an expression, reusing the value instead of recomputing it even if the
underlying data changes.

A portion of code that is frozen has the following properties:

- It is still lexed as if it were code, and validated by the parser to check for
  validity.
- No identifier resolution takes place.

These are very important as they still allow the frozen expression to be
displayed properly in the visual syntax.

> The actionables for this section are:
>
> - Work out what they should look like visually.
> - Work out how best to implement this.

## Documentation Comments

Documentation comments allow users to attach documentation to Enso language
constructs that can later be displayed in a rich format for users of the API.
Such comments are automatically connected to the language construct, and can be
used both for displaying static documentation as well as providing dynamic help
to the user in Enso Studio itself.

A documentation comment in Enso is a _block_, and the block is started with a
double `#` character. The block ends when the indentation returns to the
baseline indentation for that block (see [blocks](./functions.md#code-blocks)
for more information). By way of example:

```
## My documentation comment
   continues all the way down
   until I unindent again.
```

Documentation blocks are associated with the _next_ entity in the file, except
for if they occur as the _very first_ entity in the file. In this case, they are
treated as the module's documentation.

Documentation comments are _not_ allowed inside textual interpolations.

The tool that generates this documentation aims to be fairly robust, and tries
to assign produce sensible results even if the user makes a mistake. Such
mistakes will be highlighted to the user.

The documentation is using
[Markdown](https://www.markdownguide.org/basic-syntax/) syntax with some
additional features, outlined below.

### YAML frontmatter

Documentation can start with an optional YAML frontmatter section, containing
entry metadata. This section is delimited by `---` lines, as follows:

```
## ---
   icon: data_input
   suggested: 1
   macros:
    - equals: filter=..Equal
   ---
   Documentation continues here in Markdown format.
```

The following keys are supported:

- `added` (string): Used to describe when a given construct was added to the
  library.
- `advanced` (boolean): Items that are _not_ private, but are for power users.
- `aliases` (array of strings): A name or names under which the documented
  entity will display in the searcher.
- `group` (string): Used to group constructs together in the searcher and
  documentation.
- `icon` (string): Used to provide an icon for the construct in the searcher and
  nodes.
- `deprecated` (boolean): Used for constructs that should no longer be used and
  that may be removed in the future.
- `modified` (string): Used for constructs that have had their behaviour change
  after a certain version of the library.
- `private` (boolean): Used to describe constructs that are private in the
  language.
- `removed` (string): Used to describe constructs that have been removed and are
  no longer functional.
- `unstable` (boolean): Used for items that are not yet considered stable.
- `upcoming` (string): Used to describe constructs that will be added in future
  versions of the library.
- `macros` (array of key-value pairs): Used to define macros for automatic
  expansion when the entry is selected in the component browser. Each macro
  consists of a key (name) and a value (expansion).
- `suggested` (number): Used to suggest a priority for the entry in the
  component browser. Lower the number, higher the priority.

Each key in the metadata is optional.

### Tables

Documentation supports tables, with the syntax described
[here](https://www.markdownguide.org/extended-syntax/#tables).
