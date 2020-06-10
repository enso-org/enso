---
layout: developer-doc
title: Searcher
category: runtime
tags: [runtime, search, execution]
order: 7
---

# Searcher
The language auto-completion feature requires an ability to search the codebase
using different search criteria. This document describes the Searcher module,
which consists of the suggestions database and the ranking algorithm for
ordering the results.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Suggestions Database](#suggestions-database)
  - [Static Analysis](#static-analysis)
  - [Runtime Inspection](#runtime-inspection)
  - [Search Indexes](#search-indexes)
- [Results Ranking](#results-ranking)

<!-- /MarkdownTOC -->

## Suggestions Database
Suggestions database holds entries for fulfilling the auto-completion requests
with additional indexes used to answer the different search criteria. The
database is populated partially by analyzing the sources and enriched with the
extra information from the runtime.

API data types for the suggestions database are specified in the [Language
Server Message Specification](../language-server/protocol-language-server.md)
document.

### Database Structure

Implementation utilizes the SQLite database.

Database is created per project and the database file stored in the project
directory. That way the index can be preserved between the IDE restarts.

#### Suggestions Table

* `id` `INTEGER` - unique identifier
* `kind` `INTEGER` - type of suggestion entry, i.e. Atom, Method, Function, or
  Local
* `name` `TEXT` - entry name
* `self_type` `TEXT` - self type of the Method
* `return_type` `TEXT` - return type of the entry
* `documentation` `TEXT` - documentation string

#### Arguments Table

* `id` `INTEGER` - unique identifier
* `suggestion_id` `INTEGER` - suggestion key this argument relates to
* `name` `TEXT` - argument name
* `type` `TEXT` - argument type; const 'Any' is used to specify generic types
* `is_suspended` `INTEGER` - indicates whether the argument is lazy
* `has_defult` `INTEGER` - indicates whether the argument has default value
* `default_value` `TEXT` - optional default value

### Static Analysis
The database is filled by analyzing the Intermediate Representation (`IR`)
parsed from the source.

Language constructs for suggestions database extracted from the `IR`:

- Atoms
- Methods
- Functions
- Local assignments
- Documentation

In addition, it holds the locality information for each entry. Locality
information specifies the scope where the node was defined, which is used in the
results [ranking](#results-ranking) algorithm.

### Runtime Inspection
The `IR` is missing information about the types. Runtime instrumentation is used
to capture the types and refine the database entries.

Information for suggestions database gathered in runtime:

- Types

### Search Indexes
The database allows searching the entries by the following criteria, applying
the [ranking](#results-ranking) algorithm:

- name
- type
- documentation text
- documentation tags

## Results Ranking
The search result has an intrinsic ranking based on the scope and the type.

### Scope
The results from the local scope have the highest rank in the search result. As
the scope becomes wider, the rank decreases.

``` ruby
const_x = 42

main =
    x1 = 0
    foo x =
        x2 = x + 1
        calculate #
```

For example, when completing the argument of `calculate: Number -> Number`
function, the search results will have the order of: `x2` > `x1` > `const_x`.

### Type

Suggestions based on the type are selected by matching with the string runtime
type representation.
