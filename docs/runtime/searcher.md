---
layout: developer-doc
title: Searcher
category: runtime
tags: [runtime, search, execution]
order: 8
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
- [Implementation](#implementation)

<!-- /MarkdownTOC -->

## Suggestions Database

Suggestions database holds entries for fulfilling the auto-completion requests
with additional indexes used to answer the different search criteria. The
database is populated partially by analyzing the sources and enriched with the
extra information from the runtime.

API data types for the suggestions database are specified in the
[Language Server Message Specification](../language-server/protocol-language-server.md)
document.

### Database Structure

Implementation utilizes the SQLite database.

Database is created per project and the database file stored in the project
directory. That way the index can be preserved between the IDE restarts.

#### Suggestions Table

Suggestions table stores suggestion entries.

| Column               | Type      | Description                                                         |
| -------------------- | --------- | ------------------------------------------------------------------- |
| `id`                 | `INTEGER` | the unique identifier                                               |
| `externalId`         | `UUID`    | the external id from the IR                                         |
| `kind`               | `INTEGER` | the type of suggestion entry, i.e. Atom, Method, Function, or Local |
| `module`             | `TEXT`    | the module name                                                     |
| `name`               | `TEXT`    | the suggestion name                                                 |
| `self_type`          | `TEXT`    | the self type of the Method                                         |
| `return_type`        | `TEXT`    | the return type of the entry                                        |
| `scope_start`        | `INTEGER` | the start position of the definition scope                          |
| `scope_end`          | `INTEGER` | the end position of the definition scope                            |
| `documentation`      | `TEXT`    | the documentation string                                            |
| `documentation_html` | `TEXT`    | the documentation string rendered as HTML                           |

#### Arguments Table

Arguments table stores all suggestion arguments with `suggestion_id` foreign
key.

| Column          | Type      | Description                                                     |
| --------------- | --------- | --------------------------------------------------------------- |
| `id`            | `INTEGER` | the unique identifier                                           |
| `suggestion_id` | `INTEGER` | the suggestion key this argument relates to                     |
| `index`         | `INTEGER` | the argument position in the arguments list                     |
| `name`          | `TEXT`    | the argument name                                               |
| `type`          | `TEXT`    | the argument type; const 'Any' is used to specify generic types |
| `is_suspended`  | `INTEGER` | indicates whether the argument is lazy                          |
| `has_defult`    | `INTEGER` | indicates whether the argument has default value                |
| `default_value` | `TEXT`    | the optional default value                                      |

#### Suggestions Version Table

Versions table has a single row with the current database version. The version
is updated on every change in the suggestions table.

| Column | Type      | Description                                                     |
| ------ | --------- | --------------------------------------------------------------- |
| `id`   | `INTEGER` | the unique identifier representing the currend database version |

#### Schema Version Table

Schema version table has a single row with the current database schema version.
It is used during the application startup to check that the database schema is
up to date with the application version.

| Column | Type      | Description                                                        |
| ------ | --------- | ------------------------------------------------------------------ |
| `id`   | `INTEGER` | unique identifier representing the currend database schema version |

#### Suggestions Order Table

Suggestions order table stores the order of the module-level entries in the
source file. `suggestion_id` column is a foreign key that refers to the `id`
primary key in the Suggestions table.

| Column          | Type      | Description                              |
| --------------- | --------- | ---------------------------------------- |
| `suggestion_id` | `INTEGER` | the unique identifier of a suggestion    |
| `prev_id`       | `INTEGER` | the suggestion that goes before this one |
| `next_id`       | `INTEGER` | the suggestion that goes after this one  |

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

```ruby
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

## Implementation

The searcher primarily consists of:

- Suggestions database that stores suggestion entries and SHA hashes of the
  opened file contents.
- Search requests handler that serves search and capability requests, and
  listens to the notifications from the runtime.
- Suggestion builder (aka _indexer_) that extracts suggestion entries from `IR`
  in compile time.

```

                                  +--------------------------------+
                                  |         SuggestionsDB          |
                                  +-----------------+--------------+
                                  |         SuggestionsRepo        |
                                  +----+------------+---------+----+
                                       ^                      ^
                                       |                      |
            Capability,Search          v                      v
 +--------+      Request     +---------+----------+        +--+------------------+
 | Client +<---------------->+ SuggestionsHandler |        | CollaborativeBuffer |
 +--------+ Database Update  +---------+----------+        +-------------------+-+
            Notifications              ^                  OpenFileNotification |
                                       |                  (isIndexed)          |
                                       |                                       v
                             +---------+---------------------------------------+-------+
                             |                   RuntimeConnector                      |
                             +----+----------+---------------------------------+-------+
                                  ^          ^                                 |
        ExpressionValuesComputed  |          |  SuggestionsDBUpdate            |
                                  |          |                                 |
               +------------------+--+    +--+----------------+                |
               | ExecutionInstrument |    | EnsureCompiledJob |           +----+------+
               +---------------------+    +-------------------+           |  Module   |
                                          |                   | (re)index +-----------+
                                          | SuggestionBuilder +-----------+ isIndexed |
                                          |                   |           +-----------+
                                          +-------------------+

```

### Indexing

Indexing is a process of extracting suggestions information from the `IR`. To
keep the index in the consistent state, the language server tracks SHA versions
of all opened files in the suggestions database.

- When the user opens a file, we compare its SHA hash with the saved digest. If
  they don't match, we mark the file for re-index by setting the corresponding
  flag in the OpenFile notification sent to the runtime. On receive, the runtime
  marks the module as not indexed.
- When the file is compiled, we check the indexed flag on the module. If the
  module has not been indexed yet, we send all suggestions extracted from the IR
  as a ReIndexed update to the language server. If the module has been indexed,
  we only send suggestions that have been affected by the recent edits as a
  Database update.
- When the language server receives ReIndexed update, it removes all suggestions
  related to this module, applies newly received updates, and sends a combined
  update of removed and added entries to the subscribed users. When the language
  server receives a regular Database update, it applies the update on the
  database and sends the appropriate updates to the subscribers.
- When the user deletes a file, we remove all entries with the corresponding
  module from the database and update the users.

### Suggestion Builder

The suggestion builder takes part in the indexing process. It extracts
suggestions from the compiled `IR`. It also converts `IR` locations represented
as an absolute char indexes (from the beginning of the file) to a position
relative to a line used in the rest of the project.

### Runtime Instrumentation

Apart from the type ascriptions, we can only get the return types of expressions
in runtime. On the module execution, the language server listens to the
`ExpressionValuesComputed` notifications sent from the runtime, which contain
the external id and the actual return type. Then the language server uses
external id as a key to update return types in the database.

### Search Requests

The search request handler has direct access to the database. The completion
request is more complicated then others. To query the database, it needs to
convert the requested file path to the module name. To do this, on startup, the
language server loads the package definition from the root directory and obtains
the project name. Then, having the project name and the root directory path, it
can recover the module name from the requested file path.

The project name can be changed with the refactoring `renameProject` command. To
cover this case, the request handler listens to the `ProjectNameChanged` event
and updates the project name accordingly.
