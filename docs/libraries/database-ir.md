---
layout: developer-doc
title: Database IR
category: libraries
tags: [libraries, databases, integrations]
order: 4
---

# Overview

The database internal representation (IR) is used to describe full SQL queries
and statements in a backend-neutral way. The IR is compiled to SQL in
`Base_Generator`, with backend-specific variations supplied by the `Dialect`
modules.

End-users do not use IR types directly; they interact wih the `DB_Table` and
`DB_Column` types, which are analagous to the in-memory `Table` and `Column`
types. User-facing operations on these types do not immediately execute SQL in
the database backends; they only create IR. As a final step, the IR is compiled
into SQL and sent to the backend.

Informally, a "query" consists of a table expression and a set of column
expressions, roughly corresponding to:

```sql
select [column expression], [column expression]
from [table expression]
```

This terminology applies to both the user-facing and IR types, which represent
table and column expression in multiple ways.

# Main IR Types

Column expressions are represented by `SQL_Expression`. `SQL_Expression` values
only have meaning within the context of a table expression; they do not contain
their own table expressions.

Table expressions are represented by the mutually-recursive types `From_Spec`
and `Context`.

Top-level queries and DDL/DML commands are represented by the `Query` type.

## SQL_Expression

Represents a column expression. Can be a single column (`Column`), a derived
expression built from other expressions (`Operation`), a constant value
(`Constant`, `Literal`, `Text_Literal`), or a let-binding (`Let` and `Let_Ref`).

`SQL_Expression`s only have meaning in the context of a particular table
expression; for example, a `SQL_Expression.Column` value consists of the
name/alias of a table expression and the name of a column within it.

`Let` and `Let_Ref` variants are used to express let-style bindings using SQL
`with` syntax. This is used to reduce duplication. This is not so much for
efficiency, since backends often do their own de-duplication, but rather for
reducing the size of the SQL, which can grow exponentially with certain kinds of
nesting.

## From_Spec

Represents a table expression. Can be a database table (`Table`), a derived
table built from other tables (`Join`, `Union`), or a constant value (`Query`,
`Literal_Values`).

A `Query` value is a complete SQL query, either as a single `Text` or as an
`SQL_Statement` built safely from strings and values. A `Literal_Values`
consists of a table-shaped vector-of-vectors of values and is compiled into an
inline literal SQL table expression.

`Sub_Query` is used to nest a query as a subquery, replacing column expressions
with aliases to those same column expressions within the subquery. This is used
to keep query elements such as `where`, `order by`, and `group by` separate to
prevent unwanted interactions between them. This allows `join` and `union`
operations on complex queries, as well as more specific operations such as
`DB_Table.add_row_number`. This is explained more fully below in the
[`Subqueries` section](#subqueries).

## Context

Represents a table expression, along with `where`, `order by`, `group by` and
`limit` clauses.

A `DB_Column` contains its own reference to a `Context`, so it can be read
without relying on the `DB_Table` object that it came from. In fact, `DB_Column`
values are standalone and not directly tied to particular `DB_Table` instance.
Instead, they are connected to the `Context` objects they contain, and all
`DB_Columns` from a single table expression must share the same `Context`. This
corresponds to the idea that the columns expressions in a `SELECT` clause all
refer to the same table expression in the `FROM` clause.

And also we can 'merge' `DB_Column`s that have the same `Context` into a single
`DB_Table` e.g. via `DB_Table.set`, allowing to add more derived expressions to
existing tables. Compatibility between `Context`s is verified by the
`Helpers.check_integrity` method.

## Query

A query (`Select`), or other DML or DDL command (`Insert`, `Create_Table`,
`Drop_Table `, and others).

# Relationships Between The Main Types

This section covers the main ways in which both the IR and user-facing types are
combined and nested to describe typical queries; it is not comprehensive.

A `DB_Table` serves as a user-facing table expression, and contains column
expressions as `Internal_Column`s and a table expression as a `Context`.

A `DB_Column` serves as a user-facing column expression, and contains a column
expression as an `SQL_Expression` and a table expression as a `Context`.

An `Internal_Column` serves as a column expression, and contains a
`SQL_Expression`, but no table expression. An `Internal_Column` is always used
inside a `DB_Table`, and inherits its table expression from the `DB_Table`'s
`Context`.

A `From_Spec` serves as a table expression, and corresponds to the 'from' clause
of an SQL query. It can be a base value (table name, constant, etc), join,
union, or subquery:

- `From_Spec.Join`: contains `From_Spec` values from the individual tables, as
  well as `SQL_Expressions` for join conditions
- `From_Spec.Union`: contains a vector of `Query` values for the individual
  tables.
- `From_Spec.Sub_Query`: contains column expressions as `SQL_Expression`s, and a
  table expression as a `Context`.

A `Context` serves as a table expression, and corresponds to the `from` clause
of an SQL query, as well as everything after the `from` clause, including
`where`, `order by`, `group by` and `limit` clauses.

# Subqueries

Subqueries are created using `Context.as_subquery`. They correspond to (and are
compiled into) subselects. This allows them to be referred to by an alias, and
also nests certian clauses (`where`, `order by`, `group by` and `limit`) in a
kind of 'scope' within the subselect so that they will not interfere with other
such clauses.

By itself, turning a query into a subquery does not change its value. But it
prepares it to be used in larger queries, such as ones formed with `join` and
`union`, as well as other more specific operations within the database library
(such as `DB_Table.add_row_number`).

In the IR, `Context.as_subquery` prepares a table expression for nesting, but
does not do the actual nesting within another query. To do the actual nesting,
you use the prepared subquery as a table expression within a larger query.

Creating a subquery consists of replacing complex column expressions with
aliases that refer to the original complex expressions within the nested query.
For example, a query such as

```sql
select [complex column expression 1],
       [complex column expression 2]
from [complex table expression]
where [where clauses]
group by [group-by clauses]
order by [order-by clauses]
```

would be transformed into

```sql
select alias1, alias2
from (select [complex column expression 1] as alias1,
             [complex column expression 2] as alias2
      from [complex table expression]
      where [where clauses]
      group by [group-by clauses]
      order by [order-by clauses]) as [table alias]
```

After this transformation, the top-level query has no `where`, `group by`, or
`order by` clauses. These can now be added:

```sql
select alias1, alias2
from (select [complex column expression 1] as alias1,
             [complex column expression 2] as alias2
      from [complex table expression]
      where [where clauses]
      group by [group-by clauses]
      order by [order-by clauses]) as [table alias]
where [more where clauses]
group by [more group-by clauses]
order by [more order-by clauses])
```

Thanks to this nesting, there can be no unwanted interference between the
`where`, `group by`, or `order by` at different levels.

The added table alias allows join conditions to refer to the columns of the
individual tables being joined.

The `Context.as_subquery` method returns a `Sub_Query_Setup`, which contains a
table expression as a `From_Spec`, a set of simple column expressions as
`Internal_Column`s, and a helper function that can convert an original complex
`Internal_Column` into its simplified alias form.

# Examples

In each of the examples below, there is an Enso value, followed by the SQL that
the value is compiled into, and the results of the query. The first three
examples are table expressions, and the second three are column expressions.

## Query a simple table

This is a simple `select *`.

Enso:

```
t = table_builder [['x', [1, 2]], ['y', [10, 20]]]
```

IR:

```
(Select
  [['x', (Column 'table_0' 'x')], ['y', (Column 'table_0' 'y')]]
  (Context.Value (Table 'table_0' 'table_0' Nothing) [] [] [] Nothing []))
```

SQL:

```
SELECT table_0.x AS x, table_0.y AS y
FROM table_0 AS table_0
```

Results:

```
 x | y
---+----
 1 | 10
 2 | 20
```

## Add a derived column

This adds a derived column, resulting in a more complex column expression.

Enso:

```
tc = t . set ((t.at 'x') * (t.at 'x')) as="prod"
```

IR:

```
(Select
  [['x', (Column 'table_0' 'x')], ['y', (Column 'table_0' 'y')], ['prod', (Operation '*' [(Column 'table_0' 'x'), (Column 'table_0' 'x')] Nothing)]]
  (Context.Value (Table 'table_0' 'table_0' Nothing) [] [] [] Nothing []))
```

SQL:

```
SELECT table_0.x AS x, table_0.y AS y, (table_0.x * table_0.x) AS prod
FROM table_0 AS table_0
```

Results:

```
 x | y  | prod
---+----+------
 1 | 10 | 1
 2 | 20 | 4
```

## As subquery

This uses `as_subquery` to nest the table in a subselect, so that the top-level
column expressions are all simple, and the complex product column expression is
nested inside the subselect.

Enso:

```
tcsq = tc.as_subquery
```

IR:

```
(Select
  [['x', (Column 'table_0' 'x')], ['y', (Column 'table_0' 'y')], ['prod', (Column 'table_0' 'prod')]]
  (Context.Value
    (Sub_Query
      [['x', (Column 'table_0' 'x')], ['y', (Column 'table_0' 'y')], ['prod', (Operation '*' [(Column 'table_0' 'x'), (Column 'table_0' 'x')] Nothing)]]
      (Context.Value (Table 'table_0' 'table_0' Nothing) [] [] [] Nothing []) 'table_0')
    [] [] [] Nothing []))
```

SQL:

```
SELECT table_0.x AS x, table_0.y AS y, table_0.prod AS prod
FROM (SELECT table_0.x AS x, table_0.y AS y, (table_0.x * table_0.x) AS prod
      FROM table_0 AS table_0) AS table_0
```

Results:

```
 x | y  | prod
---+----+------
 1 | 10 | 1
 2 | 20 | 4
```

## Derived column expression

Complex column expression.

Enso:

```
prod = tc.at "prod"
```

IR:

```
(Select
  [['prod', (Operation '*' [(Column 'table_0' 'x'), (Column 'table_0' 'x')] Nothing)]]
  (Context.Value (Table 'table_0' 'table_0' Nothing) [] [] [] Nothing []))
```

SQL:

```
SELECT (table_0.x * table_0.x) AS prod
FROM table_0 AS table_0
```

Results:

```
 prod
------
 1
 4
```

## More complex derived column expression

Even more complex column expression, with a repeated subexpression.

Enso:

```
prodsum = (prod + prod) . rename "prodsum"
```

IR:

```
(Select
  [['prodsum', (Operation 'ADD_NUMBER' [(Operation '*' [(Column 'table_0' 'x'), (Column 'table_0' 'x')] Nothing), (Operation '*' [(Column 'table_0' 'x'), (Column 'table_0' 'x')] Nothing)] Nothing)]]
  (Context.Value (Table 'table_0' 'table_0' Nothing) [] [] [] Nothing []))
```

SQL:

```
SELECT ((table_0.x * table_0.x) + (table_0.x * table_0.x)) AS prodsum
FROM table_0 AS table_0
```

Results:

```
 prodsum
---------
 2
 8
```

## More complex derived column expression, with `Let`

This nests the product column expression inside a `with` clause, so it is not
repeated in the main `select`.

Enso:

```
lprodsum = prod.let "prod" prod->
    (prod + prod) . rename "let_prodsum"
```

IR:

```
(Select
  [['let_prodsum',
      (Let 'prod' 'enso-table-eea768aa-06bb-4aab-88b0-e5cd45fdd35d'
           (Operation '*' [(Column 'table_0' 'x'), (Column 'table_0' 'x')] Nothing)
           (Operation 'ADD_NUMBER' [(Let_Ref 'prod' 'enso-table-eea768aa-06bb-4aab-88b0-e5cd45fdd35d'), (Let_Ref 'prod' 'enso-table-eea768aa-06bb-4aab-88b0-e5cd45fdd35d')] Nothing))]]
  (Context.Value (Table 'table_0' 'table_0' Nothing) [] [] [] Nothing []))
```

SQL:

```
SELECT (WITH prod_0 AS (SELECT ((table_0.x * table_0.x)) AS x)\
             SELECT (prod_0.x + prod_0.x) FROM prod_0) AS let_prodsum
             FROM table_0 AS table_0
```

Results:

```
 let_prodsum
-------------
 2
 8
```

# Context Extensions

TODO

# Additional Types

- SQL_Statement
- SQL_Fragment
- SQL_Builder
- SQL_Query

TODO
