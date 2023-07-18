# Exploring Table operation performance

These benchmarks are used to compare various approaches to computing operations
on Table columns, to find out what best practices should we use for these and
find venues for optimization of the language and Table implementation.

These benchmarks are not meant to be used for tracking performance of the
current implementation itself. That is supposed to be done by another project -
`Table_Benchmarks`.

## Structure

Currently, the benchmarks are split into a few files, each exploring some
separate topic, like mapping a single column, combining two columns with some
operation, or computing an aggregate operation over a column. In each file,
there may be a few Enso types, each representing a separate benchmark. Usually,
we have two benchmarks for each operation type - one dealing with a primitive
value type like integers (`long` in the Java side) and another dealing with a
reference type like `String` or `Date`. We expect the performance
characteristics between these may differ, e.g. because Java allows to use `long`
without boxing, so we compare them separately.

Each Enso type for a given benchmark contains multiple methods which represent
various 'approaches' to computing the same operation.

Each benchmark run has a name that consists of the type it defines it, a dot and
the method representing the particular approach, e.g.
`Boxed_Map_Test.enso_map_as_vector`.

## Running

The runner is very simple. If any options are to be customized, the Enso file
itself needs to be modified. One can run the whole project to run all the
benchmarks, or run only a specific file.

## Analysis

The output of the benchmarks should be saved to a file. Then that file can be
loaded using the Enso workflow in `tools/performance/benchmark-analysis`.

The workflow is tuned to analysing these comparative benchmarks.

At the top, one can select which file is to be analyzed. Below there is a
dropdown allowing to select one particular benchmark (represented by the type,
e.g. `Boxed_Map_Test`). With that selected, one can display a scatter plot
visualization comparing various approaches of that one given benchmark. On the
plot we can see runtimes of subsequent iterations. Later, we drop the first 40
iterations (the number can easily be customized in the workflow) to ensure
sufficient warm-up for each benchmark. Then a table is displayed computing the
average runtime of each approach and how they compare relative to each other - a
dropdown allows to select one benchmark that will be used as a reference point
(100%) for the average runtime comparison.
