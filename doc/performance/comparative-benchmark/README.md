# Comparative Benchmarking
While we have comprehensive benchmarks of the Enso implementation itself, it's
also important that we have an idea of how Enso compares to other languages. In
order to do this we have a set of comparative benchmarks that execute the same
algorithm across the following languages:

- [C++](./benchmarks/cpp), as the usual fastest benchmark.
- [JavaScript](./benchmarks/js), as a commonly used language.
- [Haskell](./benchmarks/haskell), as the fastest functional language.
- [Java](./benchmarks/java), as the peak of performance on the JVM.
- [Python](./benchmarks/python), as the most-commonly used language for
  data-science.

Rather than benchmarking _identical_ code in each of these languages, we write
idiomatic code. This provides a better idea of how _real world_ code in each of
the languages may execute.

## Plot Generation
We also provide a script that ingests the data from the benchmark and creates a
nice plot of performance trends over time. It can be found in the directory
[`utilities/plot-generator`](./utilities/plot-generator).

