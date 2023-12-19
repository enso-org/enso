# Benchmarks

In this document, we describe the benchmark types used for the runtime - Engine
micro benchmarks in the section
[Engine JMH microbenchmarks](#engine-jmh-microbenchmarks) and standard library
benchmarks in the section
[Standard library benchmarks](#standard-library-benchmarks), and how and where
are the results stored and visualized in the section
[Visualization](#visualization).

To track the performance of the engine, we use
[JMH](https://openjdk.org/projects/code-tools/jmh/). There are two types of
benchmarks:

- [micro benchmarks](#engine-jmh-microbenchmarks) located directly in the
  `runtime` SBT project. These benchmarks are written in Java, and are used to
  measure the performance of specific parts of the engine.
- [standard library benchmarks](#standard-library-benchmarks) located in the
  `test/Benchmarks` Enso project. These benchmarks are entirelly written in
  Enso, along with the harness code.

## Engine JMH microbenchmarks

These benchmarks are written in Java and are used to measure the performance of
specific parts of the engine. The sources are located in the `runtime` SBT
project, under `src/bench` source directory.

### Running the benchmarks

To run the benchmarks, use `bench` or `benchOnly` command - `bench` runs all the
benchmarks and `benchOnly` runs only one benchmark specified with the fully
qualified name. The parameters for these benchmarks are hard-coded inside the
JMH annotations in the source files. In order to change, e.g., the number of
measurement iterations, you need to modify the parameter to the `@Measurement`
annotation.

### Debugging the benchmarks

Currently, the best way to debug the benchmark is to set the `@Fork` annotation
to 0, and to run `withDebug` command like this:

```
withDebug --debugger benchOnly -- <fully qualified benchmark name>
```

## Standard library benchmarks

Unlike the Engine micro benchmarks, these benchmarks are written entirely in
Enso and located in the `test/Benchmarks` Enso project. There are two ways to
run these benchmarks:

- [Running standalone](#running-standalone)
- [Running via JMH launcher](#running-via-jmh-launcher)

Note that to avoid inflating the run-time of the std-lib benchmarks on the CI,
some extra benchmarks (which are not measuring important functionality, but may
serve as a baseline when trying to understand performance of similar scenarios)
are disabled by default. To enable them, set the `ENSO_ENABLE_EXTRA_BENCHMARKS`
environment variable before running any benchmarks.

### Running standalone

There is a universal launcher that enlists and executes all available benchmarks
in `test/Benchmarks` project. Run it with

```bash
enso$ runEngineDistribution --run test/Benchmarks
```

command. The launcher accepts additional `filter` argument which allows one to
select a benchmark of one's choice by checking for substrings in group or
benchmark name. For example:

```bash
enso$ runEngineDistribution --run test/Benchmarks New_Vector
```

runs all the benchmarks that have `New_Vector` in their name.

The harness within the project is not meant for any sophisticated benchmarking,
but rather for quick local evaluation. See the `Bench.measure` method
documentation for more details. For more sophisticated approach, run the
benchmarks via the JMH launcher.

### Running via JMH launcher

The JMH launcher is located in `std-bits/benchmarks` directory, as
`std-benchmarks` SBT project. It is a single Java class with a `main` method
that just delegates to the
[standard JMH launcher](https://github.com/openjdk/jmh/blob/master/jmh-core/src/main/java/org/openjdk/jmh/Main.java),
therefore, supports all the command line options as the standard launcher. For
the full options summary, either see the
[JMH source code](https://github.com/openjdk/jmh/blob/master/jmh-core/src/main/java/org/openjdk/jmh/runner/options/CommandLineOptions.java),
or run the launcher with `-h` option.

The `std-benchmarks` SBT project supports `bench` and `benchOnly` commands, that
work the same as in the `runtime` project, with the exception that the benchmark
name does not have to be specified as a fully qualified name, but as a regular
expression. To access the full flexibility of the JMH launcher, run it via
`Bench/run` - for example, to see the help message: `Bench/run -h`. For example,
you can run all the benchmarks that have "New_Vector" in their name with just 3
seconds for warmup iterations and 2 measurement iterations with
`Bench/run -w 3 -i 2 New_Vector`.

Whenever you add or delete any benchmarks from `test/Benchmarks` project, the
generated JMH sources need to be recompiled with `Bench/clean; Bench/compile`.
You do not need to recompile the `std-benchmarks` project if you only modify the
benchmark sources.

## Visualization

The benchmarks are invoked as a daily
[GitHub Action](https://github.com/enso-org/enso/actions/workflows/benchmark.yml),
that can be invoked manually on a specific branch as well. The results are kept
in the artifacts produced from the actions. In
`tools/performance/engine-benchmarks` directory, there is a simple Python script
for collecting and processing the results. See the
[README in that directory](../../tools/performance/engine-benchmarks/README.md)
for more information about how to run that script. This script is invoked
regularly on a private machine and the results are published in
[https://enso-org.github.io/engine-benchmark-results/](https://enso-org.github.io/engine-benchmark-results/).
