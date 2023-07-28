# Benchmarks
In this document, we describe the benchmark types used for the runtime - micro benchmarks in
the section [Engine JMH microbenchmarks](#engine-jmh-microbenchmarks) and meso benchmarks
in the section [Standard library benchmarks](#standard-library-benchmarks), and how and where
are the results stored and visualized in the section [Visualization](#visualization).

To track the performance of the engine, we use [JMH](https://openjdk.org/projects/code-tools/jmh/).
There are two types of benchmarks:
- [micro benchmarks](#engine-jmh-microbenchmarks) located directly in the `runtime` SBT project.
  These benchmarks are written in Java, and are used to measure the performance of
  specific parts of the engine.
- [standard library benchmarks](#standard-library-benchmarks) (meso benchmarks) located in the `test/Benchmarks` Enso project.
  These benchmarks are entirelly written in Enso, along with the harness code.

## Engine JMH microbenchmarks
These benchmarks are written in Java and are used to measure the performance of
specific parts of the engine.
The sources are located in the `runtime` SBT project, under `src/bench` source directory.

### Running the benchmarks
To run the benchmarks, use `bench` or `benchOnly` command - `bench` runs all the benchmarks and
`benchOnly` runs only one benchmark specified with the fully qualified name.
The parameters for these benchmarks are hard-coded inside the JMH annotations in the
source files. In order to change, e.g., the number of measurement iterations, you need to
modify the parameter to the `@Measurement` annotation.

### Debugging the benchmarks

Make sure your IDE listens for JDWP connection at port 5005.
Debug the benchmark by running `withDebug` command like this:
```
withDebug --debugger benchOnly -- <fully qualified benchmark name>
```

## Standard library benchmarks
Unlike the micro benchmarks, these benchmarks are written entirelly in Enso and located in the
`test/Benchmarks` Enso project. Sometimes, we call those *meso* benchmarks.
There are two ways to run these benchmarks:
- [Running standalone](#running-standalone)
- [Running via JMH launcher](#running-via-jmh-launcher)

### Running standalone
A single source file in the project may contain multiple benchmark definitions.
If the source file defines `main` method, we can evaluate it the same way as any other
Enso source file, for example via `runEngineDistribution --in-project test/Benchmarks --run <source file>`.
The harness within the project is not meant for any sophisticated benchmarking, but rather
for quick local evaluation. See the `Bench.measure` method documentation for more details.
For more sophisticated approach, run the benchmarks via JMH launcher.

### Running via JMH launcher
The JMH launcher is located in `std-bits/benchmarks` directory, as `bench-libs` SBT project.
It is a single Java class with a `main` method that just delegates to the
[standard JMH launcher](https://github.com/openjdk/jmh/blob/master/jmh-core/src/main/java/org/openjdk/jmh/Main.java),
therefore, supports all the command line options as the standard launcher.
For the full options summary, either see the [JMH source code](https://github.com/openjdk/jmh/blob/master/jmh-core/src/main/java/org/openjdk/jmh/runner/options/CommandLineOptions.java),
or run the launcher with `-h` option.

The `bench-libs` SBT project supports `bench` and `benchOnly` commands, that work the same
as in the `runtime` project, with the exception that the benchmark name does not have to be
specified as a fully qualified name, but as a regular expression.
To access the full flexibility of the JMH launcher, run it via `Bench/run` - for example,
to see the help message: `Bench/run -h`.

To debug a single benchmark, set the `javaOptions` with something like `set javaOptions += "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=localhost:8000"`,
and launch a single benchmark without forking with `Bench/run -f 0 <bench name>`.
Then, attach to the debugger with your IDE.


## Visualization
The benchmarks are invoked as a daily [GitHub Action](https://github.com/enso-org/enso/actions/workflows/benchmark.yml),
that can be invoked manually on a specific branch as well.
The results are kept in the artifacts produced from the actions.
In `tools/performance/engine-benchmarks` directory, there is a simple Python
script for collecting and processing the results. See the README in that directory
for more information about how to run that script.
This script is invoked regularly on a private machine and the results are
published in [https://enso-org.github.io/engine-benchmark-results/](https://enso-org.github.io/engine-benchmark-results/).

This is not meant as a permanent solution.
