package org.enso.benchmarks;

/**
 * A configuration for a {@link BenchGroup benchmark group}.
 * Corresponds to {@code Bench_Options} in {@code distribution/lib/Standard/Test/0.0.0-dev/src/Bench.enso}
 */
public interface BenchConfig {
  int size();
  int iter();
}
