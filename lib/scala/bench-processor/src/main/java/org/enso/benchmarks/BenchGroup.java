package org.enso.benchmarks;

import java.util.List;

/**
 * A group of benchmarks with its own name and configuration.
 * Corresponds to {@code Bench.Group} defined in {@code distribution/lib/Standard/Test/0.0.0-dev/src/Bench.enso}.
 */
public interface BenchGroup {
  String name();
  BenchConfig configuration();
  List<BenchSpec> specs();
}
