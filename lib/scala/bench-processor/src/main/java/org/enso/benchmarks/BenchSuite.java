package org.enso.benchmarks;

import java.util.List;

/**
 * Wraps all the groups of benchmarks specified in a single module. Corresponds to {@code Bench.All}
 * defined in {@code distribution/lib/Standard/Test/0.0.0-dev/src/Bench.enso}.
 */
public interface BenchSuite {
  List<BenchGroup> groups();
}
