package org.enso.benchmarks;

import org.graalvm.polyglot.Value;

/**
 * Specification of a single benchmark. Corresponds to {@code Bench.Spec} defined in {@code
 * distribution/lib/Standard/Test/0.0.0-dev/src/Bench.enso}.
 */
public interface BenchSpec {
  String name();

  Value code();
}
