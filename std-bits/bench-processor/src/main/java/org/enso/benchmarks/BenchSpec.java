package org.enso.benchmarks;

import org.graalvm.polyglot.Value;

/**
 * Specification of a single benchmark.
 */
public interface BenchSpec {
  String name();
  Value code();
}
