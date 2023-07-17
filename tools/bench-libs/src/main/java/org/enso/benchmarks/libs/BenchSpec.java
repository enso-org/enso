package org.enso.benchmarks.libs;

import org.graalvm.polyglot.Value;

/**
 * Specification of a single benchmark.
 */
public interface BenchSpec {
  String name();
  Value code();
}
