package org.enso.interpreter.bench.benchmarks.meso;

import java.util.List;

public interface BenchGroup {
  String name();
  BenchConfig configuration();
  List<BenchSpec> specs();
}
