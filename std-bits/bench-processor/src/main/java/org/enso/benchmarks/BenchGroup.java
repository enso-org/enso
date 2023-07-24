package org.enso.benchmarks;

import java.util.List;

public interface BenchGroup {
  String name();
  BenchConfig configuration();
  List<BenchSpec> specs();
}
