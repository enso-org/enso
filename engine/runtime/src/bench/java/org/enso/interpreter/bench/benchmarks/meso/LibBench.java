package org.enso.interpreter.bench.benchmarks.meso;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.infra.BenchmarkParams;

public class LibBench {
  @Setup
  public void setup(BenchmarkParams params) {
    String benchName = params.getBenchmark();
  }

  @Benchmark
  public void benchmark() {

  }
}
