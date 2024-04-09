package org.enso.interpreter.bench.benchmarks;

import org.enso.interpreter.bench.BenchmarksRunner;
import org.openjdk.jmh.runner.RunnerException;

public class RuntimeBenchmarksRunner {
  public static void main(String[] args) throws RunnerException {
    BenchmarksRunner.run(args);
  }
}
