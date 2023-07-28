package org.enso.benchmarks.libs;

import org.enso.benchmarks.processor.GenerateBenchSources;
import org.enso.benchmarks.runner.BenchRunner;

@GenerateBenchSources
public class LibBenchRunner {

  public static void main(String[] args) {
    BenchRunner.run(args);
  }
}
