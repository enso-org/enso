package org.enso.benchmarks.libs;

import org.enso.benchmarks.processor.GenerateBenchSources;
import org.enso.benchmarks.runner.BenchRunner;

@GenerateBenchSources(
    projectRootPath = "test/Benchmarks",
    moduleName = "local.Benchmarks.Main"
)
public class LibBenchRunner {

  public static void main(String[] args) {
    BenchRunner.run(args);
  }
}
