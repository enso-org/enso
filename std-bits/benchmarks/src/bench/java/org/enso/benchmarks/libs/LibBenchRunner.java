package org.enso.benchmarks.libs;

import org.enso.benchmarks.processor.GenerateBenchSources;
import org.enso.benchmarks.runner.BenchRunner;
import org.openjdk.jmh.runner.RunnerException;

@GenerateBenchSources(
    projectRootPath = "test/Benchmarks",
    moduleName = "local.Benchmarks.Main"
)
public class LibBenchRunner {

  public static void main(String[] args) throws RunnerException {
    BenchRunner.run(args);
  }
}
