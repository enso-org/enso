package org.enso.benchmarks.libs;

import org.enso.benchmarks.processor.GenerateBenchSources;
import org.enso.benchmarks.runner.BenchRunner;
import org.openjdk.jmh.runner.RunnerException;

/**
 * The only purpose for this class is to enable the {@link org.enso.benchmarks.processor.BenchProcessor}
 * to generate JMH sources for the benchmarks in {@code test/Benchmarks} project.
 * For more information see {@code docs/infrastructure/benchmarks.md#Standard-library-benchmarks}.
 */
@GenerateBenchSources(
    projectRootPath = "test/Benchmarks",
    moduleName = "local.Benchmarks.Main"
)
public class LibBenchRunner {

  public static void main(String[] args) throws RunnerException {
    BenchRunner.run(args);
  }
}
