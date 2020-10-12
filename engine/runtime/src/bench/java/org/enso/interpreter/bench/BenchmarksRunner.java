package org.enso.interpreter.bench;

import org.openjdk.jmh.results.RunResult;
import org.openjdk.jmh.runner.BenchmarkList;
import org.openjdk.jmh.runner.BenchmarkListEntry;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

import javax.xml.bind.JAXBException;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/** Runner class for the benchmarks. Discovers, runs and reports benchmark results. */
public class BenchmarksRunner {
  public static final File REPORT_FILE = new File("./bench-report.xml");

  /** @return A list of qualified names of all benchmarks visible to JMH. */
  public List<String> getAvailable() {
    return BenchmarkList.defaultList().getAll(null, new ArrayList<>()).stream()
        .map(BenchmarkListEntry::getUsername)
        .collect(Collectors.toList());
  }

  /**
   * Runs and persists a new report for a given benchmark.
   *
   * @param label a qualified (including method) name of the benchmark to run.
   * @return a {@link BenchmarkItem} containing current run result and historical results.
   */
  public BenchmarkItem run(String label) throws RunnerException, JAXBException {
    Options benchmarkOptions =
        new OptionsBuilder()
            .jvmArgsAppend("-Xss16M")
            .include("^" + label + "$")
            .build();
    RunResult benchmarksResult = new Runner(benchmarkOptions).runSingle();

    Report report;
    if (REPORT_FILE.exists()) {
      report = Report.readFromFile(REPORT_FILE);
    } else {
      report = new Report();
    }

    BenchmarkItem benchItem =
        new BenchmarkResultProcessor().processResult(label, report, benchmarksResult);

    Report.writeToFile(report, REPORT_FILE);
    return benchItem;
  }
}
