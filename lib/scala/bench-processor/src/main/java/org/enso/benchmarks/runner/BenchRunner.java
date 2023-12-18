package org.enso.benchmarks.runner;

import jakarta.xml.bind.JAXBException;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import org.openjdk.jmh.results.RunResult;
import org.openjdk.jmh.runner.BenchmarkList;
import org.openjdk.jmh.runner.BenchmarkListEntry;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.CommandLineOptionException;
import org.openjdk.jmh.runner.options.CommandLineOptions;
import org.openjdk.jmh.runner.options.OptionsBuilder;

public class BenchRunner {
  public static final File REPORT_FILE = new File("./bench-report.xml");

  /**
   * @return A list of qualified names of all benchmarks visible to JMH.
   */
  public List<String> getAvailable() {
    return BenchmarkList.defaultList().getAll(null, new ArrayList<>()).stream()
        .map(BenchmarkListEntry::getUsername)
        .collect(Collectors.toList());
  }

  public static void run(String[] args) throws RunnerException {
    CommandLineOptions cmdOpts = null;
    try {
      cmdOpts = new CommandLineOptions(args);
    } catch (CommandLineOptionException e) {
      System.err.println("Error parsing command line args:");
      System.err.println("  " + e.getMessage());
      System.exit(1);
    }

    if (Boolean.getBoolean("bench.compileOnly")) {
      // Do not report results from `compileOnly` mode
      runCompileOnly(cmdOpts.getIncludes());
    } else {
      Runner jmhRunner = new Runner(cmdOpts);

      if (cmdOpts.shouldHelp()) {
        System.err.println("Enso benchmark runner: A modified JMH runner for Enso benchmarks.");
        try {
          cmdOpts.showHelp();
        } catch (IOException e) {
          throw new IllegalStateException("Unreachable", e);
        }
        System.exit(0);
      }

      if (cmdOpts.shouldList()) {
        jmhRunner.list();
        System.exit(0);
      }

      Collection<RunResult> results;
      results = jmhRunner.run();

      for (RunResult result : results) {
        try {
          reportResult(result.getParams().getBenchmark(), result);
        } catch (JAXBException e) {
          throw new IllegalStateException("Benchmark result report writing failed", e);
        }
      }
      System.out.println("Benchmark results reported into " + REPORT_FILE.getAbsolutePath());
    }
  }

  private static Collection<RunResult> runCompileOnly(List<String> includes)
      throws RunnerException {
    System.out.println("Running benchmarks " + includes + " in compileOnly mode");
    var optsBuilder = new OptionsBuilder().measurementIterations(1).warmupIterations(0).forks(0);
    includes.forEach(optsBuilder::include);
    var opts = optsBuilder.build();
    var runner = new Runner(opts);
    return runner.run();
  }

  public static BenchmarkItem runSingle(String label) throws RunnerException, JAXBException {
    String includeRegex = "^" + label + "$";
    if (Boolean.getBoolean("bench.compileOnly")) {
      var results = runCompileOnly(List.of(includeRegex));
      var firstResult = results.iterator().next();
      return reportResult(label, firstResult);
    } else {
      var opts =
          new OptionsBuilder()
              .jvmArgsAppend("-Xss16M", "-Dpolyglot.engine.MultiTier=false")
              .include(includeRegex)
              .build();
      RunResult benchmarksResult = new Runner(opts).runSingle();
      return reportResult(label, benchmarksResult);
    }
  }

  private static BenchmarkItem reportResult(String label, RunResult result) throws JAXBException {
    Report report;
    if (REPORT_FILE.exists()) {
      report = Report.readFromFile(REPORT_FILE);
    } else {
      report = new Report();
    }

    BenchmarkItem benchItem = new BenchmarkResultProcessor().processResult(label, report, result);

    Report.writeToFile(report, REPORT_FILE);
    return benchItem;
  }
}
