package org.enso.interpreter.bench;

import jakarta.xml.bind.JAXBException;
import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.List;
import org.openjdk.jmh.infra.BenchmarkParams;
import org.openjdk.jmh.infra.IterationParams;
import org.openjdk.jmh.results.BenchmarkResult;
import org.openjdk.jmh.results.IterationResult;
import org.openjdk.jmh.results.RunResult;
import org.openjdk.jmh.runner.Defaults;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.format.OutputFormat;
import org.openjdk.jmh.runner.format.OutputFormatFactory;
import org.openjdk.jmh.runner.options.CommandLineOptionException;
import org.openjdk.jmh.runner.options.CommandLineOptions;
import org.openjdk.jmh.runner.options.OptionsBuilder;
import org.openjdk.jmh.runner.options.TimeValue;

/** Runner class for the benchmarks. Discovers, runs and reports benchmark results. */
public class BenchmarksRunner {
  public static final File REPORT_FILE = new File("./bench-report.xml");

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
      var output =
          OutputFormatFactory.createFormatInstance(
              System.out, cmdOpts.verbosity().orElse(Defaults.VERBOSITY));
      Runner jmhRunner = new Runner(cmdOpts, new GitHubActionsFormat(output));

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

  /**
   * Results from compileOnly mode are not reported. Moreover, if some of the benchmarks in this
   * mode fails, the whole process immediately fails. This behavior is different to *normal*
   * benchmarks, where a single failure does not stop the whole process.
   */
  private static void runCompileOnly(List<String> includes) {
    if (includes.isEmpty()) {
      System.out.println("Running all benchmarks in compileOnly mode");
    } else {
      System.out.println("Running benchmarks " + includes + " in compileOnly mode");
    }
    var optsBuilder =
        new OptionsBuilder()
            .measurementTime(TimeValue.seconds(1))
            .measurementIterations(1)
            .warmupIterations(0)
            .shouldFailOnError(true)
            .forks(0);
    includes.forEach(optsBuilder::include);
    var opts = optsBuilder.build();
    var runner = new Runner(opts);
    try {
      runner.run();
      System.out.println(
          "benchmarks run successfully in compileOnly mode. Results are not reported.");
    } catch (RunnerException e) {
      System.err.println("Benchmark run failed: " + e.getMessage());
      e.printStackTrace(System.err);
      System.exit(1);
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

  private static final class GitHubActionsFormat implements OutputFormat {
    private final OutputFormat output;

    GitHubActionsFormat(OutputFormat output) {
      this.output = output;
    }

    @Override
    public void iteration(BenchmarkParams benchParams, IterationParams params, int iteration) {
      output.iteration(benchParams, params, iteration);
    }

    @Override
    public void iterationResult(
        BenchmarkParams benchParams, IterationParams params, int iteration, IterationResult data) {
      output.iterationResult(benchParams, params, iteration, data);
    }

    @Override
    public void startBenchmark(BenchmarkParams benchParams) {
      output.println("::group::" + benchParams.getBenchmark());
    }

    @Override
    public void endBenchmark(BenchmarkResult result) {
      output.println("::endgroup::");
    }

    @Override
    public void startRun() {
      output.startRun();
    }

    @Override
    public void endRun(Collection<RunResult> result) {
      output.endRun(result);
    }

    @Override
    public void print(String s) {
      output.print(s);
    }

    @Override
    public void println(String s) {
      output.println(s);
    }

    @Override
    public void flush() {
      output.flush();
    }

    @Override
    public void close() {}

    @Override
    public void verbosePrintln(String s) {
      output.verbosePrintln(s);
    }

    @Override
    public void write(int b) {
      output.write(b);
    }

    @Override
    public void write(byte[] b) throws IOException {
      output.write(b);
    }
  }
}
