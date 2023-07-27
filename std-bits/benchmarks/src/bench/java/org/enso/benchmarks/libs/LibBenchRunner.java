package org.enso.benchmarks.libs;

import java.io.IOException;
import java.util.Collection;
import org.enso.benchmarks.processor.GenerateBenchSources;
import org.graalvm.polyglot.Engine;
import org.openjdk.jmh.results.RunResult;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.CommandLineOptionException;
import org.openjdk.jmh.runner.options.CommandLineOptions;

@GenerateBenchSources
public class LibBenchRunner {

  public static void main(String[] args) {
    CommandLineOptions cmdOpts = null;
    try {
      cmdOpts = new CommandLineOptions(args);
    } catch (CommandLineOptionException e) {
      System.err.println("Error parsing command line args:");
      System.err.println("  " + e.getMessage());
      System.exit(1);
    }
    Runner jmhRunner = new Runner(cmdOpts);

    if (cmdOpts.shouldHelp()) {
      System.err.println("Enso libs benchmark runner: A modified JMH runner for Enso benchmarks.");
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
    try {
      results = jmhRunner.run();
    } catch (RunnerException e) {
      throw new RuntimeException(e);
    }

    System.out.println("Results:");
    for (RunResult result : results) {
      System.out.println(result.toString());
    }
  }
}
