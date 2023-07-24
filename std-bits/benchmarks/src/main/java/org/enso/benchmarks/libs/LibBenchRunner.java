package org.enso.benchmarks.libs;

import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import org.enso.benchmarks.BenchGroup;
import org.enso.benchmarks.BenchSpec;
import org.enso.benchmarks.BenchSuite;
import org.enso.benchmarks.processor.Dummy;
import org.openjdk.jmh.results.RunResult;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.ChainedOptionsBuilder;
import org.openjdk.jmh.runner.options.CommandLineOptionException;
import org.openjdk.jmh.runner.options.CommandLineOptions;
import org.openjdk.jmh.runner.options.OptionsBuilder;

@Dummy
public class LibBenchRunner {

  private static final String benchNameDelimiter = "/";

  public static void main(String[] args) {
    CommandLineOptions cmdOpts = null;
    try {
      cmdOpts = new CommandLineOptions(args);
    } catch (CommandLineOptionException e) {
      System.err.println("Error parsing command line args:");
      System.err.println("  " + e.getMessage());
      System.exit(1);
    }

    if (cmdOpts.shouldHelp()) {
      System.err.println("Enso libs benchmark runner: A modified JMH runner for Enso benchmarks.");
      System.err.println();
      System.err.println("Usage: runner [options] [benchmark-regex]..");
      System.err.println("  [benchmark-regex].. - regexes of benchmarks to run");
      System.err.println("  [options] - options passed to JMH runner.");
      System.err.println();
      System.err.println("Benchmark regex format: <group-regex/label-regex>");
      System.err.println("  - note the slash between regexes");
      System.err.println("  - `group-regex` or `label-regex` can be omitted, in such case, it is treated as `.*`");
      System.err.println();
      System.err.println("Options from JMH Runner:");
      try {
        cmdOpts.showHelp();
      } catch (IOException e) {
        throw new IllegalStateException("Unreachable", e);
      }
      System.exit(0);
    }

    File ensoRootDir = Paths.get(System.getProperty("user.dir")).toFile();
    for (; ensoRootDir != null; ensoRootDir = ensoRootDir.getParentFile()) {
      if (ensoRootDir.getName().equals("enso")) {
        break;
      }
    }
    if (ensoRootDir == null) {
      throw new IllegalStateException("Unreachable: Could not find Enso root directory");
    }
    File benchRootDir = ensoRootDir.toPath()
        .resolve("test")
        .resolve("Benchmarks")
        .toFile();
    if (!benchRootDir.isDirectory() || !benchRootDir.canRead()) {
      throw new IllegalStateException("Unreachable: Could not find Enso benchmarks directory");
    }
    // Note that ensoHomeOverride does not have to exist, only its parent directory
    File ensoHomeOverride = ensoRootDir.toPath()
        .resolve("distribution")
        .resolve("component")
        .toFile();
    var specCollector = new SpecCollector(benchRootDir, ensoHomeOverride);
    Collection<BenchSuite> benchSuites = specCollector.collectAllBenchSpecs();

    if (cmdOpts.shouldList()) {
      for (BenchSuite benchSuite : benchSuites) {
        for (BenchGroup group : benchSuite.groups()) {
          System.out.println("Group \"" + group.name() + "\": ");
          group.specs().forEach(
              spec -> System.out.println("  - " + spec.name())
          );
        }
      }
      System.exit(0);
    }

    Set<String> benchNames = new HashSet<>();
    for (BenchSuite benchSuite: benchSuites) {
      for (BenchGroup group : benchSuite.groups()) {
        String groupName = group.name();
        for (BenchSpec spec : group.specs()) {
          String specName = spec.name();
          benchNames.add(groupName + benchNameDelimiter + specName);
        }
      }
    }

    List<Pattern> includePatterns = cmdOpts.getIncludes()
        .stream()
        .map(Pattern::compile)
        .toList();
    // Filter benchNames that match includePatterns
    benchNames.removeIf(benchName -> {
      for (Pattern includePattern : includePatterns) {
        if (includePattern.matcher(benchName).matches()) {
          return false;
        }
      }
      return true;
    });
    if (benchNames.isEmpty()) {
      System.err.println("No benchmarks to run");
      System.exit(1);
    }

    Runner jmhRunner = new Runner(cmdOpts);
    Collection<RunResult> results = Collections.emptyList();
    try {
      results = jmhRunner.run();
    } catch (RunnerException e) {
      throw new RuntimeException(e);
    }

    // TODO: Collect all the results
  }

  public void run(String label) {
    ChainedOptionsBuilder builder = new OptionsBuilder()
      .jvmArgsAppend("-Xss16M", "-Dpolyglot.engine.MultiTier=false")
      .include("^" + label + "$");
  }
}
