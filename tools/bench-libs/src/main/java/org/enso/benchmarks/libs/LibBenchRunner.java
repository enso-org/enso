package org.enso.benchmarks.libs;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import org.enso.polyglot.MethodNames.Module;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.openjdk.jmh.results.RunResult;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.ChainedOptionsBuilder;
import org.openjdk.jmh.runner.options.CommandLineOptionException;
import org.openjdk.jmh.runner.options.CommandLineOptions;
import org.openjdk.jmh.runner.options.OptionsBuilder;

public class LibBenchRunner {
  private static Context ctx;

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
      // TODO: Print help
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
    if (cmdOpts.shouldList()) {
      // TODO: List benchmarks
      System.exit(0);
    }
    List<String> includes = cmdOpts.getIncludes();


    File benchRootDir = Paths.get("../../distribution/component/test/Benchmarks").toFile();

    initCtx();
    var specCollector = new SpecCollector(ctx, rootDir);
    specCollector.collectBenchSpecsFromSingleFile()
    Set<BenchSuite> benchSpecs = collectAllBenchSpecs();
    for (BenchSuite benchSpec : benchSpecs) {
      for (BenchGroup group : benchSpec.groups()) {
        System.out.println("Discovered group: " + group.name());
      }
    }

    // Merge cmdOpts with chainedOptsBuilder
    ChainedOptionsBuilder optsBuilder = new OptionsBuilder()
        .include(cmdOpts.getIncludes().get(0));

    Runner jmhRunner = new Runner(optsBuilder.build());
    Collection<RunResult> results = Collections.emptyList();
    try {
      results = jmhRunner.run();
    } catch (RunnerException e) {
      throw new RuntimeException(e);
    }

    // TODO: Collect all the results
    closeCtx();
  }

  private static void initCtx() {
    ctx = Context.newBuilder()
        .allowExperimentalOptions(true)
        .allowIO(true)
        .allowAllAccess(true)
        .logHandler(new ByteArrayOutputStream())
        .option(
            "enso.languageHomeOverride",
            Paths.get("../../distribution/component").toFile().getAbsolutePath()
        ).build();
  }

  private static void closeCtx() {
    ctx.close();
  }

  public void run(String label) {
    ChainedOptionsBuilder builder = new OptionsBuilder()
      .jvmArgsAppend("-Xss16M", "-Dpolyglot.engine.MultiTier=false")
      .include("^" + label + "$");
  }

  private static Set<BenchSuite> collectAllBenchSpecs() {
    String benchmarksProjectRootDir = "test/Benchmarks/src";
    File singleFile = new File("/home/pavel/dev/enso/engine/runtime/src/bench/resources/org.enso.interpreter.bench.benchmarks.meso/simple.enso");
    return Set.of(collectBenchSpecsFromSingleFile(singleFile));
  }

  private static BenchSuite collectBenchSpecsFromSingleFile(File benchFile) {
    assert benchFile.exists();
    assert benchFile.canRead();
    Source source;
    try {
      source = Source.newBuilder("enso", benchFile).build();
    } catch (IOException e) {
      throw new IllegalStateException("Unreachable", e);
    }
    Value module = ctx.eval(source);
    var benchSuite = module
        .invokeMember(Module.EVAL_EXPRESSION, "all")
        .as(BenchSuite.class);
    return benchSuite;
  }
}
