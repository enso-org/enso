package org.enso.interpreter.bench.benchmarks.meso;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import org.enso.polyglot.MethodNames;
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

// TODO: Move to different project?
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
    try (var ctx = Context.newBuilder()
        .allowExperimentalOptions(true)
        .allowIO(true)
        .allowAllAccess(true)
        .logHandler(new ByteArrayOutputStream())
        .option(
            "enso.languageHomeOverride",
            Paths.get("../../distribution/component").toFile().getAbsolutePath()
        ).build()) {
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
}
