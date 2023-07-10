package org.enso.interpreter.bench.benchmarks.meso;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import org.enso.polyglot.MethodNames;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;
import org.openjdk.jmh.infra.BenchmarkParams;
import org.openjdk.jmh.infra.Blackhole;

@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 3, time=3)
@Measurement(iterations = 3, time=1)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class VectorOperations {
  private Value code;

  @Setup
  public void initializeBenchmark(BenchmarkParams params) throws Exception {
    File sourceFile;
    for (var folder = Paths.get("../../distribution/component").toFile();; folder = folder.getParentFile()) {
      sourceFile = fileFrom(folder, "test", "Benchmarks", "src", "Vector", "Operations.enso");
      if (sourceFile.canRead()) {
        break;
      }
    }
    sourceFile = sourceFile.getCanonicalFile();
    var projectRoot = sourceFile.getParentFile().getParentFile().getParent();

    var ctx = Context.newBuilder()
      .allowExperimentalOptions(true)
      .allowIO(true)
      .allowAllAccess(true)
      .logHandler(new ByteArrayOutputStream())
      .option("enso.projectRoot", projectRoot).option(
        "enso.languageHomeOverride",
        Paths.get("../../distribution/component").toFile().getAbsolutePath()
      ).build();

    var src = Source.newBuilder("enso", sourceFile).build();
    var module = ctx.eval(src);
    var all = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "bench_suites").as(All.class);
    var allSuites = new ArrayList<Suite>();
    for (var g : all.groups()) {
      for (var s : g.specs()) {
        allSuites.add(s);
      }
    }

    var shortName = params.getBenchmark().replaceAll(".*\\.", "");

    Predicate<Suite> matches = (s) -> {
      var name = s.name().replaceAll(" ", "");
      return shortName.equalsIgnoreCase(name);
    };

    var firstOption = allSuites.stream().filter(matches).findFirst();
    if (firstOption.isEmpty()) {
      var names = allSuites.stream().map(Suite::name).collect(Collectors.toList());
      throw new IllegalStateException("Cannot find suite: " + shortName + " among: " + names);
    }
    var found = firstOption.get();
    if (found.code() == null) {
      throw new NullPointerException("no code member: " + found);
    }
    this.code = found.code();
  }

  public static interface All {
    List<Group> groups();
  }

  public static interface Group {
    String name();
    List<Suite> specs();
  }

  public static interface Suite {
    String name();
    Value code();
  }

  private static File fileFrom(File folder, String... children) {
    File f = folder;
    for (int i = 0; i < children.length; i++) {
        f = new File(f, children[i]);
    }
    return f;
  }

  @Benchmark
  public void newVector(Blackhole blackhole) {
    benchmark(blackhole);
  }

  @Benchmark
  public void newConstant(Blackhole blackhole) {
    benchmark(blackhole);
  }

  @Benchmark
  public void newRandom(Blackhole blackhole) {
    benchmark(blackhole);
  }

  @Benchmark
  public void sum(Blackhole blackhole) {
    benchmark(blackhole);
  }

  @Benchmark
  public void sumStatistic(Blackhole blackhole) {
    benchmark(blackhole);
  }

  protected final void benchmark(Blackhole blackhole) {
    var result = code.execute(0);
    blackhole.consume(result);
  }
}
