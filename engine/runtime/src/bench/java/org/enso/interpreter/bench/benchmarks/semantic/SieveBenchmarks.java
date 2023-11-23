package org.enso.interpreter.bench.benchmarks.semantic;

import java.io.ByteArrayOutputStream;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;

import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.IOAccess;
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
@Warmup(iterations = 3)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class SieveBenchmarks {
  private Value computeNthPrime;

  @Setup
  public void initializeBenchmark(BenchmarkParams params) throws Exception {
    var ctx = Context.newBuilder()
      .allowExperimentalOptions(true)
      .allowIO(IOAccess.ALL)
      .allowAllAccess(true)
      .option(
              RuntimeOptions.LOG_LEVEL,
              Level.WARNING.getName()
      )
      .logHandler(System.err)
      .option(
        "enso.languageHomeOverride",
        Paths.get("../../distribution/component").toFile().getAbsolutePath()
      ).build();

    var benchmarkName = SrcUtil.findName(params);
    var module = ctx.eval(SrcUtil.read(benchmarkName));

    this.computeNthPrime = module.invokeMember("eval_expression", "compute_nth_prime");

  }

  @Benchmark
  public void sieveWithoutTypes(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void sieve1(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void sieveAscribed(Blackhole matter) {
    performBenchmark(matter);
  }

  private void performBenchmark(Blackhole hole) throws AssertionError {
    var prime = computeNthPrime.execute(100_000);
    if (!prime.fitsInInt() || prime.asInt() != 1_299_709) {
      throw new AssertionError("Expecting correct result: " + prime);
    }
    hole.consume(prime);
  }

}

