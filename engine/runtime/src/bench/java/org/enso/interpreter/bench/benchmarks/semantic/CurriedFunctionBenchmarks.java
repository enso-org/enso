package org.enso.interpreter.bench.benchmarks.semantic;

import java.io.ByteArrayOutputStream;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import org.graalvm.polyglot.Context;
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
@Warmup(iterations = 3)
@Measurement(iterations = 3)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class CurriedFunctionBenchmarks {
  private Value fn;
  private Value avg;

  @Setup
  public void initializeBenchmark(BenchmarkParams params) throws Exception {
    var ctx = Context.newBuilder()
      .allowExperimentalOptions(true)
      .logHandler(new ByteArrayOutputStream())
      .allowIO(true)
      .allowAllAccess(true)
      .option(
        "enso.languageHomeOverride",
        Paths.get("../../distribution/component").toFile().getAbsolutePath()
      ).build();

    var benchmarkName = SrcUtil.findName(params);
    var code = """
        avg fn len =
            sum acc i = if i == len then acc else
                sum (acc + fn i) i+1
            (sum 0 0) / len

        type Holder
            three_times x = 3 * x

        callback_curried =
            h = Holder
            h.three_times

        callback_lambda =
            h = Holder
            (x -> h.three_times x)
        """;

    var module = ctx.eval(SrcUtil.source(benchmarkName, code));

    Function<String,Value> getMethod = (name) -> module.invokeMember("eval_expression", name);
    switch (benchmarkName) {
      case "averageLambda": {
        this.fn = getMethod.apply("callback_lambda");
        break;
      }
      case "averageCurried": {
        this.fn = getMethod.apply("callback_curried");
        break;
      }
      default:
        throw new IllegalStateException("Unexpected benchmark: " + params.getBenchmark());
    }
    this.avg = getMethod.apply("avg");
  }

  @Benchmark
  public void averageLambda(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void averageCurried(Blackhole matter) {
    performBenchmark(matter);
  }

  private void performBenchmark(Blackhole hole) throws AssertionError {
    var average = avg.execute(fn, 10000);
    if (!average.fitsInDouble()) {
      throw new AssertionError("Shall be a double: " + average);
    }
    var result = (long) average.asDouble();
    boolean isResultCorrect = result == 14998;
    if (!isResultCorrect) {
      throw new AssertionError("Expecting reasonable average but was " + result + "\n" + fn);
    }
    hole.consume(result);
  }
}

