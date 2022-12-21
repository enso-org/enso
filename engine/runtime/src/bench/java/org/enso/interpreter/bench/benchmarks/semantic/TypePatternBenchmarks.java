package org.enso.interpreter.bench.benchmarks.semantic;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.infra.BenchmarkParams;
import org.openjdk.jmh.infra.Blackhole;

import java.io.ByteArrayOutputStream;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;


@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 3)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class TypePatternBenchmarks {
  private Value patternMatch;
  private Value avg;
  private Value vec;
  private Value self;

  @Setup
  public void initializeBenchmark(BenchmarkParams params) {
    var ctx = Context.newBuilder()
      .allowExperimentalOptions(true)
      .allowIO(true)
      .allowAllAccess(true)
      .logHandler(new ByteArrayOutputStream())
      .option(
        "enso.languageHomeOverride",
        Paths.get("../../distribution/component").toFile().getAbsolutePath()
      ).build();
    var module = ctx.eval("enso", """
        from Standard.Base import Integer, Vector, Any, Decimal

        avg arr =
            sum acc i = if i == arr.length then acc else
                @Tail_Call sum (acc + arr.at i) i+1
            (sum 0 0) / arr.length

        avg_pattern self arr pattern =
            avg (arr.map (pattern self))

        gen_vec size value =
            b = Vector.new_builder size
            b.append value
            b.append value
            add_more n = if n == size then b else
                b.append value
                @Tail_Call add_more n+1
            add_more 2 . to_vector

        match_any = v -> case v of
            n : Any -> n + 1

        match_dec = v -> case v of
            n : Decimal -> n + 1
        """);

    this.self = module.invokeMember("get_associated_type");
    Function<String,Value> getMethod = (name) -> module.invokeMember("get_method", self, name);

    var length = 100;
    this.vec = getMethod.apply("gen_vec").execute(self, length, 1.1);
    switch (params.getBenchmark().replaceFirst(".*\\.", "")) {
      case "matchOverAny":
        this.patternMatch = getMethod.apply("match_any");
        break;
      case "matchOverDecimal":
        this.patternMatch = getMethod.apply("match_dec");
        break;
      default:
        throw new IllegalStateException("Unexpected benchmark: " + params.getBenchmark());
    }
    this.avg = getMethod.apply("avg_pattern");
  }

  /**
   * Adding @ExplodeLoop in {@link org.enso.interpreter.node.controlflow.caseexpr.CatchTypeBranchNode} specialization
   * decreases the performance of this benchmark.
   */
  @Benchmark
  public void matchOverAny(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void matchOverDecimal(Blackhole matter) {
    performBenchmark(matter);
  }

  private void performBenchmark(Blackhole matter) throws AssertionError {
    var average = avg.execute(self, this.vec, this.patternMatch);
    if (!average.fitsInDouble()) {
      throw new AssertionError("Shall be a double: " + average);
    }
    var result = average.asDouble();
    if (result < 2.099 && result > 2.1) { // Precision loss due to conversion
      throw new AssertionError("Expecting the average to be 2.1: " + result);
    }
    matter.consume(result);
  }
}

