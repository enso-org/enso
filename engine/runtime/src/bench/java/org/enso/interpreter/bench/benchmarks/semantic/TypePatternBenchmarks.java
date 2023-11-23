package org.enso.interpreter.bench.benchmarks.semantic;

import org.enso.polyglot.MethodNames.Module;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.IOAccess;
import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.infra.BenchmarkParams;
import org.openjdk.jmh.infra.Blackhole;

import java.io.ByteArrayOutputStream;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.logging.Level;


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
    var code ="""
        from Standard.Base import Integer, Vector, Any, Float

        avg arr =
            sum acc i = if i == arr.length then acc else
                @Tail_Call sum (acc + arr.at i) i+1
            (sum 0 0) / arr.length

        avg_pattern arr pattern =
            avg (arr.map (pattern _))

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

        match_float = v -> case v of
            n : Float -> n + 1
        """;
    var benchmarkName = SrcUtil.findName(params);
    var src = SrcUtil.source(benchmarkName, code);
    var module = ctx.eval(src);

    Function<String,Value> getMethod = (name) -> module.invokeMember(Module.EVAL_EXPRESSION, name);

    var length = 100;
    this.vec = getMethod.apply("gen_vec").execute(length, 1.1);
    switch (SrcUtil.findName(params)) {
      case "matchOverAny" -> this.patternMatch = getMethod.apply("match_any");
      case "matchOverDecimal" -> this.patternMatch = getMethod.apply("match_float");
      default -> throw new IllegalStateException("Unexpected benchmark: " + params.getBenchmark());
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

  /**
   * Benchmark that matches over a Float. The old (decimal) name is kept to keep the history of results consistent.
   */
  @Benchmark
  public void matchOverDecimal(Blackhole matter) {
    performBenchmark(matter);
  }

  private void performBenchmark(Blackhole matter) throws AssertionError {
    var average = avg.execute(this.vec, this.patternMatch);
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

