package org.enso.interpreter.bench.benchmarks.semantic;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Random;
import java.util.concurrent.TimeUnit;
import org.enso.common.MethodNames;
import org.enso.compiler.benchmarks.Utils;
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
import org.openjdk.jmh.annotations.TearDown;
import org.openjdk.jmh.annotations.Warmup;
import org.openjdk.jmh.infra.BenchmarkParams;

@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 3, time = 3)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class WarningBenchmarks {
  private static final int INPUT_VEC_SIZE = 10_000;
  private static final int INPUT_DIFF_VEC_SIZE = 10_000;
  private Context ctx;
  private Value vecSumBench;

  private Value createVec;
  private Value mapVecWithWarnings;
  private Value noWarningsVec;
  private Value sameWarningVec;
  private Value randomVec;
  private Value randomElemsWithWarningsVec;
  private Value constElem;
  private Value constElemWithWarning;

  private String benchmarkName;

  private int randomVectorSum = 0;

  private record GeneratedVector(StringBuilder repr, int sum) {}

  private GeneratedVector generateRandomVector(
      Random random, String vectorName, long vectorSize, int maxRange) {
    List<Integer> primitiveValues = new ArrayList<>();
    random.ints(vectorSize, 0, maxRange).forEach(primitiveValues::add);
    var sb = new StringBuilder();
    sb.append(vectorName).append(" = [");
    var sum = 0;
    for (Integer intValue : primitiveValues) {
      sb.append(intValue).append(",");
      sum += Math.abs(intValue);
    }
    sb.setCharAt(sb.length() - 1, ']');
    sb.append('\n');
    return new GeneratedVector(sb, sum);
  }

  @Setup
  public void initializeBench(BenchmarkParams params) throws IOException {
    this.ctx = Utils.createDefaultContextBuilder().build();
    var random = new Random(42);

    benchmarkName = SrcUtil.findName(params);

    var benchCode =
        """
        from Standard.Base import all

        vec_sum_bench : Vector Integer -> Integer
        vec_sum_bench vec =
            vec.fold 0 (x->y->x+y)
        """;

    var setupCode =
        new StringBuilder(
            """
        from Standard.Base import all

        create_vec size elem = Runtime.no_inline <|
            Vector.fill size elem

        elem =
            42

        elem_const_with_warning = Runtime.no_inline <|
            x = 42
            Warning.attach "Foo!" x

        elem_with_warning v = Runtime.no_inline <|
            Warning.attach "Foo!" v

        map_vector_with_warnings vec = Runtime.no_inline <|
            vec.map (e-> elem_with_warning e)
        """);

    // generate random vector
    var randomIntVectorName = "vector_with_random_values";
    var vectorWithRandomValues =
        generateRandomVector(random, randomIntVectorName, INPUT_DIFF_VEC_SIZE, 3_000);
    setupCode.append(vectorWithRandomValues.repr());
    randomVectorSum = vectorWithRandomValues.sum();

    var setupSrc = SrcUtil.source(benchmarkName + "_Setup", setupCode.toString());
    Value setupModule = ctx.eval(setupSrc);
    createVec =
        Objects.requireNonNull(
            setupModule.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "create_vec"));
    mapVecWithWarnings =
        Objects.requireNonNull(
            setupModule.invokeMember(
                MethodNames.Module.EVAL_EXPRESSION, "map_vector_with_warnings"));
    constElem =
        Objects.requireNonNull(
            setupModule.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "elem"));
    constElemWithWarning =
        Objects.requireNonNull(
            setupModule.invokeMember(
                MethodNames.Module.EVAL_EXPRESSION, "elem_const_with_warning"));
    noWarningsVec = createVec.execute(INPUT_VEC_SIZE, constElem);
    sameWarningVec = createVec.execute(INPUT_VEC_SIZE, constElemWithWarning);
    randomVec =
        Objects.requireNonNull(
            setupModule.invokeMember(MethodNames.Module.EVAL_EXPRESSION, randomIntVectorName));
    randomElemsWithWarningsVec = mapVecWithWarnings.execute(randomVec);

    var benchSrc = SrcUtil.source(benchmarkName + "_Bench", benchCode);
    Value benchModule = ctx.eval(benchSrc);
    vecSumBench =
        Objects.requireNonNull(
            benchModule.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "vec_sum_bench"));
  }

  @TearDown
  public void cleanup() {
    ctx.close(true);
  }

  @Benchmark
  public void noWarningsVecSum() {
    Value res = vecSumBench.execute(noWarningsVec);
    checkResult(res, INPUT_VEC_SIZE * 42);
  }

  @Benchmark
  public void sameWarningVecSum() {
    Value res = vecSumBench.execute(sameWarningVec);
    checkResult(res, INPUT_VEC_SIZE * 42);
  }

  @Benchmark
  public void randomElementsVecSum() {
    Value res = vecSumBench.execute(randomVec);
    checkResult(res, randomVectorSum);
  }

  @Benchmark
  public void diffWarningRandomElementsVecSum() {
    Value res = vecSumBench.execute(randomElemsWithWarningsVec);
    checkResult(res, randomVectorSum);
  }

  private static void checkResult(Value res, int expected) {
    if (res.asInt() != expected) {
      throw new AssertionError("Expected result: " + INPUT_VEC_SIZE * 42 + ", got: " + res.asInt());
    }
  }
}
