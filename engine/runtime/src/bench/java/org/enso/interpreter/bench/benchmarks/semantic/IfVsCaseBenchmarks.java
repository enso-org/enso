package org.enso.interpreter.bench.benchmarks.semantic;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import org.enso.interpreter.test.TestBase;
import org.enso.polyglot.MethodNames.Module;
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
import org.openjdk.jmh.annotations.TearDown;
import org.openjdk.jmh.annotations.Warmup;
import org.openjdk.jmh.infra.BenchmarkParams;

@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 3, time = 3)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class IfVsCaseBenchmarks extends TestBase {
  private static final int INPUT_VEC_SIZE = 100_000;
  private Context ctx;
  private Value ifBench3;
  private Value caseBench3;
  private Value ifBench6;
  private Value ifBench6In;
  private Value caseBench6;
  private Value createVec;
  private Value inputVec;
  private OutputStream out = new ByteArrayOutputStream();

  @Setup
  public void initializeBench(BenchmarkParams params) throws IOException {
    ctx = Context.newBuilder("enso")
        .allowAllAccess(true)
        .logHandler(out)
        .out(out)
        .err(out)
        .allowIO(true)
        .allowExperimentalOptions(true)
        .option(
            "enso.languageHomeOverride",
            Paths.get("../../distribution/component").toFile().getAbsolutePath()
        )
        .option("engine.MultiTier", "true")
        .option("engine.BackgroundCompilation", "true")
        .build();

    var code = """
        from Standard.Base import all

        type My_Type
            Value f1 f2 f3 f4 f5 f6

        if_bench_3 : Vector My_Type -> Integer
        if_bench_3 vec =
            vec.fold 0 acc-> curr->
                if curr.f1.not then acc else
                    if curr.f2.not then acc else
                        if curr.f3.not then acc else
                            acc + 1

        case_bench_3 : Vector My_Type -> Integer
        case_bench_3 vec =
            vec.fold 0 acc-> curr->
                case curr.f1 of
                    False -> acc
                    True -> case curr.f2 of
                        False -> acc
                        True -> case curr.f3 of
                            False -> acc
                            True -> acc + 1

        if_bench_6 : Vector My_Type -> Integer
        if_bench_6 vec =
            vec.fold 0 acc-> curr->
                if curr.f1.not then acc else
                    if curr.f2.not then acc else
                        if curr.f3.not then acc else
                            if curr.f4.not then acc else
                                if curr.f5.not then acc else
                                    if curr.f6.not then acc else
                                        acc + 1

        if_bench_6_in : Vector My_Type -> Integer
        if_bench_6_in vec =
            vec.fold 0 acc-> curr->
                curr.f1.not.if_then_else acc <|
                    curr.f2.not.if_then_else acc <|
                        curr.f3.not.if_then_else acc <|
                            curr.f4.not.if_then_else acc <|
                                curr.f5.not.if_then_else acc <|
                                    curr.f6.not.if_then_else acc <|
                                        acc + 1

        case_bench_6 : Vector My_Type -> Integer
        case_bench_6 vec =
            vec.fold 0 acc-> curr->
                case curr.f1 of
                    False -> acc
                    True -> case curr.f2 of
                        False -> acc
                        True -> case curr.f3 of
                            False -> acc
                            True -> case curr.f4 of
                                False -> acc
                                True -> case curr.f5 of
                                    False -> acc
                                    True -> case curr.f6 of
                                        False -> acc
                                        True -> acc + 1

        create_vec polyglot_vec =
            Vector.from_polyglot_array polyglot_vec . map elem->
                My_Type.Value (elem.at 0) (elem.at 1) (elem.at 2) (elem.at 3) (elem.at 4) (elem.at 5)

        """;

    var benchmarkName = SrcUtil.findName(params);
    var src = SrcUtil.source(benchmarkName, code);
    Value module = ctx.eval(src);
    ifBench3 = Objects.requireNonNull(module.invokeMember(Module.EVAL_EXPRESSION, "if_bench_3"));
    caseBench3 = Objects.requireNonNull(module.invokeMember(Module.EVAL_EXPRESSION, "case_bench_3"));
    ifBench6 = Objects.requireNonNull(module.invokeMember(Module.EVAL_EXPRESSION, "if_bench_6"));
    ifBench6In = Objects.requireNonNull(module.invokeMember(Module.EVAL_EXPRESSION, "if_bench_6_in"));
    caseBench6 = Objects.requireNonNull(module.invokeMember(Module.EVAL_EXPRESSION, "case_bench_6"));
    createVec = Objects.requireNonNull(module.invokeMember(Module.EVAL_EXPRESSION, "create_vec"));
    // So far, input is a vector of My_Type.Value with all fields set to True
    inputVec = createMyTypeAllTrue(INPUT_VEC_SIZE);
  }

  @TearDown
  public void tearDown() {
    ctx.close();
  }

  /**
   * Iterates over a vector of {@code My_Type} values with True only fields.
   */
  @Benchmark
  public void ifBench3() {
    Value res = ifBench3.execute(inputVec);
    checkResult(res);
  }

  @Benchmark
  public void ifBench6() {
    Value res = ifBench6.execute(inputVec);
    checkResult(res);
  }

  @Benchmark
  public void ifBench6In() {
    Value res = ifBench6In.execute(inputVec);
    checkResult(res);
  }

  @Benchmark
  public void caseBench3() {
    Value res = caseBench3.execute(inputVec);
    checkResult(res);
  }

  @Benchmark
  public void caseBench6() {
    Value res = caseBench6.execute(inputVec);
    checkResult(res);
  }

  private static void checkResult(Value res) {
    if (res.asInt() != INPUT_VEC_SIZE) {
      throw new AssertionError("Expected result: " + INPUT_VEC_SIZE + ", got: " + res.asInt());
    }
  }

  /**
   * Creates a vector of {@code My_Type} with all True fields
   */
  private Value createMyTypeAllTrue(int size) {
    List<List<Boolean>> inputPolyVec = new ArrayList<>();
    for (int i = 0; i < size; i++) {
      inputPolyVec.add(List.of(true, true, true, true, true, true));
    }
    return createVec.execute(inputPolyVec);
  }
}
