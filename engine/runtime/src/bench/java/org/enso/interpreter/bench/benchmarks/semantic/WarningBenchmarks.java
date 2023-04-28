package org.enso.interpreter.bench.benchmarks.semantic;

import org.enso.interpreter.test.TestBase;
import org.enso.polyglot.MethodNames;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.TearDown;
import org.openjdk.jmh.annotations.Warmup;
import org.openjdk.jmh.infra.BenchmarkParams;

import java.io.IOException;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 3, time = 3)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class WarningBenchmarks extends TestBase {
    private static final int INPUT_VEC_SIZE = 10000;
    private Context ctx;
    private Value vecSumBench;

    private Value createVec;
    private Value noWarningsVec;
    private Value sameWarningVec;

    private Value elem;

    private Value elemWithWarning;

    private String benchmarkName;

    @Setup
    public void initializeBench(BenchmarkParams params) throws IOException {
        ctx = createDefaultContext();

        benchmarkName = params.getBenchmark().replaceFirst(".*\\.", "");

        var code = """
        from Standard.Base import all

        vec_sum_bench : Vector Integer -> Integer
        vec_sum_bench vec =
            vec.fold 0 (x->y->x+y)

        create_vec size elem =
            Vector.fill size elem
            
        elem =
            42
            
        elem_with_warning =
            x = 42
            Warning.attach "Foo!" x
        """;
        var src = SrcUtil.source(benchmarkName, code);
        Value module = ctx.eval(src);
        vecSumBench = Objects.requireNonNull(module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "vec_sum_bench"));
        createVec = Objects.requireNonNull(module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "create_vec"));
        elem = Objects.requireNonNull(module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "elem"));
        elemWithWarning = Objects.requireNonNull(module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "elem_with_warning"));
        noWarningsVec = createVec.execute(INPUT_VEC_SIZE, elem);
        sameWarningVec = createVec.execute(INPUT_VEC_SIZE, elemWithWarning);
    }

    @TearDown
    public void cleanup() {
        ctx.close(true);
    }

    @Benchmark
    public void noWarningsVecSum() {
        Value res = vecSumBench.execute(noWarningsVec);
        checkResult(res);
    }

    @Benchmark
    public void sameWarningVecSum() {
        Value res = vecSumBench.execute(sameWarningVec);
        checkResult(res);
    }

    private static void checkResult(Value res) {
        if (res.asInt() != INPUT_VEC_SIZE*42) {
            throw new AssertionError("Expected result: " + INPUT_VEC_SIZE*42 + ", got: " + res.asInt());
        }
    }

}
