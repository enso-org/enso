package org.enso.interpreter.bench.benchmarks.semantic;

import org.enso.interpreter.test.TestBase;
import org.enso.polyglot.MethodNames;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.infra.BenchmarkParams;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Paths;
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

        var file = File.createTempFile("warnings", ".enso");
        try (var w = new FileWriter(file)) {
            w.write(code);
        }
        var src = Source.newBuilder("enso", file).build();
        Value module = ctx.eval(src);
        vecSumBench = Objects.requireNonNull(module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "vec_sum_bench"));
        createVec = Objects.requireNonNull(module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "create_vec"));
        elem = Objects.requireNonNull(module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "elem"));
        elemWithWarning = Objects.requireNonNull(module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "elem_with_warning"));
        noWarningsVec = createVec.execute(INPUT_VEC_SIZE, elem);
        sameWarningVec = createVec.execute(INPUT_VEC_SIZE, elemWithWarning);
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
