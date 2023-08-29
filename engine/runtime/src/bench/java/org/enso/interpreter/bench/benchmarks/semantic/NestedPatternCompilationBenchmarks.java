package org.enso.interpreter.bench.benchmarks.semantic;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.AbstractList;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
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
public class NestedPatternCompilationBenchmarks {
    private Value self;
    private String benchmarkName;
    private String code;
    private Context ctx;

    @Setup
    public void initializeBenchmark(BenchmarkParams params) throws Exception {
        ctx = Context.newBuilder()
                .allowExperimentalOptions(true)
                .allowIO(IOAccess.ALL)
                .allowAllAccess(true)
                .logHandler(new ByteArrayOutputStream())
                .option(
                        "enso.languageHomeOverride",
                        Paths.get("../../distribution/component").toFile().getAbsolutePath()
                ).build();

        benchmarkName = SrcUtil.findName(params);
        code = """
            type List
                Cons a b
                Nil

            test x =
                case x of
                    List.Nil -> 0
                    List.Cons a List.Nil -> a
                    List.Cons a (List.Cons b List.Nil) -> a+b
                    List.Cons a (List.Cons b (List.Cons c List.Nil)) -> a+b+c
                    List.Cons a (List.Cons b (List.Cons c (List.Cons d List.Nil))) -> a+b+c+d
                    List.Cons a (List.Cons b (List.Cons c (List.Cons d (List.Cons e List.Nil)))) -> a+b+c+d+e
                    List.Cons a (List.Cons b (List.Cons c (List.Cons d (List.Cons e (List.Cons f List.Nil))))) -> a+b+c+d+e+f

            list_of_6 =
                List.Cons 1 (List.Cons 2 (List.Cons 3 (List.Cons 4 (List.Cons 5 (List.Cons 6 List.Nil)))))
        """;
    }

    @Benchmark
    public void sumList(Blackhole hole) throws IOException {
        // Compilation is included in the benchmark on purpose
        var module = ctx.eval(SrcUtil.source(benchmarkName, code));

        this.self = module.invokeMember("get_associated_type");
        Function<String,Value> getMethod = (name) -> module.invokeMember("get_method", self, name);

        var list = getMethod.apply("list_of_6").execute(self);
        var result = getMethod.apply("test").execute(self, list);

        if (!result.fitsInDouble()) {
            throw new AssertionError("Shall be a double: " + result);
        }
        var calculated = (long) result.asDouble();
        var expected = 21;
        if (calculated != expected) {
            throw new AssertionError("Expected " + expected + " from sum but got " + calculated);
        }
        hole.consume(result);
    }

}

