package org.enso.interpreter.bench.benchmarks.semantic;

import java.io.ByteArrayOutputStream;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
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
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class StringBenchmarks {
  private Value vec;
  private Value self;
  private Value allLength;

  @Setup
  public void initializeBenchmark(BenchmarkParams params) {
    Engine eng = Engine.newBuilder()
      .allowExperimentalOptions(true)
      .logHandler(new ByteArrayOutputStream())
      .option(
        "enso.languageHomeOverride",
        Paths.get("../../distribution/component").toFile().getAbsolutePath()
      ).build();
    var ctx = Context.newBuilder()
      .engine(eng)
      .allowIO(true)
      .allowAllAccess(true)
      .build();
    var module = ctx.eval("enso", """
        from Standard.Base import all

        all_length v = v.fold 0 (sum -> str -> sum + str.length)

        create rep len =
            s = "Long string".repeat rep
            v = Vector.new len (_ -> s)
            v
        """);

    this.self = module.invokeMember("get_associated_type");
    Function<String,Value> getMethod = (name) -> module.invokeMember("get_method", self, name);

    var repeat = 2000;
    var length = 1000;
    this.vec = getMethod.apply("create").execute(self, repeat, length);
    this.allLength = getMethod.apply("all_length");
  }

  @Benchmark
  public void lengthOfStrings(Blackhole matter) {
    var average = allLength.execute(vec, vec);
    if (!average.fitsInLong()) {
      throw new AssertionError("Shall be a number: " + average);
    }
    var result = average.asLong();
    boolean isResultCorrect = result == 22000000;
    if (!isResultCorrect) {
      throw new AssertionError("Expecting reasonable result but was " + result);
    }
    matter.consume(result);
  }
}

