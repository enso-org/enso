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
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class ListBenchmarks {
  private final int LENGTH_OF_EXPERIMENT = 1_000_000;
  private Value list;
  private Value plusOne;
  private Value self;
  private Value sum;
  private Value oldSum;

  @Setup
  public void initializeBenchmark(BenchmarkParams params) throws Exception {
    var ctx = Context.newBuilder()
      .allowExperimentalOptions(true)
      .allowIO(true)
      .allowAllAccess(true)
      .logHandler(new ByteArrayOutputStream())
      .option(
        "enso.languageHomeOverride",
        Paths.get("../../distribution/component").toFile().getAbsolutePath()
      ).build();

    var benchmarkName = SrcUtil.findName(params);
    var code = """
      from Standard.Base.Data.List.List import Cons, Nil
      import Standard.Base.IO

      type Lenivy
          Nic
          Hlava ~x ~xs

          map self fn = case self of
              Lenivy.Nic -> Lenivy.Nic
              Lenivy.Hlava x xs -> Lenivy.Hlava (fn x) (xs.map fn)

      plus_one list = list.map (x -> x + 1)

      leniva_suma list acc = case list of
          Lenivy.Nic -> acc
          Lenivy.Hlava x xs -> @Tail_Call leniva_suma xs acc+x

      lenivy_generator n =
          go x v l = if x > n then l else
              @Tail_Call go x+1 v+1 (Lenivy.Hlava v l)
          go 1 1 Lenivy.Nic

      sum list acc =
          case list of
              Nil -> acc
              Cons x xs -> @Tail_Call sum xs acc+x

      generator n =
          go x v l = if x > n then l else
              @Tail_Call go x+1 v+1 (Cons v l)
          go 1 1 Nil
      """;

    var module = ctx.eval(SrcUtil.source(benchmarkName, code));

    this.self = module.invokeMember("get_associated_type");
    Function<String,Value> getMethod = (name) -> module.invokeMember("get_method", self, name);

    this.plusOne = getMethod.apply("plus_one");

    switch (benchmarkName) {
      case "mapOverList": {
        this.list = getMethod.apply("generator").execute(self, LENGTH_OF_EXPERIMENT);
        this.sum = getMethod.apply("sum");
        this.oldSum = sum.execute(self, this.list, 0);
        if (!this.oldSum.fitsInLong()) {
          throw new AssertionError("Expecting a number " + this.oldSum);
        }
        break;
      }
      case "mapOverLazyList": {
        this.list = getMethod.apply("lenivy_generator").execute(self, LENGTH_OF_EXPERIMENT);
        this.sum = getMethod.apply("leniva_suma");
        this.oldSum = sum.execute(self, this.list, 0);
        if (!this.oldSum.fitsInLong()) {
          throw new AssertionError("Expecting a number " + this.oldSum);
        }
        break;
      }
      default:
        throw new IllegalStateException("Unexpected benchmark: " + params.getBenchmark());
    }
  }

  @Benchmark
  public void mapOverList(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void mapOverLazyList(Blackhole matter) {
    performBenchmark(matter);
  }

  private void performBenchmark(Blackhole hole) throws AssertionError {
    var newList = plusOne.execute(self, list);
    var newSum = sum.execute(self, newList, 0);

    var result = newSum.asLong() - oldSum.asLong();
    if (result != LENGTH_OF_EXPERIMENT) {
      throw new AssertionError("Unexpected result " + result);
    }
    hole.consume(result);
  }
}

