package org.enso.interpreter.bench.benchmarks.semantic;

import java.io.ByteArrayOutputStream;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.logging.Level;

import org.enso.polyglot.RuntimeOptions;
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
public class ListBenchmarks {
  private final int LENGTH_OF_EXPERIMENT = 1_000_000;
  private Value list;
  private Value plusOne;
  private Value self;
  private Object zero;
  private Value sum;
  private Value oldSum;

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

    var benchmarkName = SrcUtil.findName(params);
    var code = """
      from Standard.Base.Any import Any
      from Standard.Base.Data.List.List import Cons, Nil
      from Standard.Base.Data.Text import Text
      from Standard.Base.Data.Numbers import Float
      from Standard.Base import Integer
      import Standard.Base.IO

      type V
          Val (a : Integer)

          zero = V.Val 0

          plus_v self (other : V) = V.Val self.a+other.a

          sum_int list (acc:V) =
              case list of
                  Nil -> acc.a
                  Cons x xs -> @Tail_Call V.sum_int xs (acc.plus_v (V.Val x))

          sum_conv list (acc:V) =
              case list of
                  Nil -> acc.a
                  Cons x xs -> @Tail_Call V.sum_conv xs (acc.plus_v x)

      v_zero = V.zero
      v_sum_int = V.sum_int
      v_sum_conv = V.sum_conv

      V.from (that : Integer) = V.Val that

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

      sum_any list (acc:Any) =
          case list of
              Nil -> acc
              Cons x xs -> @Tail_Call sum_any xs acc+x

      sum_int list (acc:Integer) =
          case list of
              Nil -> acc
              Cons x xs -> @Tail_Call sum_int xs acc+x

      sum_multi list (acc:Text|Float|Integer|Any) =
          case list of
              Nil -> acc
              Cons x xs -> @Tail_Call sum_multi xs acc+x

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
      case "mapOverList" ->  {
        this.list = getMethod.apply("generator").execute(self, LENGTH_OF_EXPERIMENT);
        this.zero = 0;
        this.sum = getMethod.apply("sum");
        this.oldSum = sum.execute(self, this.list, zero);
        if (!this.oldSum.fitsInLong()) {
          throw new AssertionError("Expecting a number " + this.oldSum);
        }
      }
      case "mapAnyOverList" ->  {
        this.list = getMethod.apply("generator").execute(self, LENGTH_OF_EXPERIMENT);
        this.zero = 0;
        this.sum = getMethod.apply("sum_any");
        this.oldSum = sum.execute(self, this.list, this.zero);
        if (!this.oldSum.fitsInLong()) {
          throw new AssertionError("Expecting a number " + this.oldSum);
        }
      }
      case "mapMultiOverList" ->  {
        this.list = getMethod.apply("generator").execute(self, LENGTH_OF_EXPERIMENT);
        this.zero = 0;
        this.sum = getMethod.apply("sum_multi");
        this.oldSum = sum.execute(self, this.list, this.zero);
        if (!this.oldSum.fitsInLong()) {
          throw new AssertionError("Expecting a number " + this.oldSum);
        }
      }
      case "mapIntegerOverList" ->  {
        this.list = getMethod.apply("generator").execute(self, LENGTH_OF_EXPERIMENT);
        this.zero = 0;
        this.sum = getMethod.apply("sum_int");
        this.oldSum = sum.execute(self, this.list, this.zero);
        if (!this.oldSum.fitsInLong()) {
          throw new AssertionError("Expecting a number " + this.oldSum);
        }
      }
      case "mapVOverList" ->  {
        this.list = getMethod.apply("generator").execute(self, LENGTH_OF_EXPERIMENT);
        this.zero = getMethod.apply("v_zero").execute(self);
        this.sum = getMethod.apply("v_sum_int");
        this.oldSum = sum.execute(self, this.list, this.zero);
        if (!this.oldSum.fitsInLong()) {
          throw new AssertionError("Expecting a number " + this.oldSum);
        }
      }
      case "mapConvOverList" ->  {
        this.list = getMethod.apply("generator").execute(self, LENGTH_OF_EXPERIMENT);
        this.zero = getMethod.apply("v_zero").execute(self);
        this.sum = getMethod.apply("v_sum_conv");
        this.oldSum = sum.execute(self, this.list, this.zero);
        if (!this.oldSum.fitsInLong()) {
          throw new AssertionError("Expecting a number " + this.oldSum);
        }
      }
      case "mapOverLazyList" ->  {
        this.list = getMethod.apply("lenivy_generator").execute(self, LENGTH_OF_EXPERIMENT);
        this.zero = 0;
        this.sum = getMethod.apply("leniva_suma");
        this.oldSum = sum.execute(self, this.list, this.zero);
        if (!this.oldSum.fitsInLong()) {
          throw new AssertionError("Expecting a number " + this.oldSum);
        }
      }
      default -> throw new IllegalStateException("Unexpected benchmark: " + params.getBenchmark());
    }
  }

  @Benchmark
  public void mapOverList(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void mapAnyOverList(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void mapIntegerOverList(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void mapMultiOverList(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void mapVOverList(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void mapConvOverList(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void mapOverLazyList(Blackhole matter) {
    performBenchmark(matter);
  }

  private void performBenchmark(Blackhole hole) throws AssertionError {
    var newList = plusOne.execute(self, list);
    var newSum = sum.execute(self, newList, zero);

    var result = newSum.asLong() - oldSum.asLong();
    if (result != LENGTH_OF_EXPERIMENT) {
      throw new AssertionError("Unexpected result " + result);
    }
    hole.consume(result);
  }
}

