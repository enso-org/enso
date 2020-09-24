package org.enso.interpreter.bench.benchmarks.semantic;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.concurrent.TimeUnit;
import org.enso.interpreter.bench.fixtures.semantic.RecursionFixtures;
import org.enso.interpreter.test.DefaultInterpreterRunner;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Warmup;

@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
public class RecursionBenchmarks {
  private static RecursionFixtures recursionFixtures = new RecursionFixtures();

  private void runOnHundredMillion(DefaultInterpreterRunner.MainMethod main) {
    main.mainFunction().value().execute(main.mainConstructor(), recursionFixtures.hundredMillion());
  }

  public static class CRope {
    private Object left;
    private Object right;

    public CRope(Object left, Object right) {
      this.left = left;
      this.right = right;
    }

    public Object getLeft() {
      return left;
    }

    public Object getRight() {
      return right;
    }

    public String optimize() {
      Deque<Object> workStack = new ArrayDeque<>();
      StringBuilder bldr = new StringBuilder();
      workStack.push(this);
      while (!workStack.isEmpty()) {
        Object item = workStack.pop();
        if (item instanceof String) {
          bldr.append((String) item);
        } else {
          CRope rope = (CRope) item;
          workStack.push(rope.getRight());
          workStack.push(rope.getLeft());
        }
      }
      return bldr.toString();
    }
  }

  public String buildLongStr() {
    CRope cr = new CRope("", "");
    for (long i = 1; i < 1000000; i++) {
      cr = new CRope(cr, ((Long) i).toString());
    }
    return cr.optimize();
  }

  public String buildLongStrBldr() {
    StringBuilder bldr = new StringBuilder();
    for (long i = 1; i < 1000000; i++) {
      bldr.append(((Long) i).toString());
    }
    return bldr.toString();
  }

  @Benchmark
  public void benchJavaStr() {
    buildLongStr();
  }

  @Benchmark
  public void benchSumTCO() {
    runOnHundredMillion(recursionFixtures.sumTCO());
  }

  @Benchmark
  public void benchSumTCOWithEval() {
    runOnHundredMillion(recursionFixtures.sumTCOWithEval());
  }

  @Benchmark
  public void benchSumTCOFoldLike() {
    runOnHundredMillion(recursionFixtures.sumTCOFoldLike());
  }

  @Benchmark
  public void benchSumRecursive() {
    DefaultInterpreterRunner.MainMethod main = recursionFixtures.sumRecursive();
    main.mainFunction().value().execute(main.mainConstructor(), recursionFixtures.hundred());
  }

  @Benchmark
  public void benchOversaturatedRecursiveCall() {
    runOnHundredMillion(recursionFixtures.oversaturatedRecursiveCall());
  }

  @Benchmark
  public void benchSumStateTCO() {
    runOnHundredMillion(recursionFixtures.sumStateTCO());
  }

  @Benchmark
  public void benchNestedThunkSum() {
    runOnHundredMillion(recursionFixtures.nestedThunkSum());
  }
}
