package org.enso.exploratory_benchmark_helpers;

import java.util.BitSet;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.problems.BlackholeProblemAggregator;
import org.enso.table.problems.ProblemAggregator;

public class LongNullHandling {
  public interface Operation {
    LongStorage run(
        LongStorage storage, LongStorage arg, MapOperationProblemAggregator problemBuilder);
  }

  public abstract static class NoNulls implements Operation {

    protected abstract long doLong(
        long a, long b, int ix, MapOperationProblemAggregator problemBuilder);

    @Override
    public LongStorage run(
        LongStorage storage, LongStorage arg, MapOperationProblemAggregator problemBuilder) {
      int n = storage.size();
      long[] newVals = new long[n];
      BitSet missing = new BitSet();
      for (int i = 0; i < n; i++) {
        if (storage.isNa(i) || arg.isNa(i)) {
          missing.set(i);
        } else {
          newVals[i] = doLong(storage.getItem(i), arg.getItem(i), i, problemBuilder);
        }
      }
      return new LongStorage(newVals, n, missing, IntegerType.INT_64);
    }
  }

  public abstract static class BoxingNulls implements Operation {

    protected abstract Long doLong(
        long a, long b, int ix, MapOperationProblemAggregator problemBuilder);

    @Override
    public LongStorage run(
        LongStorage storage, LongStorage arg, MapOperationProblemAggregator problemBuilder) {
      int n = storage.size();
      long[] newVals = new long[n];
      BitSet missing = new BitSet();
      for (int i = 0; i < n; i++) {
        if (storage.isNa(i) || arg.isNa(i)) {
          missing.set(i);
        } else {
          Long x = doLong(storage.getItem(i), arg.getItem(i), i, problemBuilder);
          if (x == null) {
            missing.set(i);
          } else {
            newVals[i] = x;
          }
        }
      }
      return new LongStorage(newVals, n, missing, IntegerType.INT_64);
    }
  }

  public abstract static class ReportingNulls implements Operation {
    static class NullityReporter {
      private boolean wasLastNull = false;

      void willBeNull() {
        wasLastNull = true;
      }
    }

    protected abstract long doLong(
        long a,
        long b,
        int ix,
        MapOperationProblemAggregator problemBuilder,
        NullityReporter nullityReporter);

    @Override
    public LongStorage run(
        LongStorage storage, LongStorage arg, MapOperationProblemAggregator problemBuilder) {
      int n = storage.size();
      long[] newVals = new long[n];
      BitSet missing = new BitSet();
      NullityReporter nullityReporter = new NullityReporter();
      for (int i = 0; i < n; i++) {
        if (storage.isNa(i) || arg.isNa(i)) {
          missing.set(i);
        } else {
          long x = doLong(storage.getItem(i), arg.getItem(i), i, problemBuilder, nullityReporter);
          if (nullityReporter.wasLastNull) {
            missing.set(i);
            nullityReporter.wasLastNull = false;
          } else {
            newVals[i] = x;
          }
        }
      }
      return new LongStorage(newVals, n, missing, IntegerType.INT_64);
    }
  }

  // Currently ignoring problem reporting in the benchmarks, for simplicity. We may want to revisit
  // this and pass a
  // proper aggregator.
  private static final ProblemAggregator parentAggregatorForBenchmarks =
      BlackholeProblemAggregator.INSTANCE;

  public static LongStorage runNoNulls(LongStorage arg1, LongStorage arg2) {
    MapOperationProblemAggregator problemBuilder =
        new MapOperationProblemAggregator(parentAggregatorForBenchmarks, null);
    NoNulls operation =
        new NoNulls() {
          @Override
          protected long doLong(
              long a, long b, int ix, MapOperationProblemAggregator problemBuilder) {
            if (b == 0) {
              problemBuilder.reportDivisionByZero(ix);
              return 0;
            } else {
              return a / b;
            }
          }
        };

    return operation.run(arg1, arg2, problemBuilder);
  }

  public static LongStorage runBoxingNulls(LongStorage arg1, LongStorage arg2) {
    MapOperationProblemAggregator problemBuilder =
        new MapOperationProblemAggregator(parentAggregatorForBenchmarks, null);
    BoxingNulls operation =
        new BoxingNulls() {
          @Override
          protected Long doLong(
              long a, long b, int ix, MapOperationProblemAggregator problemBuilder) {
            if (b == 0) {
              problemBuilder.reportDivisionByZero(ix);
              return null;
            } else {
              return a / b;
            }
          }
        };

    return operation.run(arg1, arg2, problemBuilder);
  }

  public static LongStorage runReportingNulls(LongStorage arg1, LongStorage arg2) {
    MapOperationProblemAggregator problemBuilder =
        new MapOperationProblemAggregator(parentAggregatorForBenchmarks, null);
    ReportingNulls operation =
        new ReportingNulls() {
          @Override
          protected long doLong(
              long a,
              long b,
              int ix,
              MapOperationProblemAggregator problemBuilder,
              NullityReporter nullityReporter) {
            if (b == 0) {
              problemBuilder.reportDivisionByZero(ix);
              nullityReporter.willBeNull();
              return 0;
            } else {
              return a / b;
            }
          }
        };

    return operation.run(arg1, arg2, problemBuilder);
  }
}
