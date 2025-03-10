package org.enso.exploratory_benchmark_helpers;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.problems.BlackholeProblemAggregator;
import org.enso.table.problems.ProblemAggregator;

public class LongNullHandling {
  public interface Operation {
    Storage<Long> run(
        ColumnLongStorage storage,
        ColumnLongStorage arg,
        MapOperationProblemAggregator problemAggregator);
  }

  public abstract static class NoNulls implements Operation {

    protected abstract long doLong(
        long a, long b, long ix, MapOperationProblemAggregator problemAggregator);

    @Override
    public Storage<Long> run(
        ColumnLongStorage storage,
        ColumnLongStorage arg,
        MapOperationProblemAggregator problemAggregator) {
      long n = storage.getSize();
      var builder = Builder.getForLong(IntegerType.INT_64, n, problemAggregator);
      for (long i = 0; i < n; i++) {
        if (storage.isNothing(i) || arg.isNothing(i)) {
          builder.appendNulls(1);
        } else {
          builder.appendLong(
              doLong(storage.getItemAsLong(i), arg.getItemAsLong(i), i, problemAggregator));
        }
      }
      return builder.seal();
    }
  }

  public abstract static class BoxingNulls implements Operation {

    protected abstract Long doLong(
        long a, long b, long ix, MapOperationProblemAggregator problemAggregator);

    @Override
    public Storage<Long> run(
        ColumnLongStorage storage,
        ColumnLongStorage arg,
        MapOperationProblemAggregator problemAggregator) {
      long n = storage.getSize();
      var builder = Builder.getForLong(IntegerType.INT_64, n, problemAggregator);
      for (long i = 0; i < n; i++) {
        if (storage.isNothing(i) || arg.isNothing(i)) {
          builder.appendNulls(1);
        } else {
          Long x = doLong(storage.getItemAsLong(i), arg.getItemAsLong(i), i, problemAggregator);
          if (x == null) {
            builder.appendNulls(1);
          } else {
            builder.appendLong(x);
          }
        }
      }
      return builder.seal();
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
        long ix,
        MapOperationProblemAggregator problemAggregator,
        NullityReporter nullityReporter);

    @Override
    public Storage<Long> run(
        ColumnLongStorage storage,
        ColumnLongStorage arg,
        MapOperationProblemAggregator problemAggregator) {
      long n = storage.getSize();
      var builder = Builder.getForLong(IntegerType.INT_64, n, problemAggregator);
      NullityReporter nullityReporter = new NullityReporter();
      for (long i = 0; i < n; i++) {
        if (storage.isNothing(i) || arg.isNothing(i)) {
          builder.appendNulls(1);
        } else {
          long x =
              doLong(
                  storage.getItemAsLong(i),
                  arg.getItemAsLong(i),
                  i,
                  problemAggregator,
                  nullityReporter);
          if (nullityReporter.wasLastNull) {
            builder.appendNulls(1);
            nullityReporter.wasLastNull = false;
          } else {
            builder.appendLong(x);
          }
        }
      }
      return builder.seal();
    }
  }

  // Currently ignoring problem reporting in the benchmarks, for simplicity. We may want to revisit
  // this and pass a
  // proper aggregator.
  private static final ProblemAggregator parentAggregatorForBenchmarks =
      BlackholeProblemAggregator.INSTANCE;

  public static Storage<Long> runNoNulls(ColumnLongStorage arg1, ColumnLongStorage arg2) {
    MapOperationProblemAggregator problemAggregator =
        new MapOperationProblemAggregator(parentAggregatorForBenchmarks, null);
    NoNulls operation =
        new NoNulls() {
          @Override
          protected long doLong(
              long a, long b, long ix, MapOperationProblemAggregator problemAggregator) {
            if (b == 0) {
              // ToDo: ProblemAggregator should accept a long instead of an int.
              problemAggregator.reportDivisionByZero((int) ix);
              return 0;
            } else {
              return a / b;
            }
          }
        };

    return operation.run(arg1, arg2, problemAggregator);
  }

  public static Storage<Long> runBoxingNulls(ColumnLongStorage arg1, ColumnLongStorage arg2) {
    MapOperationProblemAggregator problemAggregator =
        new MapOperationProblemAggregator(parentAggregatorForBenchmarks, null);
    BoxingNulls operation =
        new BoxingNulls() {
          @Override
          protected Long doLong(
              long a, long b, long ix, MapOperationProblemAggregator problemAggregator) {
            if (b == 0) {
              // ToDo: ProblemAggregator should accept a long instead of an int.
              problemAggregator.reportDivisionByZero((int) ix);
              return null;
            } else {
              return a / b;
            }
          }
        };

    return operation.run(arg1, arg2, problemAggregator);
  }

  public static Storage<Long> runReportingNulls(ColumnLongStorage arg1, ColumnLongStorage arg2) {
    MapOperationProblemAggregator problemAggregator =
        new MapOperationProblemAggregator(parentAggregatorForBenchmarks, null);
    ReportingNulls operation =
        new ReportingNulls() {
          @Override
          protected long doLong(
              long a,
              long b,
              long ix,
              MapOperationProblemAggregator problemAggregator,
              NullityReporter nullityReporter) {
            if (b == 0) {
              // ToDo: ProblemAggregator should accept a long instead of an int.
              problemAggregator.reportDivisionByZero((int) ix);
              nullityReporter.willBeNull();
              return 0;
            } else {
              return a / b;
            }
          }
        };

    return operation.run(arg1, arg2, problemAggregator);
  }
}
