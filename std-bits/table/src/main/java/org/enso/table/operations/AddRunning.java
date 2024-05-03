package org.enso.table.operations;

import java.util.BitSet;
import org.enso.base.polyglot.NumericConverter;
import org.enso.base.statistics.Statistic;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.IgnoredNaN;
import org.enso.table.data.table.problems.IgnoredNothing;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;

public class AddRunning {

  public static Storage<?> create_running(
      Statistic statistic,
      Column sourceColumn,
      Column[] groupingColumns,
      Column[] orderingColumns,
      int[] directions,
      ProblemAggregator problemAggregator) {
    var runningStatistic = createRunningStatistic(statistic, sourceColumn, problemAggregator);
    RunningLooper.loop(
        groupingColumns,
        orderingColumns,
        directions,
        problemAggregator,
        runningStatistic,
        sourceColumn.getSize());
    return runningStatistic.getResult();
  }

  private static RunningStatistic<?> createRunningStatistic(
      Statistic statistic, Column sourceColumn, ProblemAggregator problemAggregator) {
    switch (statistic) {
      case Sum -> {
        return new RunningSumStatistic(sourceColumn, problemAggregator);
      }
      case Mean -> {
        return new RunningMeanStatistic(sourceColumn, problemAggregator);
      }
      case Minimum -> {
        return new RunningMinStatistic(sourceColumn, problemAggregator);
      }
      case Maximum -> {
        if (sourceColumn.getStorage().getType() == IntegerType.INT_64) {
          return new RunningMaxLongStatistic(sourceColumn, problemAggregator);
          // return new RunningMaxStatistic<long>(sourceColumn, problemAggregator);
        }
        return new RunningMaxStatistic(sourceColumn, problemAggregator);
        // return new RunningMaxStatistic<double>(sourceColumn, problemAggregator);
      }
      default -> throw new IllegalArgumentException("Unsupported statistic: " + statistic);
    }
  }

  private interface TypeHandler<T> {

    T tryConvertingToType(Object o);

    long typeToRawLongBits(T t);

    Storage<T> createStorage(long[] result, int size, BitSet isNothing);
  }

  private static class DoubleHandler implements TypeHandler<Double> {

    @Override
    public Double tryConvertingToType(Object o) {
      return NumericConverter.tryConvertingToDouble(o);
    }

    @Override
    public long typeToRawLongBits(Double d) {
      return Double.doubleToRawLongBits(d);
    }

    @Override
    public Storage<Double> createStorage(long[] result, int size, BitSet isNothing) {
      return new DoubleStorage(result, size, isNothing);
    }
  }

  private static class LongHandler implements TypeHandler<Long> {

    @Override
    public Long tryConvertingToType(Object o) {
      return NumericConverter.tryConvertingToLong(o);
    }

    @Override
    public long typeToRawLongBits(Long l) {
      return l;
    }

    @Override
    public Storage<Long> createStorage(long[] result, int size, BitSet isNothing) {
      return new LongStorage(result, size, isNothing, IntegerType.INT_64);
    }
  }

  private abstract static class RunningStatisticBase<T> implements RunningStatistic<T> {

    long[] result;
    BitSet isNothing;
    ColumnAggregatedProblemAggregator columnAggregatedProblemAggregator;
    Column sourceColumn;
    TypeHandler<T> typeHandler;

    RunningStatisticBase(
        Column sourceColumn, ProblemAggregator problemAggregator, TypeHandler<T> typeHandler) {
      result = new long[sourceColumn.getSize()];
      isNothing = new BitSet();
      columnAggregatedProblemAggregator = new ColumnAggregatedProblemAggregator(problemAggregator);
      this.sourceColumn = sourceColumn;
      this.typeHandler = typeHandler;
    }

    @Override
    public void calculateNextValue(int i, RunningIterator<T> it) {
      Object value = sourceColumn.getStorage().getItemBoxed(i);
      if (value == null) {
        columnAggregatedProblemAggregator.reportColumnAggregatedProblem(
            new IgnoredNothing(sourceColumn.getName(), i));
      }
      T dValue = typeHandler.tryConvertingToType(value);
      T dNextValue;
      if (dValue != null && dValue.equals(Double.NaN)) {
        columnAggregatedProblemAggregator.reportColumnAggregatedProblem(
            new IgnoredNaN(sourceColumn.getName(), i));
        dNextValue = it.currentValue();
      } else {
        dNextValue = it.next(dValue);
      }
      if (dNextValue == null) {
        isNothing.set(i);
      } else {
        result[i] = typeHandler.typeToRawLongBits(dNextValue);
      }
    }

    @Override
    public Storage<T> getResult() {
      return typeHandler.createStorage(result, sourceColumn.getSize(), isNothing);
    }
  }

  private abstract static class RunningIteratorBase implements RunningIterator<Double> {

    protected double current;
    private boolean isInitialized = false;

    @Override
    public Double next(Double value) {
      if (value != null) {
        if (!isInitialized) {
          isInitialized = true;
          initialize(value);
        } else {
          increment(value);
        }
      }
      return isInitialized ? getCurrent() : null;
    }

    @Override
    public Double currentValue() {
      return isInitialized ? getCurrent() : null;
    }

    protected void initialize(double value) {
      current = value;
    }

    protected abstract void increment(double value);

    protected double getCurrent() {
      return current;
    }
  }

  private static class RunningSumStatistic extends RunningStatisticBase<Double> {

    RunningSumStatistic(Column sourceColumn, ProblemAggregator problemAggregator) {
      super(sourceColumn, problemAggregator, new DoubleHandler());
    }

    @Override
    public RunningIterator<Double> getNewIterator() {
      return new RunningSumIterator();
    }

    private static class RunningSumIterator extends RunningIteratorBase {

      @Override
      public void increment(double value) {
        current += value;
      }
    }
  }

  private static class RunningMeanStatistic extends RunningStatisticBase<Double> {

    RunningMeanStatistic(Column sourceColumn, ProblemAggregator problemAggregator) {
      super(sourceColumn, problemAggregator, new DoubleHandler());
    }

    @Override
    public RunningIterator<Double> getNewIterator() {
      return new RunningMeanIterator();
    }

    private static class RunningMeanIterator extends RunningIteratorBase {

      private int currentCount;

      @Override
      public void increment(double value) {
        current += value;
        currentCount++;
      }

      @Override
      public void initialize(double value) {
        current = value;
        currentCount = 1;
      }

      @Override
      public double getCurrent() {
        return current / currentCount;
      }
    }
  }

  private static class RunningMinStatistic extends RunningStatisticBase<Double> {

    RunningMinStatistic(Column sourceColumn, ProblemAggregator problemAggregator) {
      super(sourceColumn, problemAggregator, new DoubleHandler());
    }

    @Override
    public RunningIterator<Double> getNewIterator() {
      return new RunningMinIterator();
    }

    private static class RunningMinIterator extends RunningIteratorBase {

      @Override
      public void increment(double value) {
        current = Math.min(current, value);
      }
    }
  }

  private static class RunningMaxStatistic extends RunningStatisticBase<Double> {

    RunningMaxStatistic(Column sourceColumn, ProblemAggregator problemAggregator) {
      super(sourceColumn, problemAggregator, new DoubleHandler());
    }

    @Override
    public RunningIterator<Double> getNewIterator() {
      return new RunningMaxIterator();
    }

    private static class RunningMaxIterator extends RunningIteratorBase {

      @Override
      public void increment(double value) {
        current = Math.max(current, value);
      }
    }
  }

  private static class RunningMaxLongStatistic extends RunningStatisticBase<Long> {

    RunningMaxLongStatistic(Column sourceColumn, ProblemAggregator problemAggregator) {
      super(sourceColumn, problemAggregator, new LongHandler());
    }

    @Override
    public RunningIterator<Long> getNewIterator() {
      return new RunningMaxLongIterator();
    }

    private static class RunningMaxLongIterator extends RunningIteratorLong {

      @Override
      public void increment(long value) {
        current = Math.max(current, value);
      }
    }
  }

  private abstract static class RunningIteratorLong implements RunningIterator<Long> {

    protected long current;
    private boolean isInitialized = false;

    @Override
    public Long next(Long value) {
      if (value != null) {
        if (!isInitialized) {
          isInitialized = true;
          initialize(value);
        } else {
          increment(value);
        }
      }
      return isInitialized ? getCurrent() : null;
    }

    @Override
    public Long currentValue() {
      return isInitialized ? getCurrent() : null;
    }

    protected void initialize(long value) {
      current = value;
    }

    protected abstract void increment(long value);

    protected long getCurrent() {
      return current;
    }
  }
}
