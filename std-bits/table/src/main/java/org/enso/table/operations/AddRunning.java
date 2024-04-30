package org.enso.table.operations;

import java.util.BitSet;
import org.enso.base.polyglot.NumericConverter;
import org.enso.base.statistics.Statistic;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
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
    if (orderingColumns.length != directions.length) {
      throw new IllegalArgumentException(
          "The number of ordering columns and directions must be the same.");
    }
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

  private static RunningStatistic<Double> createRunningStatistic(
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
        return new RunningMaxStatistic(sourceColumn, problemAggregator);
      }
      default -> throw new IllegalArgumentException("Unsupported statistic: " + statistic);
    }
  }

  private abstract static class RunningStatisticBase implements RunningStatistic<Double> {

    long[] result;
    BitSet isNothing;
    ColumnAggregatedProblemAggregator columnAggregatedProblemAggregator;
    Column sourceColumn;

    RunningStatisticBase(Column sourceColumn, ProblemAggregator problemAggregator) {
      result = new long[sourceColumn.getSize()];
      isNothing = new BitSet();
      columnAggregatedProblemAggregator = new ColumnAggregatedProblemAggregator(problemAggregator);
      this.sourceColumn = sourceColumn;
    }

    @Override
    public void calculateNextValue(int i, RunningIterator<Double> it) {
      Object value = sourceColumn.getStorage().getItemBoxed(i);
      if (value == null) {
        columnAggregatedProblemAggregator.reportColumnAggregatedProblem(
            new IgnoredNothing(sourceColumn.getName(), i));
      }
      Double dValue = NumericConverter.tryConvertingToDouble(value);
      Double dNextValue;
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
        result[i] = Double.doubleToRawLongBits(dNextValue);
      }
    }

    @Override
    public Storage<Double> getResult() {
      return new DoubleStorage(result, sourceColumn.getSize(), isNothing);
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

  private static class RunningSumStatistic extends RunningStatisticBase {

    RunningSumStatistic(Column sourceColumn, ProblemAggregator problemAggregator) {
      super(sourceColumn, problemAggregator);
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

  private static class RunningMeanStatistic extends RunningStatisticBase {

    RunningMeanStatistic(Column sourceColumn, ProblemAggregator problemAggregator) {
      super(sourceColumn, problemAggregator);
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

  private static class RunningMinStatistic extends RunningStatisticBase {

    RunningMinStatistic(Column sourceColumn, ProblemAggregator problemAggregator) {
      super(sourceColumn, problemAggregator);
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

  private static class RunningMaxStatistic extends RunningStatisticBase {

    RunningMaxStatistic(Column sourceColumn, ProblemAggregator problemAggregator) {
      super(sourceColumn, problemAggregator);
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
}
