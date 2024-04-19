package org.enso.table.operations;

import org.enso.base.statistics.Statistic;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ProblemAggregator;

public class AddRunning {

  public static Storage<Double> create_running(
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
    var runningGenerator =
        RunningGenerator.createGenerator(
            sourceColumn, groupingColumns, orderingColumns, directions, problemAggregator);
    runningGenerator.generate(new RunningIteratorFactoryImpl(statistic));
    var ret =
        new DoubleStorage(
            runningGenerator.result, sourceColumn.getSize(), runningGenerator.isNothing);
    return ret;
  }

  private static class RunningIteratorFactoryImpl implements RunningIteratorFactory {

    Statistic statistic;

    RunningIteratorFactoryImpl(Statistic statistic) {
      this.statistic = statistic;
    }

    @Override
    public RunningIterator getIterator() {
      switch (statistic) {
        case Sum -> {
          return new RunningSumIterator();
        }
        case Mean -> {
          return new RunningMeanIterator();
        }
        case Minimum -> {
          return new RunningMinIterator();
        }
        case Maximum -> {
          return new RunningMaxIterator();
        }
        default -> throw new IllegalArgumentException("Unsupported statistic: " + statistic);
      }
    }
  }

  private abstract static class RunningIteratorBase implements RunningIterator {

    protected Double current;
    private boolean isFirst = true;

    @Override
    public Double next(Double value) {
      if (isFirst) {
        isFirst = false;
        initialize(value);
      } else if (value == null || current == null) {
        current = null;
      } else {
        increment(value);
      }
      return getCurrent();
    }

    public void initialize(Double value) {
      current = value;
    }

    public abstract void increment(Double value);

    public Double getCurrent() {
      return current;
    }
  }

  private static class RunningSumIterator extends RunningIteratorBase {

    @Override
    public void increment(Double value) {
      current += value;
    }
  }

  private static class RunningMeanIterator extends RunningIteratorBase {

    private int currentCount;

    @Override
    public void increment(Double value) {
      current += value;
      currentCount++;
    }

    @Override
    public void initialize(Double value) {
      super.initialize(value);
      currentCount = 1;
    }

    @Override
    public Double getCurrent() {
      return current == null ? null : current / currentCount;
    }
  }

  private static class RunningMinIterator extends RunningIteratorBase {

    @Override
    public void increment(Double value) {
      current = Math.min(current, value);
    }
  }

  private static class RunningMaxIterator extends RunningIteratorBase {

    @Override
    public void increment(Double value) {
      current = Math.max(current, value);
    }
  }
}
