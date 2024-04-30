package org.enso.table.operations;

import java.util.BitSet;
import org.enso.base.polyglot.NumericConverter;
import org.enso.base.statistics.Statistic;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.IgnoredNothing;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;

/**
 * @author riley
 */
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
            groupingColumns, orderingColumns, directions, problemAggregator);
    var factory = new RunningIteratorFactoryImpl(statistic, sourceColumn, problemAggregator);
    runningGenerator.generate(factory, sourceColumn.getSize());
    var ret = new DoubleStorage(factory.result, sourceColumn.getSize(), factory.isNothing);

    return ret;
  }

  private static class RunningIteratorFactoryImpl implements RunningIteratorFactory {

    Statistic statistic;
    long[] result;
    BitSet isNothing;
    ColumnAggregatedProblemAggregator columnAggregatedProblemAggregator;
    Column sourceColumn;

    RunningIteratorFactoryImpl(
        Statistic statistic, Column sourceColumn, ProblemAggregator problemAggregator) {
      this.statistic = statistic;
      result = new long[sourceColumn.getSize()];
      isNothing = new BitSet();
      columnAggregatedProblemAggregator = new ColumnAggregatedProblemAggregator(problemAggregator);
      this.sourceColumn = sourceColumn;
    }

    @Override
    public void calculateNextValue(int i, RunningIterator it) {
      Object value = sourceColumn.getStorage().getItemBoxed(i);
      if (value == null) {
        columnAggregatedProblemAggregator.reportColumnAggregatedProblem(
            new IgnoredNothing(sourceColumn.getName(), i));
      }
      Double dValue = NumericConverter.tryConvertingToDouble(value);
      Double dNextValue = it.next(dValue);
      if (dNextValue == null) {
        isNothing.set(i);
      } else {
        result[i] = Double.doubleToRawLongBits(dNextValue);
      }
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
      return !isInitialized ? null : getCurrent();
    }

    protected void initialize(double value) {
      current = value;
    }

    protected abstract void increment(double value);

    protected double getCurrent() {
      return current;
    }
  }

  private static class RunningSumIterator extends RunningIteratorBase {

    @Override
    public void increment(double value) {
      current += value;
    }
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

  private static class RunningMinIterator extends RunningIteratorBase {

    @Override
    public void increment(double value) {
      current = Math.min(current, value);
    }
  }

  private static class RunningMaxIterator extends RunningIteratorBase {

    @Override
    public void increment(double value) {
      current = Math.max(current, value);
    }
  }
}
