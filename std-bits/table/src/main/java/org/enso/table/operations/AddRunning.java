package org.enso.table.operations;

import org.enso.base.polyglot.NumericConverter;
import org.enso.base.statistics.Statistic;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForDouble;
import org.enso.table.data.column.builder.BuilderForLong;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.IgnoredNaN;
import org.enso.table.data.table.problems.IgnoredNothing;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;

public class AddRunning {
  public static ColumnStorage<?> create_running(
      Statistic statistic,
      Column sourceColumn,
      Column[] groupingColumns,
      Column[] orderingColumns,
      int[] directions,
      ProblemAggregator problemAggregator) {
    var runningStatistic =
        new RunningStatisticRowVisitorFactory(statistic, sourceColumn, problemAggregator);
    return GroupingOrderingVisitor.visit(
        groupingColumns,
        orderingColumns,
        directions,
        problemAggregator,
        runningStatistic,
        sourceColumn.getSize());
  }

  private static class RunningStatisticRowVisitorFactory implements RowVisitorFactory {
    RunningStatistic<?> runningStatistic;

    RunningStatisticRowVisitorFactory(
        Statistic statistic, Column sourceColumn, ProblemAggregator problemAggregator) {
      runningStatistic = createRunningStatistic(statistic, sourceColumn, problemAggregator);
    }

    @Override
    public GroupRowVisitor getNewRowVisitor() {
      return new RunningStatisticRowVisitor<>(runningStatistic);
    }

    @Override
    public ColumnStorage<?> seal() {
      return runningStatistic.getResult();
    }

    private static class RunningStatisticRowVisitor<T> implements GroupRowVisitor {
      RunningStatistic<T> runningStatistic;
      RunningIterator<T> iterator;

      RunningStatisticRowVisitor(RunningStatistic<T> runningStatistic) {
        this.runningStatistic = runningStatistic;
        iterator = runningStatistic.getNewIterator();
      }

      @Override
      public void visit(long row) {
        runningStatistic.calculateNextValue(Math.toIntExact(row), iterator);
      }
    }
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
      case Product -> {
        return new RunningProductStatistic(sourceColumn, problemAggregator);
      }
      case Minimum -> {
        if (sourceColumn.getStorage().getType() instanceof IntegerType type) {
          return new RunningMinLongStatistic(sourceColumn, problemAggregator, type);
        }
        return new RunningMinStatistic(sourceColumn, problemAggregator);
      }
      case Maximum -> {
        if (sourceColumn.getStorage().getType() instanceof IntegerType type) {
          return new RunningMaxLongStatistic(sourceColumn, problemAggregator, type);
        }
        return new RunningMaxStatistic(sourceColumn, problemAggregator);
      }
      case VariancePopulation -> {
        return new RunningVarianceStatistic(sourceColumn, problemAggregator, true);
      }
      case VarianceSample -> {
        return new RunningVarianceStatistic(sourceColumn, problemAggregator, false);
      }
      case StandardDeviationPopulation -> {
        return new RunningStandardDeviationStatistic(sourceColumn, problemAggregator, true);
      }
      case StandardDeviationSample -> {
        return new RunningStandardDeviationStatistic(sourceColumn, problemAggregator, false);
      }
      case SkewPopulation -> {
        return new RunningSkewStatistic(sourceColumn, problemAggregator, true);
      }
      case SkewSample -> {
        return new RunningSkewStatistic(sourceColumn, problemAggregator, false);
      }
      case Kurtosis -> {
        return new RunningKurtosisStatistic(sourceColumn, problemAggregator);
      }

      default -> throw new IllegalArgumentException("Unsupported statistic: " + statistic);
    }
  }

  private interface RunningStatistic<T> {
    void calculateNextValue(int i, RunningIterator<T> it);

    ColumnStorage<?> getResult();

    RunningIterator<T> getNewIterator();
  }

  private abstract static class RunningStatisticDouble implements RunningStatistic<Double> {
    BuilderForDouble builder;
    ColumnAggregatedProblemAggregator columnAggregatedProblemAggregator;
    Column sourceColumn;

    RunningStatisticDouble(Column sourceColumn, ProblemAggregator problemAggregator) {
      columnAggregatedProblemAggregator = new ColumnAggregatedProblemAggregator(problemAggregator);
      this.sourceColumn = sourceColumn;
      this.builder =
          Builder.getForDouble(
              FloatType.FLOAT_64, sourceColumn.getSize(), columnAggregatedProblemAggregator);
    }

    public void calculateNextValue(int i, RunningIterator<Double> it) {
      Object value = sourceColumn.getStorage().getItemBoxed(i);
      if (value == null) {
        columnAggregatedProblemAggregator.reportColumnAggregatedProblem(
            new IgnoredNothing(sourceColumn.getName(), i));
      }

      Double dValue = NumericConverter.tryConvertingToDouble(value);
      if (dValue != null && dValue.equals(Double.NaN)) {
        columnAggregatedProblemAggregator.reportColumnAggregatedProblem(
            new IgnoredNaN(sourceColumn.getName(), (long) i));
        builder.append(it.currentValue());
      } else {
        builder.append(it.next(dValue));
      }
    }

    public ColumnStorage<?> getResult() {
      return builder.seal();
    }
  }

  private abstract static class RunningStatisticLong implements RunningStatistic<Long> {
    BuilderForLong builder;
    ColumnAggregatedProblemAggregator columnAggregatedProblemAggregator;
    Column sourceColumn;

    RunningStatisticLong(
        Column sourceColumn, IntegerType type, ProblemAggregator problemAggregator) {
      columnAggregatedProblemAggregator = new ColumnAggregatedProblemAggregator(problemAggregator);
      this.sourceColumn = sourceColumn;
      this.builder =
          Builder.getForLong(type, sourceColumn.getSize(), columnAggregatedProblemAggregator);
    }

    public void calculateNextValue(int i, RunningIterator<Long> it) {
      Object value = sourceColumn.getStorage().getItemBoxed(i);
      if (value == null) {
        columnAggregatedProblemAggregator.reportColumnAggregatedProblem(
            new IgnoredNothing(sourceColumn.getName(), i));
      }

      Long lValue = NumericConverter.tryConvertingToLong(value);
      if (lValue == null) {
        columnAggregatedProblemAggregator.reportColumnAggregatedProblem(
            new IgnoredNaN(sourceColumn.getName(), i));
        builder.append(it.currentValue());
      } else {
        builder.append(it.next(lValue));
      }
    }

    public ColumnStorage<?> getResult() {
      return builder.seal();
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

  private static class RunningSumStatistic extends RunningStatisticDouble {
    RunningSumStatistic(Column sourceColumn, ProblemAggregator problemAggregator) {
      super(sourceColumn, problemAggregator);
    }

    @Override
    public RunningIterator<Double> getNewIterator() {
      return new RunningSumIterator();
    }
  }

  private static class RunningMeanStatistic extends RunningStatisticDouble {
    RunningMeanStatistic(Column sourceColumn, ProblemAggregator problemAggregator) {
      super(sourceColumn, problemAggregator);
    }

    @Override
    public RunningIterator<Double> getNewIterator() {
      return new RunningMeanIterator();
    }
  }

  private static class RunningProductStatistic extends RunningStatisticDouble {
    RunningProductStatistic(Column sourceColumn, ProblemAggregator problemAggregator) {
      super(sourceColumn, problemAggregator);
    }

    @Override
    public RunningIterator<Double> getNewIterator() {
      return new RunningProductIterator();
    }
  }

  private static class RunningVarianceStatistic extends RunningStatisticDouble {
    private final boolean isPopulationVariance;

    RunningVarianceStatistic(
        Column sourceColumn, ProblemAggregator problemAggregator, boolean isPopulationVariance) {
      super(sourceColumn, problemAggregator);
      this.isPopulationVariance = isPopulationVariance;
    }

    @Override
    public RunningIterator<Double> getNewIterator() {
      return new RunningVarianceIterator(isPopulationVariance);
    }
  }

  private static class RunningStandardDeviationStatistic extends RunningStatisticDouble {
    private final boolean isPopulation;

    RunningStandardDeviationStatistic(
        Column sourceColumn, ProblemAggregator problemAggregator, boolean isPopulation) {
      super(sourceColumn, problemAggregator);
      this.isPopulation = isPopulation;
    }

    @Override
    public RunningIterator<Double> getNewIterator() {
      return new RunningStandardDeviationIterator(isPopulation);
    }
  }

  private static class RunningSkewStatistic extends RunningStatisticDouble {
    private final boolean isPopulation;

    RunningSkewStatistic(
        Column sourceColumn, ProblemAggregator problemAggregator, boolean isPopulation) {
      super(sourceColumn, problemAggregator);
      this.isPopulation = isPopulation;
    }

    @Override
    public RunningIterator<Double> getNewIterator() {
      return new RunningSkewIterator(isPopulation);
    }
  }

  private static class RunningKurtosisStatistic extends RunningStatisticDouble {
    RunningKurtosisStatistic(Column sourceColumn, ProblemAggregator problemAggregator) {
      super(sourceColumn, problemAggregator);
    }

    @Override
    public RunningIterator<Double> getNewIterator() {
      return new RunningKurtosisIterator();
    }
  }

  private static class RunningSumIterator extends RunningIteratorBase {
    protected double sum;

    @Override
    public void initialize(double value) {
      super.initialize(value);
      sum = value;
    }

    @Override
    public void increment(double value) {
      sum += value;
    }

    @Override
    public double getCurrent() {
      return sum;
    }
  }

  private static class RunningProductIterator extends RunningIteratorBase {
    @Override
    public void increment(double value) {
      current *= value;
    }
  }

  private static class RunningMeanIterator extends RunningSumIterator {
    protected int currentCount;

    @Override
    public void increment(double value) {
      super.increment(value);
      currentCount++;
    }

    @Override
    public void initialize(double value) {
      super.initialize(value);
      currentCount = 1;
    }

    @Override
    public double getCurrent() {
      return sum / currentCount;
    }
  }

  private static class RunningVarianceIterator extends RunningMeanIterator {
    protected double sumSquares;
    protected boolean isPopulation;

    RunningVarianceIterator(boolean isPopulation) {
      this.isPopulation = isPopulation;
    }

    @Override
    public void increment(double value) {
      super.increment(value);
      sumSquares += value * value;
    }

    @Override
    public void initialize(double value) {
      super.initialize(value);
      sumSquares = value * value;
    }

    @Override
    public double getCurrent() {
      double mean = super.getCurrent();
      double denominator = isPopulation ? currentCount : currentCount - 1;
      return (sumSquares - 2 * mean * sum + currentCount * mean * mean) / denominator;
    }
  }

  private static class RunningStandardDeviationIterator extends RunningVarianceIterator {
    RunningStandardDeviationIterator(boolean isPopulation) {
      super(isPopulation);
    }

    @Override
    public double getCurrent() {
      return Math.sqrt(super.getCurrent());
    }
  }

  private static class RunningSkewIterator extends RunningStandardDeviationIterator {
    protected double sumCubes;

    RunningSkewIterator(boolean isPopulation) {
      super(isPopulation);
    }

    @Override
    public void increment(double value) {
      super.increment(value);
      sumCubes += value * value * value;
    }

    @Override
    public void initialize(double value) {
      super.initialize(value);
      sumCubes = value * value * value;
    }

    @Override
    public double getCurrent() {
      if (currentCount <= 2) {
        return Double.NaN;
      }
      double mean = sum / currentCount;
      double standardDeviation = super.getCurrent();
      double denominator =
          isPopulation
              ? currentCount
              : ((double) ((currentCount - 1) * (currentCount - 2)) / (double) currentCount);
      double scale =
          1.0 / (standardDeviation * standardDeviation * standardDeviation) / denominator;
      return (sumCubes - 3 * mean * sumSquares + 2 * mean * mean * sum) * scale;
    }
  }

  private static class RunningKurtosisIterator extends RunningVarianceIterator {
    private double sumCubes;
    private double sumQuads;

    RunningKurtosisIterator() {
      super(false);
    }

    @Override
    public void increment(double value) {
      super.increment(value);
      sumCubes += value * value * value;
      sumQuads += value * value * value * value;
    }

    @Override
    public void initialize(double value) {
      super.initialize(value);
      sumCubes = value * value * value;
      sumQuads = value * value * value * value;
      currentCount = 1;
    }

    @Override
    public double getCurrent() {
      if (currentCount <= 3) {
        return Double.NaN;
      }
      double mean = sum / currentCount;
      double variance = super.getCurrent();
      double scale =
          (double) (currentCount * (currentCount + 1))
              / (double)
                  ((currentCount - 1)
                      * (currentCount - 2)
                      * (currentCount - 3)
                      * variance
                      * variance);
      double shift =
          (double) (3 * (currentCount - 1) * (currentCount - 1))
              / (double) ((currentCount - 2) * (currentCount - 3));
      double kurtosis =
          (sumQuads
                      - 4 * mean * sumCubes
                      + 6 * mean * mean * sumSquares
                      - 3 * mean * mean * mean * sum)
                  * scale
              - shift;
      return kurtosis;
    }
  }

  private static class RunningMinStatistic extends RunningStatisticDouble {
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

  private static class RunningMaxStatistic extends RunningStatisticDouble {
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

  private static class RunningMinLongStatistic extends RunningStatisticLong {
    RunningMinLongStatistic(
        Column sourceColumn, ProblemAggregator problemAggregator, IntegerType type) {
      super(sourceColumn, type, problemAggregator);
    }

    @Override
    public RunningIterator<Long> getNewIterator() {
      return new RunningMinLongIterator();
    }

    private static class RunningMinLongIterator extends RunningIteratorLong {

      @Override
      public void increment(long value) {
        current = Math.min(current, value);
      }
    }
  }

  private static class RunningMaxLongStatistic extends RunningStatisticLong {
    RunningMaxLongStatistic(
        Column sourceColumn, ProblemAggregator problemAggregator, IntegerType type) {
      super(sourceColumn, type, problemAggregator);
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
