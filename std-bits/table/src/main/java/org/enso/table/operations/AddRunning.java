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

    IntegerType type;

    LongHandler(IntegerType type) {
      this.type = type;
    }

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
      return new LongStorage(result, size, isNothing, type);
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
  }

  private static class RunningMeanStatistic extends RunningStatisticBase<Double> {

    RunningMeanStatistic(Column sourceColumn, ProblemAggregator problemAggregator) {
      super(sourceColumn, problemAggregator, new DoubleHandler());
    }

    @Override
    public RunningIterator<Double> getNewIterator() {
      return new RunningMeanIterator();
    }
  }

  private static class RunningVarianceStatistic extends RunningStatisticBase<Double> {

    private final boolean isPopulationVariance;

    RunningVarianceStatistic(
        Column sourceColumn, ProblemAggregator problemAggregator, boolean isPopulationVariance) {
      super(sourceColumn, problemAggregator, new DoubleHandler());
      this.isPopulationVariance = isPopulationVariance;
    }

    @Override
    public RunningIterator<Double> getNewIterator() {
      return new RunningVarianceIterator(isPopulationVariance);
    }
  }

  private static class RunningStandardDeviationStatistic extends RunningStatisticBase<Double> {

    private final boolean isPopulation;

    RunningStandardDeviationStatistic(
        Column sourceColumn, ProblemAggregator problemAggregator, boolean isPopulation) {
      super(sourceColumn, problemAggregator, new DoubleHandler());
      this.isPopulation = isPopulation;
    }

    @Override
    public RunningIterator<Double> getNewIterator() {
      return new RunningStandardDeviationIterator(isPopulation);
    }
  }

  private static class RunningSkewStatistic extends RunningStatisticBase<Double> {

    private final boolean isPopulation;

    RunningSkewStatistic(
        Column sourceColumn, ProblemAggregator problemAggregator, boolean isPopulation) {
      super(sourceColumn, problemAggregator, new DoubleHandler());
      this.isPopulation = isPopulation;
    }

    @Override
    public RunningIterator<Double> getNewIterator() {
      return new RunningSkewIterator(isPopulation);
    }
  }

  private static class RunningKurtosisStatistic extends RunningStatisticBase<Double> {

    RunningKurtosisStatistic(Column sourceColumn, ProblemAggregator problemAggregator) {
      super(sourceColumn, problemAggregator, new DoubleHandler());
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
      double skew = (sumCubes - 3 * mean * sumSquares + 2 * mean * mean * sum) * scale;
      return skew;
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

  private static class RunningMinLongStatistic extends RunningStatisticBase<Long> {

    RunningMinLongStatistic(
        Column sourceColumn, ProblemAggregator problemAggregator, IntegerType type) {
      super(sourceColumn, problemAggregator, new LongHandler(type));
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

    RunningMaxLongStatistic(
        Column sourceColumn, ProblemAggregator problemAggregator, IntegerType type) {
      super(sourceColumn, problemAggregator, new LongHandler(type));
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
