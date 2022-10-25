package org.enso.table.data.column.operation.aggregate.numeric;

import org.enso.table.data.column.operation.aggregate.Aggregator;
import org.enso.table.data.column.storage.DoubleStorage;
import org.enso.table.data.column.storage.NumericStorage;
import org.enso.table.data.column.storage.Storage;

import java.util.BitSet;
import java.util.OptionalDouble;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;

/**
 * An aggregator sourcing data from any {@link NumericStorage} and returning a {@link
 * DoubleStorage}.
 */
public abstract class NumericAggregator extends Aggregator {
  private final NumericStorage<?> storage;
  private final long[] data;
  private final BitSet missing;
  private int position = 0;

  /**
   * @param storage the data source
   * @param resultSize the number of times {@link Aggregator#nextGroup(IntStream)} will be called
   */
  public NumericAggregator(NumericStorage<?> storage, int resultSize) {
    this.storage = storage;
    this.data = new long[resultSize];
    this.missing = new BitSet();
  }

  /**
   * Runs the aggregation on a particular set of values.
   *
   * @param elements the values contained in the current group
   */
  protected abstract void runGroup(DoubleStream elements);

  /**
   * Used by subclasses to return a value from a given group.
   *
   * @param value the return value of a group
   */
  protected void submit(double value) {
    data[position++] = Double.doubleToRawLongBits(value);
  }

  /**
   * Used by subclasses to return a value from a given group.
   *
   * @param value the return value of a group
   */
  protected void submit(OptionalDouble value) {
    if (value.isPresent()) {
      submit(value.getAsDouble());
    } else {
      submitMissing();
    }
  }

  /** Used by subclasses to return a missing value from a given group. */
  protected void submitMissing() {
    missing.set(position++);
  }

  @Override
  public void nextGroup(IntStream positions) {
    DoubleStream elements =
        positions.filter(i -> !storage.isNa(i)).mapToDouble(storage::getItemDouble);
    runGroup(elements);
  }

  @Override
  public Storage<Double> seal() {
    return new DoubleStorage(data, data.length, missing);
  }
}
