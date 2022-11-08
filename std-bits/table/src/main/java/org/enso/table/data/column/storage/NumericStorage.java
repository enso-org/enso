package org.enso.table.data.column.storage;

import java.util.stream.DoubleStream;
import org.enso.table.data.column.operation.aggregate.Aggregator;
import org.enso.table.data.column.operation.aggregate.numeric.NumericAggregator;

/** A storage containing items representable as a {@code double}. */
public abstract class NumericStorage<T> extends Storage<T> {
  /**
   * Returns the value stored at the given index. The return value if the given index is missing
   * ({@link #isNa(long)}) is undefined.
   *
   * @param idx the index to look up
   * @return the value associated with {@code idx}
   */
  public abstract double getItemDouble(int idx);

  @Override
  protected Aggregator getVectorizedAggregator(String name, int resultSize) {
    switch (name) {
      case Aggregators.MAX:
        return new NumericAggregator(this, resultSize) {
          @Override
          protected void runGroup(DoubleStream elements) {
            submit(elements.max());
          }
        };
      case Aggregators.MIN:
        return new NumericAggregator(this, resultSize) {
          @Override
          protected void runGroup(DoubleStream elements) {
            submit(elements.min());
          }
        };
      case Aggregators.SUM:
        return new NumericAggregator(this, resultSize) {
          @Override
          protected void runGroup(DoubleStream elements) {
            double[] its = elements.toArray();
            if (its.length == 0) {
              submitMissing();
            } else {
              submit(DoubleStream.of(its).sum());
            }
          }
        };
      case Aggregators.MEAN:
        return new NumericAggregator(this, resultSize) {
          @Override
          protected void runGroup(DoubleStream elements) {
            submit(elements.average());
          }
        };
      default:
        return super.getVectorizedAggregator(name, resultSize);
    }
  }
}
