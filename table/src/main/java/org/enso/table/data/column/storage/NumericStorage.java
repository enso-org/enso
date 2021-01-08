package org.enso.table.data.column.storage;

import org.enso.table.data.column.operation.aggregate.Aggregator;
import org.enso.table.data.column.operation.aggregate.numeric.NumericAggregator;

import java.util.stream.DoubleStream;

public abstract class NumericStorage extends Storage {
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
