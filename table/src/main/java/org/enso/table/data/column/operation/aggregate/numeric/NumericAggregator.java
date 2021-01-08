package org.enso.table.data.column.operation.aggregate.numeric;

import org.enso.table.data.column.operation.aggregate.Aggregator;
import org.enso.table.data.column.storage.DoubleStorage;
import org.enso.table.data.column.storage.NumericStorage;
import org.enso.table.data.column.storage.Storage;

import java.util.BitSet;
import java.util.List;
import java.util.OptionalDouble;
import java.util.stream.DoubleStream;

public abstract class NumericAggregator extends Aggregator {
  private final NumericStorage storage;
  private final long[] data;
  private final BitSet missing;
  private int position = 0;

  public NumericAggregator(NumericStorage storage, int resultSize) {
    this.storage = storage;
    this.data = new long[resultSize];
    this.missing = new BitSet();
  }

  protected abstract void runGroup(DoubleStream elements);

  protected void submit(double value) {
    data[position++] = Double.doubleToRawLongBits(value);
  }

  protected void submit(OptionalDouble value) {
    if (value.isPresent()) {
      submit(value.getAsDouble());
    } else {
      submitMissing();
    }
  }

  protected void submitMissing() {
    missing.set(position++);
  }

  @Override
  public void nextGroup(List<Integer> positions) {
    DoubleStream elements =
        positions.stream().filter(i -> !storage.isNa(i)).mapToDouble(storage::getItemDouble);
    runGroup(elements);
  }

  @Override
  public Storage seal() {
    return new DoubleStorage(data, data.length, missing);
  }
}
