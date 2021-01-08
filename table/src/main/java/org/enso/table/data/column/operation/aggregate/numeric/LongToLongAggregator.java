package org.enso.table.data.column.operation.aggregate.numeric;

import org.enso.table.data.column.operation.aggregate.Aggregator;
import org.enso.table.data.column.storage.LongStorage;
import org.enso.table.data.column.storage.Storage;

import java.util.BitSet;
import java.util.List;
import java.util.stream.LongStream;

public abstract class LongToLongAggregator extends Aggregator {
  private final LongStorage storage;
  private final long[] items;
  private final BitSet missing;
  private int position = 0;

  public LongToLongAggregator(LongStorage storage, int resultSize) {
    this.storage = storage;
    this.items = new long[resultSize];
    this.missing = new BitSet();
  }

  protected void submitMissing() {
    missing.set(position++);
  }

  protected void submit(long value) {
    items[position++] = value;
  }

  protected abstract void runGroup(LongStream items);

  @Override
  public void nextGroup(List<Integer> positions) {
    LongStream items = positions.stream().filter(x -> !storage.isNa(x)).mapToLong(storage::getItem);
    runGroup(items);
  }

  @Override
  public Storage seal() {
    return new LongStorage(items, items.length, missing);
  }
}
