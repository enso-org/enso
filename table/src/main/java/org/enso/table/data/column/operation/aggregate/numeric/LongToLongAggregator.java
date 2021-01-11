package org.enso.table.data.column.operation.aggregate.numeric;

import org.enso.table.data.column.operation.aggregate.Aggregator;
import org.enso.table.data.column.storage.LongStorage;
import org.enso.table.data.column.storage.Storage;

import java.util.BitSet;
import java.util.List;
import java.util.stream.LongStream;

/** An aggregator consuming a {@link LongStorage} and returning a {@link LongStorage} */
public abstract class LongToLongAggregator extends Aggregator {
  private final LongStorage storage;
  private final long[] items;
  private final BitSet missing;
  private int position = 0;

  /**
   * @param storage the data source
   * @param resultSize the number of times {@link #nextGroup(List)} will be called
   */
  public LongToLongAggregator(LongStorage storage, int resultSize) {
    this.storage = storage;
    this.items = new long[resultSize];
    this.missing = new BitSet();
  }

  /** Used by subclasses to return a missing value from a given group. */
  protected void submitMissing() {
    missing.set(position++);
  }

  /**
   * Used by subclasses to return a value from a given group.
   *
   * @param value the return value of a group
   */
  protected void submit(long value) {
    items[position++] = value;
  }

  /**
   * Runs the aggregation on a particular set of values.
   *
   * @param items the values contained in the current group
   */
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
