package org.enso.table.data.column.operation.aggregate;

import org.enso.table.data.column.storage.LongStorage;
import org.enso.table.data.column.storage.Storage;

import java.util.stream.IntStream;

/** Aggregates a storage by counting the non-missing values in each group. */
public class CountAggregator extends Aggregator {
  private final Storage storage;
  private final long[] counts;
  private int position = 0;

  /**
   * @param storage the storage used as data source
   * @param resultSize the exact number of times {@link Aggregator#nextGroup(IntStream)} will be
   *     called.
   */
  public CountAggregator(Storage storage, int resultSize) {
    this.storage = storage;
    this.counts = new long[resultSize];
  }

  @Override
  public void nextGroup(IntStream positions) {
    counts[position++] = positions.filter(i -> !storage.isNa(i)).count();
  }

  @Override
  public Storage seal() {
    return new LongStorage(counts);
  }
}
