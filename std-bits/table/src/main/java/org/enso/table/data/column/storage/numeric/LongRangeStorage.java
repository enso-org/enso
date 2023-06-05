package org.enso.table.data.column.storage.numeric;

/**
 * Implements a storage that can store a simple range of integers (e.g. row indices) with a
 * specified start, step and length.
 */
public class LongRangeStorage extends ComputedLongStorage {
  private final long start;
  private final long step;

  public LongRangeStorage(long start, long step, int size) {
    super(size);
    this.start = start;
    this.step = step;
  }

  @Override
  protected long computeItem(int idx) {
    return start + idx * step;
  }
}
