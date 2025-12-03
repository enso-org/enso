package org.enso.table.data.column.builder;

import java.util.BitSet;
import org.enso.table.util.ImmutableBitSet;

/** A common base for numeric builders. */
abstract sealed class NumericBuilder implements Builder permits DoubleBuilder, LongBuilder {
  private BitSet validityMap;
  int currentSize;

  protected NumericBuilder() {}

  private BitSet getValidityMap() {
    if (validityMap == null) {
      validityMap = new BitSet();
      validityMap.set(0, currentSize);
    }
    return validityMap;
  }

  protected final void doAppendNulls(int count) {
    getValidityMap().set(currentSize, currentSize + count, false);
    currentSize += count;
  }

  protected final boolean isValid(int i) {
    return validityMap == null || validityMap.get(i);
  }

  protected final void setValid(int i) {
    if (validityMap != null) {
      validityMap.set(i);
    }
  }

  protected final ImmutableBitSet validityMap() {
    if (validityMap == null) {
      return ImmutableBitSet.allTrue(currentSize);
    } else {
      return new ImmutableBitSet(validityMap, currentSize);
    }
  }

  protected final void appendValidityMap(ImmutableBitSet validity, int n) {
    if (validity.cardinality() < n || validityMap != null) {
      validity.copyTo(getValidityMap(), currentSize, n);
    }
  }

  @Override
  public long getCurrentSize() {
    return currentSize;
  }

  protected final void ensureFreeSpaceFor(int additionalSize) {
    if (currentSize + additionalSize > getDataSize()) {
      resize(currentSize + additionalSize);
    }
  }

  /**
   * Grows the underlying array.
   *
   * <p>The method grows the array by 50% by default to amortize the re-allocation time over
   * appends. It tries to keep the invariant that after calling `grow` the array has at least one
   * free slot.
   */
  protected final void ensureSpaceToAppend() {
    int dataLength = getDataSize();

    // Check current size. If there is space, we don't need to grow.
    if (currentSize < dataLength) {
      return;
    }

    int desiredCapacity = Math.max(currentSize + 1, dataLength > 1 ? dataLength * 3 / 2 : 3);
    resize(desiredCapacity);
  }

  protected abstract int getDataSize();

  protected abstract void resize(int desiredCapacity);
}
