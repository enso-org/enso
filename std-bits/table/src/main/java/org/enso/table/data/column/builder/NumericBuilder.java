package org.enso.table.data.column.builder;

import java.util.BitSet;

/** A common base for numeric builders. */
public abstract class NumericBuilder implements Builder {
  protected BitSet isNothing;
  protected int currentSize;

  protected NumericBuilder() {
    this.isNothing = new BitSet();
    this.currentSize = 0;
  }

  @Override
  public void appendNulls(int count) {
    isNothing.set(currentSize, currentSize + count);
    currentSize += count;
  }

  @Override
  public int getCurrentSize() {
    return currentSize;
  }

  protected void ensureFreeSpaceFor(int additionalSize) {
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
  protected void ensureSpaceToAppend() {
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
