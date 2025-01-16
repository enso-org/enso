package org.enso.table.data.column.builder;

import java.util.Arrays;
import java.util.BitSet;

/** A common base for numeric builders. */
public abstract class NumericBuilder implements Builder {
  protected BitSet isNothing;
  protected long[] data;
  protected int currentSize;

  protected NumericBuilder(BitSet isNothing, long[] data, int currentSize) {
    this.isNothing = isNothing;
    this.data = data;
    this.currentSize = currentSize;
  }

  @Override
  public void appendNulls(int count) {
    isNothing.set(currentSize, currentSize + count);
    currentSize += count;
  }

  @Override
  public void append(Object o) {
    if (currentSize >= data.length) {
      grow();
    }
    appendNoGrow(o);
  }

  @Override
  public int getCurrentSize() {
    return currentSize;
  }

  protected void ensureFreeSpaceFor(int additionalSize) {
    if (currentSize + additionalSize > data.length) {
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
  protected void grow() {
    int desiredCapacity = 3;
    if (data.length > 1) {
      desiredCapacity = (data.length * 3 / 2);
    }

    // It is possible for the `currentSize` to grow arbitrarily larger than
    // the capacity, because when nulls are being added the array is not
    // resized, only the counter is incremented. Thus, we need to ensure
    // that we have allocated enough space for at least one element.
    if (currentSize >= desiredCapacity) {
      desiredCapacity = currentSize + 1;
    }

    resize(desiredCapacity);
  }

  protected void resize(int desiredCapacity) {
    this.data = Arrays.copyOf(data, desiredCapacity);
  }
}
