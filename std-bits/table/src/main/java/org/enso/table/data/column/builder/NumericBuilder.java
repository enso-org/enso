package org.enso.table.data.column.builder;

import java.lang.foreign.MemorySegment;
import java.util.BitSet;
import org.enso.table.util.ImmutableBitSet;

/** A common base for builders with lazily initialized validity bitmap. */
abstract sealed class NumericBuilder implements Builder permits DoubleBuilder, LongBuilder {
  private BitSet validityMap;
  int currentSize;

  /**
   * Initializes a validityMap builder.
   *
   * @param size the size of buffer to allocate
   * @param validity address of validity bitmap to read or {@code 0} to assume all data are valid
   */
  protected NumericBuilder(int size, long validity) {
    if (validity != 0L) {
      var seg = MemorySegment.ofAddress(validity).reinterpret((size + 7) / 8);
      var valid = seg.asByteBuffer();
      validityMap = BitSet.valueOf(valid);
      currentSize = size;
    }
  }

  /**
   * Lazily initialized getter for the {@link #validityMap} field. Allocates new bitset if none has
   * yet been allocated.
   *
   * @return never returns {@code null} value
   */
  private BitSet getValidityMap() {
    if (validityMap == null) {
      validityMap = new BitSet();
      validityMap.set(0, currentSize);
    }
    return validityMap;
  }

  /**
   * Appends given number of nulls. <b>Does modify</b> {@link #currentSize}!
   *
   * @param count number of nulls to append
   */
  protected final void doAppendNulls(int count) {
    getValidityMap().set(currentSize, currentSize + count, false);
    currentSize += count;
  }

  /**
   * Checks whether value at given index is valid. Value is valid when there is no {@link
   * #validityMap} or when the appropriate bit is set.
   *
   * @param i index to check
   * @return validity status at given index
   */
  protected final boolean isValid(int i) {
    return validityMap == null || validityMap.get(i);
  }

  /**
   * Marks given index as valid. Makes sure that {@link #isValid} call for that index will return
   * {@code true}. No change to {@link #currentSize} is done.
   *
   * @param i the index to mark as valid
   */
  protected final void setValid(int i) {
    if (validityMap != null) {
      validityMap.set(i);
    }
  }

  /**
   * Obtain an immutable snapshot of validity map
   *
   * @return
   */
  protected final ImmutableBitSet validityMap() {
    if (validityMap == null) {
      return ImmutableBitSet.allTrue(currentSize);
    } else {
      return new ImmutableBitSet(validityMap, currentSize);
    }
  }

  /**
   * Appends provided validity map at {@link #currentSize}. Doesn't modify {@link #currentSize}
   * however.
   *
   * @param validity the map to append
   * @param n the number of elements to apply
   */
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
