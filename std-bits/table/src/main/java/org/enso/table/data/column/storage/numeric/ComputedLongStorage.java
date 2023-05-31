package org.enso.table.data.column.storage.numeric;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.index.Index;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;

import java.util.BitSet;
import java.util.List;

/**
 * Implements a storage that computes the ith stored value using some function.
 *
 * <p>This storage assumes that _all_ values are present.
 */
public abstract class ComputedLongStorage extends AbstractLongStorage {
  protected final int size;

  protected abstract long computeItem(int idx);

  protected ComputedLongStorage(int size) {
    this.size = size;
  }

  @Override
  public int size() {
    return size;
  }

  @Override
  public int countMissing() {
    return 0;
  }

  @Override
  public StorageType getType() {
    return IntegerType.INT_64;
  }

  @Override
  public boolean isNa(long idx) {
    return false;
  }

  @Override
  public Long getItemBoxed(int idx) {
    return getItem(idx);
  }

  public long getItem(int idx) {
    if (idx < 0 || idx >= size) {
      throw new IndexOutOfBoundsException(
          "Index " + idx + " is out of bounds for range of length " + size + ".");
    }

    return computeItem(idx);
  }

  @Override
  public BitSet getIsMissing() {
    return EMPTY;
  }

  @Override
  public Storage<Long> mask(BitSet mask, int cardinality) {
    BitSet newMissing = new BitSet();
    long[] newData = new long[cardinality];
    int resIx = 0;
    for (int i = 0; i < size; i++) {
      if (mask.get(i)) {
        newData[resIx++] = getItem(i);
      }
    }
    return new LongStorage(newData, cardinality, newMissing);
  }

  @Override
  public Storage<Long> applyMask(OrderMask mask) {
    int[] positions = mask.getPositions();
    long[] newData = new long[positions.length];
    BitSet newMissing = new BitSet();
    for (int i = 0; i < positions.length; i++) {
      if (positions[i] == Index.NOT_FOUND) {
        newMissing.set(i);
      } else {
        newData[i] = getItem(positions[i]);
      }
    }
    return new LongStorage(newData, positions.length, newMissing);
  }

  @Override
  public Storage<Long> countMask(int[] counts, int total) {
    long[] newData = new long[total];
    BitSet newMissing = new BitSet();
    int pos = 0;
    for (int i = 0; i < counts.length; i++) {
      long item = getItem(i);
      for (int j = 0; j < counts[i]; j++) {
        newData[pos++] = item;
      }
    }
    return new LongStorage(newData, total, newMissing);
  }

  @Override
  public Storage<Long> slice(int offset, int limit) {
    int newSize = Math.min(size - offset, limit);
    long[] newData = new long[newSize];
    for (int i = 0; i < newSize; i++) {
      newData[i] = getItem(offset + i);
    }
    BitSet newMask = new BitSet();
    return new LongStorage(newData, newSize, newMask);
  }

  @Override
  public Storage<Long> slice(List<SliceRange> ranges) {
    int newSize = SliceRange.totalLength(ranges);
    long[] newData = new long[newSize];
    BitSet newMissing = new BitSet(newSize);
    int offset = 0;
    for (SliceRange range : ranges) {
      int rangeStart = range.start();
      int length = range.end() - rangeStart;
      for (int i = 0; i < length; i++) {
        newData[offset + i] = getItem(rangeStart + i);
      }
      offset += length;
    }

    return new LongStorage(newData, newSize, newMissing);
  }

  private static final BitSet EMPTY = new BitSet();
}
