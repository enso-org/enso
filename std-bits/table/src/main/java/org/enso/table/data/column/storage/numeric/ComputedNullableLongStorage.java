package org.enso.table.data.column.storage.numeric;

import java.util.BitSet;
import java.util.List;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.index.Index;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;
import org.graalvm.polyglot.Context;

/**
 * Implements a storage that computes the ith stored value using some function.
 *
 * <p>This storage allows for missing values. Prefer {@link ComputedLongStorage} for non-nullable
 * case.
 */
public abstract class ComputedNullableLongStorage extends AbstractLongStorage {
  protected final int size;

  protected abstract Long computeItem(int idx);

  protected ComputedNullableLongStorage(int size) {
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
  public IntegerType getType() {
    return IntegerType.INT_64;
  }

  @Override
  public boolean isNa(long idx) {
    if (idx < 0 || idx >= size) {
      throw new IndexOutOfBoundsException(
          "Index " + idx + " is out of bounds for range of length " + size + ".");
    }

    return computeItem((int) idx) == null;
  }

  @Override
  public Long getItemBoxed(int idx) {
    if (idx < 0 || idx >= size) {
      throw new IndexOutOfBoundsException(
          "Index " + idx + " is out of bounds for range of length " + size + ".");
    }

    return computeItem(idx);
  }

  public long getItem(int idx) {
    return getItemBoxed(idx);
  }

  @Override
  public BitSet getIsMissing() {
    BitSet missing = new BitSet();
    Context context = Context.getCurrent();
    for (int i = 0; i < size; i++) {
      if (computeItem(i) == null) {
        missing.set(i);
      }

      context.safepoint();
    }
    return missing;
  }

  @Override
  public Storage<Long> mask(BitSet mask, int cardinality) {
    BitSet newMissing = new BitSet();
    long[] newData = new long[cardinality];
    int resIx = 0;
    Context context = Context.getCurrent();
    for (int i = 0; i < size; i++) {
      if (mask.get(i)) {
        Long item = computeItem(i);
        if (item == null) {
          newMissing.set(resIx++);
        } else {
          newData[resIx++] = item;
        }
      }

      context.safepoint();
    }
    return new LongStorage(newData, cardinality, newMissing, getType());
  }

  @Override
  public Storage<Long> applyMask(OrderMask mask) {
    int[] positions = mask.getPositions();
    long[] newData = new long[positions.length];
    BitSet newMissing = new BitSet();
    Context context = Context.getCurrent();
    for (int i = 0; i < positions.length; i++) {
      if (positions[i] == Index.NOT_FOUND) {
        newMissing.set(i);
      } else {
        Long item = computeItem(positions[i]);
        if (item == null) {
          newMissing.set(i);
        } else {
          newData[i] = item;
        }
      }

      context.safepoint();
    }
    return new LongStorage(newData, positions.length, newMissing, getType());
  }

  @Override
  public Storage<Long> countMask(int[] counts, int total) {
    long[] newData = new long[total];
    BitSet newMissing = new BitSet();
    int pos = 0;
    Context context = Context.getCurrent();
    for (int i = 0; i < counts.length; i++) {
      Long item = computeItem(i);
      if (item == null) {
        newMissing.set(pos, pos + counts[i]);
        pos += counts[i];
      } else {
        long nonNullItem = item;
        for (int j = 0; j < counts[i]; j++) {
          newData[pos++] = nonNullItem;
        }
      }

      context.safepoint();
    }
    return new LongStorage(newData, total, newMissing, getType());
  }

  @Override
  public Storage<Long> slice(int offset, int limit) {
    int newSize = Math.min(size - offset, limit);
    long[] newData = new long[newSize];
    BitSet newMissing = new BitSet();
    Context context = Context.getCurrent();
    for (int i = 0; i < newSize; i++) {
      Long item = computeItem(offset + i);
      if (item == null) {
        newMissing.set(i);
      } else {
        newData[i] = item;
      }
      context.safepoint();
    }
    return new LongStorage(newData, newSize, newMissing, getType());
  }

  @Override
  public Storage<Long> slice(List<SliceRange> ranges) {
    int newSize = SliceRange.totalLength(ranges);
    long[] newData = new long[newSize];
    BitSet newMissing = new BitSet(newSize);
    int offset = 0;
    Context context = Context.getCurrent();
    for (SliceRange range : ranges) {
      int rangeStart = range.start();
      int length = range.end() - rangeStart;
      for (int i = 0; i < length; i++) {
        Long item = computeItem(rangeStart + i);
        if (item == null) {
          newMissing.set(offset + i);
        } else {
          newData[offset + i] = item;
        }
        context.safepoint();
      }
      offset += length;
    }

    return new LongStorage(newData, newSize, newMissing, getType());
  }

  @Override
  public AbstractLongStorage widen(IntegerType widerType) {
    // Currently the implementation only reports 64-bit type so there is no widening to do - we can
    // just return self.
    assert getType().equals(IntegerType.INT_64);
    return this;
  }

  @Override
  public Storage<Long> appendNulls(int count) {
    final ComputedNullableLongStorage parent = this;
    return new ComputedNullableLongStorage(parent.size + count) {
      @Override
      protected Long computeItem(int idx) {
        if (idx < parent.size) {
          return parent.computeItem(idx);
        }

        return null;
      }
    };
  }
}
