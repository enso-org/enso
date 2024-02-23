package org.enso.table.data.column.storage.numeric;

import java.util.BitSet;
import java.util.List;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.IntegerType;
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

  protected BitSet isNothing = null;

  protected abstract Long computeItem(int idx);

  protected ComputedNullableLongStorage(int size) {
    this.size = size;
  }

  @Override
  public int size() {
    return size;
  }

  @Override
  public IntegerType getType() {
    return IntegerType.INT_64;
  }

  @Override
  public boolean isNothing(long idx) {
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
  public BitSet getIsNothingMap() {
    if (isNothing == null) {
      // Only compute once as needed.
      BitSet newIsNothing = new BitSet();
      Context context = Context.getCurrent();
      for (int i = 0; i < size; i++) {
        if (computeItem(i) == null) {
          newIsNothing.set(i);
        }

        context.safepoint();
      }
      isNothing = newIsNothing;
    }
    return isNothing;
  }

  @Override
  public Storage<Long> applyFilter(BitSet filterMask, int newLength) {
    BitSet newIsNothing = new BitSet();
    long[] newData = new long[newLength];
    int resIx = 0;
    Context context = Context.getCurrent();
    for (int i = 0; i < size; i++) {
      if (filterMask.get(i)) {
        Long item = computeItem(i);
        if (item == null) {
          newIsNothing.set(resIx++);
        } else {
          newData[resIx++] = item;
        }
      }

      context.safepoint();
    }
    return new LongStorage(newData, newLength, newIsNothing, getType());
  }

  @Override
  public Storage<Long> applyMask(OrderMask mask) {
    long[] newData = new long[mask.length()];
    BitSet newIsNothing = new BitSet();
    Context context = Context.getCurrent();
    for (int i = 0; i < mask.length(); i++) {
      int position = mask.get(i);
      if (position == Storage.NOT_FOUND_INDEX) {
        newIsNothing.set(i);
      } else {
        Long item = computeItem(position);
        if (item == null) {
          newIsNothing.set(i);
        } else {
          newData[i] = item;
        }
      }

      context.safepoint();
    }
    return new LongStorage(newData, newData.length, newIsNothing, getType());
  }

  @Override
  public Storage<Long> slice(int offset, int limit) {
    int newSize = Math.min(size - offset, limit);
    long[] newData = new long[newSize];
    BitSet newIsNothing = new BitSet();
    Context context = Context.getCurrent();
    for (int i = 0; i < newSize; i++) {
      Long item = computeItem(offset + i);
      if (item == null) {
        newIsNothing.set(i);
      } else {
        newData[i] = item;
      }
      context.safepoint();
    }
    return new LongStorage(newData, newSize, newIsNothing, getType());
  }

  @Override
  public Storage<Long> slice(List<SliceRange> ranges) {
    int newSize = SliceRange.totalLength(ranges);
    long[] newData = new long[newSize];
    BitSet newIsNothing = new BitSet(newSize);
    int offset = 0;
    Context context = Context.getCurrent();
    for (SliceRange range : ranges) {
      int rangeStart = range.start();
      int length = range.end() - rangeStart;
      for (int i = 0; i < length; i++) {
        Long item = computeItem(rangeStart + i);
        if (item == null) {
          newIsNothing.set(offset + i);
        } else {
          newData[offset + i] = item;
        }
        context.safepoint();
      }
      offset += length;
    }

    return new LongStorage(newData, newSize, newIsNothing, getType());
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
