package org.enso.table.data.column.storage;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;
import org.graalvm.polyglot.Context;

public abstract class SpecializedStorage<T> extends Storage<T> {
  protected abstract SpecializedStorage<T> newInstance(T[] data);

  protected abstract T[] newUnderlyingArray(int size);

  /**
   * @param data the underlying data
   */
  protected SpecializedStorage(StorageType<T> type, T[] data) {
    this.type = type;
    this.data = data;
  }

  protected final T[] data;
  private final StorageType<T> type;

  @Override
  public final long getSize() {
    return data.length;
  }

  @Override
  public StorageType<T> getType() {
    return type;
  }

  /**
   * @param idx an index
   * @return the data item contained at the given index.
   */
  public T getItemBoxed(long idx) {
    if (idx < 0 || idx >= data.length) {
      throw new IndexOutOfBoundsException(idx);
    }
    return data[(int) idx];
  }

  @Override
  public boolean isNothing(long idx) {
    return this.getItemBoxed(idx) == null;
  }

  @Override
  public ColumnStorage<T> applyFilter(BitSet filterMask, int newLength) {
    Context context = Context.getCurrent();
    T[] newData = newUnderlyingArray(newLength);
    int resIx = 0;
    for (int i = 0; i < data.length; i++) {
      if (filterMask.get(i)) {
        newData[resIx++] = data[i];
      }

      context.safepoint();
    }
    return newInstance(newData);
  }

  @Override
  public ColumnStorage<T> applyMask(OrderMask mask) {
    Context context = Context.getCurrent();
    T[] newData = newUnderlyingArray(mask.length());
    for (int i = 0; i < mask.length(); i++) {
      int position = mask.get(i);
      newData[i] = position == OrderMask.NOT_FOUND_INDEX ? null : data[position];
      context.safepoint();
    }
    return newInstance(newData);
  }

  public T[] getData() {
    return data;
  }

  @Override
  public ColumnStorage<T> slice(int offset, int limit) {
    int newSize = Math.min(data.length - offset, limit);
    T[] newData = newUnderlyingArray(newSize);
    System.arraycopy(data, offset, newData, 0, newSize);
    return newInstance(newData);
  }

  @Override
  public ColumnStorage<T> slice(List<SliceRange> ranges) {
    Context context = Context.getCurrent();
    int newSize = SliceRange.totalLength(ranges);
    T[] newData = newUnderlyingArray(newSize);
    int offset = 0;
    for (SliceRange range : ranges) {
      int length = range.end() - range.start();
      System.arraycopy(data, range.start(), newData, offset, length);
      offset += length;
      context.safepoint();
    }

    return newInstance(newData);
  }

  @Override
  public Iterator<T> iterator() {
    return Arrays.stream(data).iterator();
  }

  @Override
  public ColumnStorageIterator<T> iteratorWithIndex() {
    return new SpecializedStorageIterator<>(data);
  }

  private static class SpecializedStorageIterator<T> implements ColumnStorageIterator<T> {
    private final T[] data;
    private int index = -1;

    public SpecializedStorageIterator(T[] data) {
      this.data = data;
    }

    @Override
    public T getItemBoxed() {
      return data[index];
    }

    @Override
    public boolean isNothing() {
      return data[index] == null;
    }

    @Override
    public boolean hasNext() {
      return index + 1 < data.length;
    }

    @Override
    public T next() {
      if (!hasNext()) {
        throw new NoSuchElementException();
      }
      return data[++index];
    }

    @Override
    public long getIndex() {
      return index;
    }

    @Override
    public boolean moveNext() {
      if (!hasNext()) {
        return false;
      }
      index++;
      return true;
    }
  }
}
