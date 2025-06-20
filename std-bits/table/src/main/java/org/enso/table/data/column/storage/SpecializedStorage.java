package org.enso.table.data.column.storage;

import java.util.Arrays;
import java.util.Iterator;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.mask.OrderMask;
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
  public Iterator<T> iterator() {
    return Arrays.stream(data).iterator();
  }
}
