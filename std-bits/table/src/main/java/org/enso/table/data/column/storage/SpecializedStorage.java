package org.enso.table.data.column.storage;

import java.util.Arrays;
import java.util.Iterator;
import org.enso.table.data.column.storage.type.StorageType;

public abstract class SpecializedStorage<T> extends Storage<T> {
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

  public T[] getData() {
    return data;
  }

  @Override
  public Iterator<T> iterator() {
    return Arrays.stream(data).iterator();
  }
}
