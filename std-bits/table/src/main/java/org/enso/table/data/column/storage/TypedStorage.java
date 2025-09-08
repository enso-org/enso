package org.enso.table.data.column.storage;

import java.util.Arrays;
import java.util.Iterator;
import org.enso.table.data.column.storage.type.StorageType;

public class TypedStorage<T> extends Storage<T> {
  /**
   * @param data the underlying data
   */
  public TypedStorage(StorageType<T> type, T[] data) {
    super(type);
    this.data = data;
  }

  protected final T[] data;

  @Override
  public final long getSize() {
    return data.length;
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

  public T[] getData() {
    return data;
  }

  @Override
  public Iterator<T> iterator() {
    return Arrays.stream(data).iterator();
  }
}
