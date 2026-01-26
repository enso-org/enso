package org.enso.table.data.column.storage;

import java.util.function.LongFunction;
import org.enso.table.data.column.storage.type.StorageType;

public class ColumnStorageProxy<T> extends Storage<T> {
  private final ColumnStorage<?> originalStorage;
  private final LongFunction<T> getter;

  public ColumnStorageProxy(
      StorageType<T> storageType, ColumnStorage<?> originalStorage, LongFunction<T> getter) {
    super(storageType);
    this.originalStorage = originalStorage;
    this.getter = getter;
  }

  @Override
  public long getSize() {
    return originalStorage.getSize();
  }

  @Override
  public T getItemBoxed(long index) {
    return getter.apply(index);
  }
}
