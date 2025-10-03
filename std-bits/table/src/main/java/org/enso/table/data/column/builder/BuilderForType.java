package org.enso.table.data.column.builder;

import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.StorageType;

public interface BuilderForType<T> extends Builder {
  @Override
  StorageType<T> getType();

  @Override
  BuilderForType<T> append(Object o);

  @Override
  BuilderForType<T> appendNulls(int count);

  @Override
  ColumnStorage<T> seal();
}
