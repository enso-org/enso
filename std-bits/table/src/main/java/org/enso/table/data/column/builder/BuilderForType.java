package org.enso.table.data.column.builder;

import org.enso.table.data.column.storage.ColumnStorage;

public interface BuilderForType<T> extends Builder {
  @Override
  BuilderForType<T> append(Object o);

  @Override
  BuilderForType<T> appendNulls(int count);

  @Override
  ColumnStorage<T> seal();
}
