package org.enso.table.data.column.builder;

import org.enso.table.data.column.storage.Storage;

public interface BuilderForType<T> extends Builder {
  @Override
  BuilderForType<T> appendNulls(int count);

  @Override
  Storage<T> seal();
}
