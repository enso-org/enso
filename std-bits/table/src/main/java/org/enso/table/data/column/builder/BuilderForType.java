package org.enso.table.data.column.builder;

import org.enso.table.data.column.storage.Storage;

public interface BuilderForType<T> extends Builder {
  @Override
  Storage<T> seal();
}
