package org.enso.table.data.column.storage;

import org.enso.table.data.column.storage.type.StorageType;

public interface ColumnStorageWithInferredStorage {
  StorageType inferPreciseType();

  ColumnStorage getInferredStorage();
}
