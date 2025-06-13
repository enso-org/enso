package org.enso.table.data.column.storage;

import org.enso.table.data.table.Column;

public interface ColumnStorageWithInferredStorage {
  static ColumnStorage<?> resolveStorage(Column storage) {
    return resolveStorage(storage.getStorage());
  }

  static ColumnStorage<?> resolveStorage(ColumnStorage<?> storage) {
    if (storage instanceof ColumnStorageWithInferredStorage withInferredStorage) {
      var inferredStorage = withInferredStorage.getInferredStorage();
      if (inferredStorage != null) {
        return resolveStorage(inferredStorage);
      }
    }
    return storage;
  }

  ColumnStorage<?> getInferredStorage();
}
