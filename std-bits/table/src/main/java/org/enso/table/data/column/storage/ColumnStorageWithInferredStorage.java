package org.enso.table.data.column.storage;

import org.enso.table.data.table.Column;

public interface ColumnStorageWithInferredStorage {
  static ColumnStorage<?> resolveStorage(Column storage) {
    return resolveStorage(storage.getStorage());
  }

  static ColumnStorage<?> resolveStorage(ColumnStorage<?> storage) {
    return storage instanceof ColumnStorageWithInferredStorage withInferredStorage
        ? resolveStorage(withInferredStorage.getInferredStorage())
        : storage;
  }

  ColumnStorage<?> getInferredStorage();
}
