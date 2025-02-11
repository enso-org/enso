package org.enso.table.data.column.operation.cast;

import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.MixedStorageFacade;
import org.enso.table.data.column.storage.ObjectStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.StorageType;

public class ToMixedStorageConverter implements StorageConverter<Object> {
  @Override
  public boolean canApply(StorageType sourceType) {
    return true;
  }

  @Override
  public ColumnStorage<Object> cast(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    if (storage instanceof ObjectStorage objectStorage) {
      return objectStorage;
    } else if (storage instanceof MixedStorageFacade facade) {
      return facade;
    } else {
      // ToDo: Merge Storage and ColumnStorage
      return new MixedStorageFacade((Storage<?>) storage);
    }
  }
}
