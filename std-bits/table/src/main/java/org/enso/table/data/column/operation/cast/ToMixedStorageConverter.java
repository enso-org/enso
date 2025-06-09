package org.enso.table.data.column.operation.cast;

import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.MixedStorageFacade;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.StorageType;

public class ToMixedStorageConverter implements StorageConverter<Object> {
  @Override
  public boolean canApply(StorageType<?> sourceType) {
    return true;
  }

  @Override
  public ColumnStorage<Object> cast(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    if (storage.getType() instanceof AnyObjectType) {
      // Already the correct type.
      return AnyObjectType.INSTANCE.asTypedStorage(storage);
    }

    return new MixedStorageFacade(storage);
  }
}
