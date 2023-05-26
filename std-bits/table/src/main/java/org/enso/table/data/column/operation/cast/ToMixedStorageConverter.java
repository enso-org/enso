package org.enso.table.data.column.operation.cast;

import org.enso.table.data.column.storage.MixedStorageFacade;
import org.enso.table.data.column.storage.ObjectStorage;
import org.enso.table.data.column.storage.Storage;

public class ToMixedStorageConverter implements StorageConverter<Object> {
  @Override
  public Storage<Object> cast(Storage<?> storage, CastProblemBuilder problemBuilder) {
    if (storage instanceof ObjectStorage objectStorage) {
      return objectStorage;
    } else if (storage instanceof MixedStorageFacade facade) {
      return facade;
    } else {
      return new MixedStorageFacade(storage);
    }
  }
}
