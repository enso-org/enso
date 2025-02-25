package org.enso.table.data.column.storage.type;

import org.enso.table.data.column.storage.ColumnStorage;

public record AnyObjectType() implements StorageType<Object> {
  public static final AnyObjectType INSTANCE = new AnyObjectType();

  @Override
  public boolean isNumeric() {
    return false;
  }

  @Override
  public boolean hasDate() {
    return false;
  }

  @Override
  public boolean hasTime() {
    return false;
  }

  @Override
  public ColumnStorage<Object> asTypedStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof AnyObjectType) {
      @SuppressWarnings("unchecked")
      var output = (ColumnStorage<Object>) storage;
      return output;
    }
    throw new IllegalArgumentException("Storage is not of AnyObjectType");
  }
}
