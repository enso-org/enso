package org.enso.table.data.column.storage.type;

import org.enso.table.data.column.storage.ColumnStorage;

public record BooleanType() implements StorageType<Boolean> {
  public static final BooleanType INSTANCE = new BooleanType();

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
  public ColumnStorage<Boolean> asTypedStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof BooleanType) {
      @SuppressWarnings("unchecked")
      var output = (ColumnStorage<Boolean>) storage;
      return output;
    }
    throw new IllegalArgumentException("Storage is not of BooleanType");
  }
}
