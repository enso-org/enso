package org.enso.table.data.column.storage.type;

import org.enso.table.data.column.storage.ColumnStorage;

public record NullType() implements StorageType<Void> {
  public static final NullType INSTANCE = new NullType();

  @Override
  public boolean isNumeric() {
    return true;
  }

  @Override
  public boolean hasDate() {
    return true;
  }

  @Override
  public boolean hasTime() {
    return true;
  }

  @Override
  public ColumnStorage<Void> asTypedStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof NullType) {
      @SuppressWarnings("unchecked")
      var output = (ColumnStorage<Void>) storage;
      return output;
    }
    throw new IllegalArgumentException("Storage is not of NullType");
  }
}
