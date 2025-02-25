package org.enso.table.data.column.storage.type;

import org.enso.table.data.column.storage.ColumnStorage;

import java.math.BigDecimal;

public record BigDecimalType() implements StorageType<BigDecimal> {
  public static final BigDecimalType INSTANCE = new BigDecimalType();

  @Override
  public boolean isNumeric() {
    return true;
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
  public ColumnStorage<BigDecimal> asTypedStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof BigDecimalType) {
      @SuppressWarnings("unchecked")
      var output = (ColumnStorage<BigDecimal>) storage;
      return output;
    }
    throw new IllegalArgumentException("Storage is not of BigDecimalType");
  }
}
