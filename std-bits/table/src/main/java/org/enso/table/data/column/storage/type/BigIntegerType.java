package org.enso.table.data.column.storage.type;

import org.enso.table.data.column.storage.ColumnStorage;

import java.math.BigInteger;

public record BigIntegerType() implements StorageType<BigInteger> {
  public static final BigIntegerType INSTANCE = new BigIntegerType();

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
  public ColumnStorage<BigInteger> asTypedStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof BigIntegerType) {
      @SuppressWarnings("unchecked")
      var output = (ColumnStorage<BigInteger>) storage;
      return output;
    }
    throw new IllegalArgumentException("Storage is not of BigIntegerType");
  }
}
