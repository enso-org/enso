package org.enso.table.data.column.storage.type;

public record BigIntegerType() implements StorageType {
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
}
