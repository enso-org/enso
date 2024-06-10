package org.enso.table.data.column.storage.type;

public record BigDecimalType() implements StorageType {
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
}
