package org.enso.table.data.column.storage.type;

public record DateTimeType() implements StorageType {
  public static final DateTimeType INSTANCE = new DateTimeType();

  @Override
  public boolean isNumeric() {
    return false;
  }

  @Override
  public boolean hasDate() {
    return true;
  }

  @Override
  public boolean hasTime() {
    return true;
  }
}
