package org.enso.table.data.column.storage.type;

public record DateType() implements StorageType {
  public static final DateType INSTANCE = new DateType();

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
    return false;
  }
}
