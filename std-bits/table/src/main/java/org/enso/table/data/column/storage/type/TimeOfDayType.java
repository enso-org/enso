package org.enso.table.data.column.storage.type;

public record TimeOfDayType() implements StorageType {
  public static final TimeOfDayType INSTANCE = new TimeOfDayType();

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
    return true;
  }
}
