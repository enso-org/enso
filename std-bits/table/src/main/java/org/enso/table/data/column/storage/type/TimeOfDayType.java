package org.enso.table.data.column.storage.type;

public record TimeOfDayType() implements StorageType {
  public static final TimeOfDayType INSTANCE = new TimeOfDayType();
}
