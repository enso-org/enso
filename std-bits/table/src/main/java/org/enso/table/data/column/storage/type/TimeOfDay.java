package org.enso.table.data.column.storage.type;

public record TimeOfDay() implements StorageType {
  public static final TimeOfDay INSTANCE = new TimeOfDay();
}
