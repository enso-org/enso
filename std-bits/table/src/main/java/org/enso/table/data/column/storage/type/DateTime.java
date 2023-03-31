package org.enso.table.data.column.storage.type;

public record DateTime() implements StorageType {
  public static final DateTime INSTANCE = new DateTime();
}
