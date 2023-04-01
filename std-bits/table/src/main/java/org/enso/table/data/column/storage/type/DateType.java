package org.enso.table.data.column.storage.type;

public record DateType() implements StorageType {
  public static final DateType INSTANCE = new DateType();
}
