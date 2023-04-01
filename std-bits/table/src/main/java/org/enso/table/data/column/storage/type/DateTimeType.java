package org.enso.table.data.column.storage.type;

public record DateTimeType() implements StorageType {
  public static final DateTimeType INSTANCE = new DateTimeType();
}
