package org.enso.table.data.column.storage.type;

public record Date() implements StorageType {
  public static final Date INSTANCE = new Date();
}
