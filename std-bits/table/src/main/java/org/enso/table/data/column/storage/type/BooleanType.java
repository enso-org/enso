package org.enso.table.data.column.storage.type;

public record BooleanType() implements StorageType {
  public static final BooleanType INSTANCE = new BooleanType();
}
