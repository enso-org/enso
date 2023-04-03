package org.enso.table.data.column.storage.type;

public record AnyObjectType() implements StorageType {
  public static final AnyObjectType INSTANCE = new AnyObjectType();
}
