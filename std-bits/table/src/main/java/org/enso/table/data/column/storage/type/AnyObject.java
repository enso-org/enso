package org.enso.table.data.column.storage.type;

public record AnyObject() implements StorageType {
  public static final AnyObject INSTANCE = new AnyObject();
}
