package org.enso.table.data.column.storage.type;

public record Text(long maxLength, boolean fixedLength) implements StorageType {
  public static final Text VARIABLE_LENGTH = new Text(-1, false);
}
