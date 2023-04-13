package org.enso.table.data.column.storage.type;

public record TextType(long maxLength, boolean fixedLength) implements StorageType {
  public static final TextType VARIABLE_LENGTH = new TextType(-1, false);
}
