package org.enso.table.data.column.storage.type;

public record BigIntegerType() implements StorageType {
  public static final BigIntegerType INSTANCE = new BigIntegerType();
}
