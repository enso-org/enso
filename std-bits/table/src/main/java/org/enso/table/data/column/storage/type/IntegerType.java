package org.enso.table.data.column.storage.type;

public record IntegerType(Bits bits) implements StorageType {
  public static final IntegerType INT_64 = new IntegerType(Bits.BITS_64);
}
