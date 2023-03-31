package org.enso.table.data.column.storage.type;

public record Integer(Bits bits) implements StorageType {
  public static final Integer INT_64 = new Integer(Bits.BITS_64);
}
