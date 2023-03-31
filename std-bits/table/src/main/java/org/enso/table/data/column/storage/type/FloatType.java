package org.enso.table.data.column.storage.type;

public record FloatType(Bits bits) implements StorageType {
  public static final FloatType FLOAT_64 = new FloatType(Bits.BITS_64);

  public FloatType {
    if (bits != Bits.BITS_64) {
      throw new IllegalArgumentException("Only 64-bit floats are currently supported.");
    }
  }
}
