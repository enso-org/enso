package org.enso.table.data.column.storage.type;

public record Float(Bits bits) implements StorageType {
  public Float {
    if (bits != Bits.BITS_64) {
      throw new IllegalArgumentException("Only 64-bit floats are currently supported.");
    }
  }
}
