package org.enso.table.data.column.storage.type;

public enum Bits {
  BITS_8,
  BITS_16,
  BITS_32,
  BITS_64;

  public int toBits() {
    return switch (this) {
      case BITS_8 -> 8;
      case BITS_16 -> 16;
      case BITS_32 -> 32;
      case BITS_64 -> 64;
    };
  }
}
