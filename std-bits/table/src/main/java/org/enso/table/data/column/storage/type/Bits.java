package org.enso.table.data.column.storage.type;

/**
 * Represents sizes for some of our storages.
 *
 * <p>This corresponds to the Enso type {@code Bits}.
 */
public enum Bits {
  BITS_8(8),
  BITS_16(16),
  BITS_32(32),
  BITS_64(64);
  private final int size;

  Bits(int size) {
    this.size = size;
  }

  public int toInteger() {
    return this.size;
  }

  public static Bits fromInteger(int size) {
    return switch (size) {
      case 8 -> BITS_8;
      case 16 -> BITS_16;
      case 32 -> BITS_32;
      case 64 -> BITS_64;
      default -> throw new IllegalArgumentException("Invalid bit-size: " + size);
    };
  }
}
