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
}
