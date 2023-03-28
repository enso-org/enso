package org.enso.table.data.column.storage.type;

/** Represents sizes for some of our storages.
 * <p>
 * This corresponds to the Enso type {@code Bits}.
 */
public enum Bits {
  BITS_8,
  BITS_16,
  BITS_32,
  BITS_64;

  public int toInteger() {
    return switch (this) {
      case BITS_8 -> 8;
      case BITS_16 -> 16;
      case BITS_32 -> 32;
      case BITS_64 -> 64;
    };
  }
}
