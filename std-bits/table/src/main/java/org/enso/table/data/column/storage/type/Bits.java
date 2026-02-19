package org.enso.table.data.column.storage.type;

import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

/**
 * Represents sizes for some of our storages.
 *
 * <p>This corresponds to the Enso type {@code Bits}.
 */
public enum Bits {
  BITS_8,
  BITS_16,
  BITS_32,
  BITS_64;

  public static Bits fromInteger(int size) {
    return switch (size) {
      case 8 -> BITS_8;
      case 16 -> BITS_16;
      case 32 -> BITS_32;
      case 64 -> BITS_64;
      default -> throw new IllegalArgumentException("Invalid bit-size: " + size);
    };
  }

  public static Value asEnsoValue(Bits bits) {
    return switch (bits) {
      case BITS_8 -> EnsoMeta.makeInstance(StorageType.ENSO_MODULE, "Bits", "Bits_8");
      case BITS_16 -> EnsoMeta.makeInstance(StorageType.ENSO_MODULE, "Bits", "Bits_16");
      case BITS_32 -> EnsoMeta.makeInstance(StorageType.ENSO_MODULE, "Bits", "Bits_32");
      case BITS_64 -> EnsoMeta.makeInstance(StorageType.ENSO_MODULE, "Bits", "Bits_64");
    };
  }
}
