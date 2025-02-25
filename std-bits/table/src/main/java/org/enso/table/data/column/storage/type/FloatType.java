package org.enso.table.data.column.storage.type;

import org.enso.table.data.column.storage.ColumnStorage;

public record FloatType(Bits bits) implements StorageType<Double> {
  public static final FloatType FLOAT_64 = new FloatType(Bits.BITS_64);

  public FloatType {
    if (bits != Bits.BITS_64) {
      throw new IllegalArgumentException("Only 64-bit floats are currently supported.");
    }
  }

  @Override
  public boolean isNumeric() {
    return true;
  }

  @Override
  public boolean hasDate() {
    return false;
  }

  @Override
  public boolean hasTime() {
    return false;
  }

  @Override
  public ColumnStorage<Double> asTypedStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof FloatType) {
      @SuppressWarnings("unchecked")
      var output = (ColumnStorage<Double>) storage;
      return output;
    }
    throw new IllegalArgumentException("Storage is not of FloatType");
  }
}
