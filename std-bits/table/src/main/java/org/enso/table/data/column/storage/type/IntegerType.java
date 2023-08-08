package org.enso.table.data.column.storage.type;

public record IntegerType(Bits bits) implements StorageType {
  public static final IntegerType INT_64 = new IntegerType(Bits.BITS_64);

  public long getMaxValue() {
    return switch (bits) {
      case BITS_8 -> Byte.MAX_VALUE;
      case BITS_16 -> Short.MAX_VALUE;
      case BITS_32 -> Integer.MAX_VALUE;
      case BITS_64 -> Long.MAX_VALUE;
    };
  }

  public long getMinValue() {
    return switch (bits) {
      case BITS_8 -> Byte.MIN_VALUE;
      case BITS_16 -> Short.MIN_VALUE;
      case BITS_32 -> Integer.MIN_VALUE;
      case BITS_64 -> Long.MIN_VALUE;
    };
  }

  public boolean fits(long value) {
    if (this.bits == Bits.BITS_64) return true;
    return value >= getMinValue() && value <= getMaxValue();
  }

  public boolean fits(double value) {
    double min = getMinValue();
    double max = getMaxValue();
    return value >= min && value <= max;
  }
}
