package org.enso.table.data.column.storage.type;

import java.math.BigInteger;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForLong;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.problems.ProblemAggregator;

public final class IntegerType implements StorageType<Long>, NumericType {
  public static final IntegerType INT_64 = new IntegerType(Bits.BITS_64);
  public static final IntegerType INT_32 = new IntegerType(Bits.BITS_32);
  public static final IntegerType INT_16 = new IntegerType(Bits.BITS_16);
  public static final IntegerType INT_8 = new IntegerType(Bits.BITS_8);

  private final Bits bits;

  private IntegerType(Bits bits) {
    this.bits = bits;
  }

  @Override
  public char typeChar() {
    return 'I';
  }

  @Override
  public long size() {
    return switch (bits) {
      case BITS_64 -> 64;
      case BITS_32 -> 32;
      case BITS_16 -> 16;
      case BITS_8 -> 8;
    };
  }

  /** Returns the number of bits of this integer type. */
  public Bits bits() {
    return bits;
  }

  @Override
  public boolean isNumeric() {
    return true;
  }

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

  public boolean fits(BigInteger value) {
    if (value.bitLength() > 63) {
      return false;
    } else {
      return fits(value.longValue());
    }
  }

  /**
   * Checks if this type can hold values of otherType - i.e. if otherType has the same or smaller
   * number of bits.
   */
  public boolean fits(IntegerType otherType) {
    return size() >= otherType.size();
  }

  public static IntegerType smallestFitting(long value, boolean allow8bit) {
    if (allow8bit && INT_8.fits(value)) return INT_8;
    if (INT_16.fits(value)) return INT_16;
    if (INT_32.fits(value)) return INT_32;
    return INT_64;
  }

  /**
   * Returns a common type that will fit values from either one (essentially the larger type of the
   * two).
   */
  public static IntegerType commonType(IntegerType type1, IntegerType type2) {
    return type1.size() >= type2.size() ? type1 : type2;
  }

  @Override
  public boolean isOfType(StorageType<?> other) {
    return other instanceof IntegerType;
  }

  @Override
  public Long valueAsType(Object value) {
    if (NumericConverter.isCoercibleToLong(value)) {
      return NumericConverter.coerceToLong(value);
    }
    return null;
  }

  @Override
  public BuilderForLong makeBuilder(long initialCapacity, ProblemAggregator problemAggregator) {
    return Builder.getForLong(this, initialCapacity, problemAggregator);
  }

  @Override
  public ColumnLongStorage asTypedStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof IntegerType) {
      var output = (ColumnLongStorage) storage;
      return output;
    }
    throw new IllegalArgumentException("Storage is not of IntegerType");
  }
}
