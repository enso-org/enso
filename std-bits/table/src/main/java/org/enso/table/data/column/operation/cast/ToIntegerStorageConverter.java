package org.enso.table.data.column.operation.cast;

import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.object.LongBuilder;
import org.enso.table.data.column.builder.object.NumericBuilder;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.Bits;
import org.enso.table.data.column.storage.type.IntegerType;

public class ToIntegerStorageConverter implements StorageConverter<Long> {
  private final double min;
  private final double max;

  public ToIntegerStorageConverter(IntegerType targetType) {
    this.min = (double) targetType.getMinValue();
    this.max = (double) targetType.getMaxValue();

    if (targetType.bits() != Bits.BITS_64) {
      throw new IllegalStateException("Internal error: Only 64-bit integers are currently supported. To support other sizes, this class will need a few adaptations.");
    }
  }

  public Storage<Long> cast(Storage<?> storage, CastProblemBuilder problemBuilder) {
    if (storage instanceof LongStorage longStorage) {
      return longStorage;
    } else if (storage instanceof DoubleStorage doubleStorage) {
      return convertDoubleStorage(problemBuilder, doubleStorage);
    } else if (storage instanceof BoolStorage boolStorage) {
      return convertBoolStorage(boolStorage);
    } else if (storage.getType() instanceof AnyObjectType) {
      return castFromMixed(storage, problemBuilder);
    } else {
      throw new IllegalStateException("No known strategy for casting storage " + storage + " to Integer.");
    }
  }

  public Storage<Long> castFromMixed(Storage<?> mixedStorage, CastProblemBuilder problemBuilder) {
    LongBuilder builder = NumericBuilder.createLongBuilder(mixedStorage.size());
    for (int i = 0; i < mixedStorage.size(); i++) {
      Object o = mixedStorage.getItemBoxed(i);
      if (o == null) {
        builder.appendNulls(1);
      } else if (o instanceof Boolean b) {
        builder.appendLong(booleanAsLong(b));
      } else if (NumericConverter.isCoercibleToLong(o)) {
        long x = NumericConverter.coerceToLong(o);
        builder.appendLong(x);
      } else if (NumericConverter.isDecimalLike(o)) {
        double x = NumericConverter.coerceToDouble(o);
        if (fitsInTargetRange(x)) {
          long converted = (long) x;
          builder.appendLong(converted);
        } else {
          problemBuilder.reportConversionFailure(o);
          builder.appendNulls(1);
        }
      } else {
        problemBuilder.reportConversionFailure(o);
        builder.appendNulls(1);
      }
    }

    return builder.seal();
  }

  private boolean fitsInTargetRange(double value) {
    return value >= min && value <= max;
  }

  private Storage<Long> convertBoolStorage(BoolStorage boolStorage) {
    int n = boolStorage.size();
    LongBuilder builder = NumericBuilder.createLongBuilder(n);
    for (int i = 0; i < n; i++) {
      if (boolStorage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        boolean value = boolStorage.getItem(i);
        builder.appendLong(booleanAsLong(value));
      }
    }
    return builder.seal();
  }

  private Storage<Long> convertDoubleStorage(CastProblemBuilder problemBuilder, DoubleStorage doubleStorage) {
    int n = doubleStorage.size();
    LongBuilder builder = NumericBuilder.createLongBuilder(n);
    for (int i = 0; i < n; i++) {
      if (doubleStorage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        double value = doubleStorage.getItemDouble(i);
        if (fitsInTargetRange(value)) {
          long converted = (long) value;
          builder.appendLong(converted);
        } else {
          builder.appendNulls(1);
          problemBuilder.reportConversionFailure(value);
        }
      }
    }
    return builder.seal();
  }

  public static long booleanAsLong(boolean value) {
    return value ? 1L : 0L;
  }
}
