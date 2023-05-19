package org.enso.table.data.column.operation.cast;

import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.object.NumericBuilder;
import org.enso.table.data.column.operation.CastProblemBuilder;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.DoubleStorage;
import org.enso.table.data.column.storage.LongStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.IntegerType;

public class ToIntegerStorageConverter implements StorageConverter<Long> {
  private final double min;
  private final double max;

  public ToIntegerStorageConverter(IntegerType targetType) {
    this.min = (double) targetType.getMinValue();
    this.max = (double) targetType.getMaxValue();
  }

  public Storage<Long> cast(Storage<?> storage, CastProblemBuilder problemBuilder) {
    if (storage instanceof LongStorage longStorage) {
      return longStorage;
    } else if (storage instanceof DoubleStorage doubleStorage) {
      int n = doubleStorage.size();
      NumericBuilder builder = NumericBuilder.createLongBuilder(n);
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
            problemBuilder.reportConversionFailure();
          }
        }
      }
      return builder.sealLong();
    } else if (storage instanceof BoolStorage boolStorage) {
      int n = boolStorage.size();
      NumericBuilder builder = NumericBuilder.createLongBuilder(n);
      for (int i = 0; i < n; i++) {
        if (boolStorage.isNa(i)) {
          builder.appendNulls(1);
        } else {
          boolean value = boolStorage.getItem(i);
          builder.appendLong(value ? 1 : 0);
        }
      }
      return builder.sealLong();
    } else if (storage.getType() instanceof AnyObjectType) {
      return castFromMixed(storage, problemBuilder);
    } else {
      throw new IllegalStateException("No known strategy for casting storage " + storage + " to Integer.");
    }
  }

  public Storage<Long> castFromMixed(Storage<?> mixedStorage, CastProblemBuilder problemBuilder) {
    NumericBuilder builder = NumericBuilder.createLongBuilder(mixedStorage.size());
    for (int i = 0; i < mixedStorage.size(); i++) {
      Object o = mixedStorage.getItemBoxed(i);
      if (o == null) {
        builder.appendNulls(1);
      } else if (o instanceof Boolean b) {
        builder.appendLong(b ? 1 : 0);
      } else if (NumericConverter.isCoercibleToLong(o)) {
        long x = NumericConverter.coerceToLong(o);
        builder.appendLong(x);
      } else if (NumericConverter.isDecimalLike(o)) {
        double x = NumericConverter.coerceToDouble(o);
        if (fitsInTargetRange(x)) {
          long converted = (long) x;
          builder.appendLong(converted);
        } else {
          problemBuilder.reportConversionFailure();
          builder.appendNulls(1);
        }
      } else {
        problemBuilder.reportConversionFailure();
        builder.appendNulls(1);
      }
    }

    return builder.sealLong();
  }

  private boolean fitsInTargetRange(double value) {
    return value >= min && value <= max;
  }
}
