package org.enso.table.data.column.operation.cast;

import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.object.NumericBuilder;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.DoubleStorage;
import org.enso.table.data.column.storage.LongStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.Bits;
import org.enso.table.data.column.storage.type.FloatType;

public class ToFloatStorageConverter implements StorageConverter<Double> {
  public ToFloatStorageConverter(FloatType targetType) {
    if (targetType.bits() != Bits.BITS_64) {
      throw new IllegalStateException("Internal error: Only 64-bit floats are currently supported.");
    }
  }

  public Storage<Double> cast(Storage<?> storage, CastProblemBuilder problemBuilder) {
    if (storage instanceof DoubleStorage doubleStorage) {
      return doubleStorage;
    } else if (storage instanceof LongStorage longStorage) {
      int n = longStorage.size();
      NumericBuilder builder = NumericBuilder.createDoubleBuilder(n);
      for (int i = 0; i < n; i++) {
        if (longStorage.isNa(i)) {
          builder.appendNulls(1);
        } else {
          double value = longStorage.getItemDouble(i);
          builder.appendDouble(value);
        }
      }
      return builder.sealDouble();
    } else if (storage instanceof BoolStorage boolStorage) {
      int n = boolStorage.size();
      NumericBuilder builder = NumericBuilder.createDoubleBuilder(n);
      for (int i = 0; i < n; i++) {
        if (boolStorage.isNa(i)) {
          builder.appendNulls(1);
        } else {
          boolean value = boolStorage.getItem(i);
          builder.appendDouble(value ? 1.0 : 0.0);
        }
      }
      return builder.sealDouble();
    } else if (storage.getType() instanceof AnyObjectType) {
      return castFromMixed(storage, problemBuilder);
    } else {
      throw new IllegalStateException("No known strategy for casting storage " + storage + " to Float.");
    }
  }

  public Storage<Double> castFromMixed(Storage<?> mixedStorage, CastProblemBuilder problemBuilder) {
    NumericBuilder builder = NumericBuilder.createDoubleBuilder(mixedStorage.size());
    for (int i = 0; i < mixedStorage.size(); i++) {
      Object o = mixedStorage.getItemBoxed(i);
      if (o == null) {
        builder.appendNulls(1);
      } else if (o instanceof Boolean b) {
        builder.appendDouble(b ? 1 : 0);
      } else if (NumericConverter.isCoercibleToDouble(o)) {
        double x = NumericConverter.coerceToDouble(o);
        builder.appendDouble(x);
      } else {
        problemBuilder.reportConversionFailure();
        builder.appendNulls(1);
      }
    }

    return builder.sealDouble();
  }
}
