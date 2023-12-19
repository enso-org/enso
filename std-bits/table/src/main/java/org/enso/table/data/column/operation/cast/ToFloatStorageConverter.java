package org.enso.table.data.column.operation.cast;

import java.math.BigInteger;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.DoubleBuilder;
import org.enso.table.data.column.builder.NumericBuilder;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.Bits;
import org.enso.table.data.column.storage.type.FloatType;
import org.graalvm.polyglot.Context;

public class ToFloatStorageConverter implements StorageConverter<Double> {
  public ToFloatStorageConverter(FloatType targetType) {
    if (targetType.bits() != Bits.BITS_64) {
      throw new IllegalStateException(
          "Internal error: Only 64-bit floats are currently supported.");
    }
  }

  @Override
  public Storage<Double> cast(Storage<?> storage, CastProblemAggregator problemAggregator) {
    if (storage instanceof DoubleStorage doubleStorage) {
      return doubleStorage;
    } else if (storage instanceof AbstractLongStorage longStorage) {
      return convertLongStorage(longStorage, problemAggregator);
    } else if (storage instanceof BoolStorage boolStorage) {
      return convertBoolStorage(boolStorage, problemAggregator);
    } else if (storage instanceof BigIntegerStorage bigIntegerStorage) {
      return convertBigIntegerStorage(bigIntegerStorage, problemAggregator);
    } else if (storage.getType() instanceof AnyObjectType) {
      return castFromMixed(storage, problemAggregator);
    } else {
      throw new IllegalStateException(
          "No known strategy for casting storage " + storage + " to Float.");
    }
  }

  public Storage<Double> castFromMixed(
      Storage<?> mixedStorage, CastProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    DoubleBuilder builder =
        NumericBuilder.createDoubleBuilder(mixedStorage.size(), problemAggregator);
    for (int i = 0; i < mixedStorage.size(); i++) {
      Object o = mixedStorage.getItemBoxed(i);
      if (o == null) {
        builder.appendNulls(1);
      } else if (o instanceof Boolean b) {
        builder.appendDouble(booleanAsDouble(b));
      } else if (NumericConverter.isCoercibleToLong(o)) {
        long x = NumericConverter.coerceToLong(o);
        builder.appendLong(x);
      } else if (NumericConverter.isFloatLike(o)) {
        double x = NumericConverter.coerceToDouble(o);
        builder.appendDouble(x);
      } else if (o instanceof BigInteger bigInteger) {
        builder.appendBigInteger(bigInteger);
      } else {
        problemAggregator.reportConversionFailure(o);
        builder.appendNulls(1);
      }

      context.safepoint();
    }

    return builder.seal();
  }

  private Storage<Double> convertLongStorage(
      AbstractLongStorage longStorage, CastProblemAggregator problemAggregator) {
    int n = longStorage.size();
    DoubleBuilder builder = NumericBuilder.createDoubleBuilder(n, problemAggregator);
    for (int i = 0; i < n; i++) {
      if (longStorage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        long value = longStorage.getItem(i);
        builder.appendLong(value);
      }
    }

    return builder.seal();
  }

  private Storage<Double> convertBoolStorage(
      BoolStorage boolStorage, CastProblemAggregator problemAggregator) {
    int n = boolStorage.size();
    DoubleBuilder builder = NumericBuilder.createDoubleBuilder(n, problemAggregator);
    for (int i = 0; i < n; i++) {
      if (boolStorage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        boolean value = boolStorage.getItem(i);
        builder.appendDouble(booleanAsDouble(value));
      }
    }

    return builder.seal();
  }

  public static double booleanAsDouble(boolean value) {
    return value ? 1.0 : 0.0;
  }

  private Storage<Double> convertBigIntegerStorage(
      Storage<BigInteger> storage, CastProblemAggregator problemAggregator) {
    int n = storage.size();
    DoubleBuilder builder = NumericBuilder.createDoubleBuilder(n, problemAggregator);
    Context context = Context.getCurrent();
    for (int i = 0; i < n; i++) {
      BigInteger value = storage.getItemBoxed(i);
      if (value == null) {
        builder.appendNulls(1);
      } else {
        builder.appendBigInteger(value);
      }

      context.safepoint();
    }

    return builder.seal();
  }
}
