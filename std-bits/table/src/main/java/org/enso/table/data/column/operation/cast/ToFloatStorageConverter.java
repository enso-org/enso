package org.enso.table.data.column.operation.cast;

import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.DoubleBuilder;
import org.enso.table.data.column.builder.NumericBuilder;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.Bits;
import org.enso.table.data.column.storage.type.FloatType;
import org.graalvm.polyglot.Context;

import java.math.BigInteger;
import java.util.BitSet;

public class ToFloatStorageConverter implements StorageConverter<Double> {
  public ToFloatStorageConverter(FloatType targetType) {
    if (targetType.bits() != Bits.BITS_64) {
      throw new IllegalStateException("Internal error: Only 64-bit floats are currently supported.");
    }
  }

  public Storage<Double> cast(Storage<?> storage, CastProblemBuilder problemBuilder) {
    if (storage instanceof DoubleStorage doubleStorage) {
      return doubleStorage;
    } else if (storage instanceof AbstractLongStorage longStorage) {
      return convertLongStorage(longStorage, problemBuilder);
    } else if (storage instanceof BoolStorage boolStorage) {
      return convertBoolStorage(boolStorage, problemBuilder);
    } else if (storage instanceof BigIntegerStorage bigIntegerStorage) {
      return convertBigIntegerStorage(bigIntegerStorage, problemBuilder);
    } else if (storage.getType() instanceof AnyObjectType) {
      return castFromMixed(storage, problemBuilder);
    } else {
      throw new IllegalStateException("No known strategy for casting storage " + storage + " to Float.");
    }
  }

  public Storage<Double> castFromMixed(Storage<?> mixedStorage, CastProblemBuilder problemBuilder) {
    Context context = Context.getCurrent();
    DoubleBuilder builder = NumericBuilder.createDoubleBuilder(mixedStorage.size());
    for (int i = 0; i < mixedStorage.size(); i++) {
      Object o = mixedStorage.getItemBoxed(i);
      if (o == null) {
        builder.appendNulls(1);
      } else if (o instanceof Boolean b) {
        builder.appendDouble(booleanAsDouble(b));
      } else if (NumericConverter.isCoercibleToLong(o)) {
        long x = NumericConverter.coerceToLong(o);
        builder.appendLong(x);
      } else if (NumericConverter.isDecimalLike(o)) {
        double x = NumericConverter.coerceToDouble(o);
        builder.appendDouble(x);
      } else if (o instanceof BigInteger bigInteger) {
        builder.appendBigInteger(bigInteger);
      } else {
        problemBuilder.reportConversionFailure(o);
        builder.appendNulls(1);
      }

      context.safepoint();
    }

    problemBuilder.aggregateOtherProblems(builder.getProblems());
    return builder.seal();
  }

  private Storage<Double> convertLongStorage(AbstractLongStorage longStorage, CastProblemBuilder problemBuilder) {
    int n = longStorage.size();
    DoubleBuilder builder = NumericBuilder.createDoubleBuilder(n);
    for (int i = 0; i < n; i++) {
      if (longStorage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        long value = longStorage.getItem(i);
        builder.appendLong(value);
      }
    }

    problemBuilder.aggregateOtherProblems(builder.getProblems());
    return builder.seal();
  }

  private Storage<Double> convertBoolStorage(BoolStorage boolStorage, CastProblemBuilder problemBuilder) {
    int n = boolStorage.size();
    DoubleBuilder builder = NumericBuilder.createDoubleBuilder(n);
    for (int i = 0; i < n; i++) {
      if (boolStorage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        boolean value = boolStorage.getItem(i);
        builder.appendDouble(booleanAsDouble(value));
      }
    }

    problemBuilder.aggregateOtherProblems(builder.getProblems());
    return builder.seal();
  }

  public static double booleanAsDouble(boolean value) {
    return value ? 1.0 : 0.0;
  }

  private Storage<Double> convertBigIntegerStorage(Storage<BigInteger> storage, CastProblemBuilder problemBuilder) {
    int n = storage.size();
    DoubleBuilder builder = NumericBuilder.createDoubleBuilder(n);
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

    problemBuilder.aggregateOtherProblems(builder.getProblems());
    return builder.seal();
  }
}
