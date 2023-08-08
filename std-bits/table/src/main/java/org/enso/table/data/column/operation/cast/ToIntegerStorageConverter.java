package org.enso.table.data.column.operation.cast;

import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.LongBuilder;
import org.enso.table.data.column.builder.NumericBuilder;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.Bits;
import org.enso.table.data.column.storage.type.IntegerType;
import org.graalvm.polyglot.Context;

public class ToIntegerStorageConverter implements StorageConverter<Long> {
  private final IntegerType targetType;

  public ToIntegerStorageConverter(IntegerType targetType) {
    this.targetType = targetType;
  }

  public Storage<Long> cast(Storage<?> storage, CastProblemBuilder problemBuilder) {
    if (storage instanceof LongStorage longStorage) {
      return longStorage;
    } else if (storage instanceof DoubleStorage doubleStorage) {
      return convertDoubleStorage(doubleStorage, problemBuilder);
    } else if (storage instanceof BoolStorage boolStorage) {
      return convertBoolStorage(boolStorage, problemBuilder);
    } else if (storage.getType() instanceof AnyObjectType) {
      return castFromMixed(storage, problemBuilder);
    } else {
      throw new IllegalStateException("No known strategy for casting storage " + storage + " to Integer.");
    }
  }

  public Storage<Long> castFromMixed(Storage<?> mixedStorage, CastProblemBuilder problemBuilder) {
    Context context = Context.getCurrent();
    LongBuilder builder = NumericBuilder.createLongBuilder(mixedStorage.size(), targetType);
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
        // TODO is this ok or do we need it to be more efficient?
        if (targetType.fits(x)) {
          long converted = (long) x;
          builder.appendLong(converted);
        } else {
          // TODO maybe more precise Out_Of_Range conversion here?
          problemBuilder.reportConversionFailure(o);
          builder.appendNulls(1);
        }
      } else {
        problemBuilder.reportConversionFailure(o);
        builder.appendNulls(1);
      }

      context.safepoint();
    }

    problemBuilder.aggregateOtherProblems(builder.getProblems());
    return builder.seal();
  }

  private Storage<Long> convertBoolStorage(BoolStorage boolStorage, CastProblemBuilder problemBuilder) {
    Context context = Context.getCurrent();
    int n = boolStorage.size();
    LongBuilder builder = NumericBuilder.createLongBuilder(n, targetType);
    for (int i = 0; i < n; i++) {
      if (boolStorage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        boolean value = boolStorage.getItem(i);
        builder.appendLong(booleanAsLong(value));
      }

      context.safepoint();
    }

    problemBuilder.aggregateOtherProblems(builder.getProblems());
    return builder.seal();
  }

  private Storage<Long> convertDoubleStorage(DoubleStorage doubleStorage, CastProblemBuilder problemBuilder) {
    Context context = Context.getCurrent();
    int n = doubleStorage.size();
    LongBuilder builder = NumericBuilder.createLongBuilder(n, targetType);
    for (int i = 0; i < n; i++) {
      if (doubleStorage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        double value = doubleStorage.getItem(i);
        if (targetType.fits(value)) {
          long converted = (long) value;
          builder.appendLong(converted);
        } else {
          builder.appendNulls(1);
          problemBuilder.reportConversionFailure(value);
        }
      }

      context.safepoint();
    }

    problemBuilder.aggregateOtherProblems(builder.getProblems());
    return builder.seal();
  }

  public static long booleanAsLong(boolean value) {
    return value ? 1L : 0L;
  }
}
