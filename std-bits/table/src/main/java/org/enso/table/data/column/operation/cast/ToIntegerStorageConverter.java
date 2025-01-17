package org.enso.table.data.column.operation.cast;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.function.ObjIntConsumer;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForLong;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.graalvm.polyglot.Context;

public class ToIntegerStorageConverter implements StorageConverter<Long> {
  private final IntegerType targetType;

  public ToIntegerStorageConverter(IntegerType targetType) {
    this.targetType = targetType;
  }

  @Override
  public Storage<Long> cast(Storage<?> storage, CastProblemAggregator problemAggregator) {
    if (storage instanceof AbstractLongStorage longStorage) {
      if (longStorage.getType().equals(targetType)) {
        return longStorage;
      } else {
        return convertLongStorage(longStorage, problemAggregator);
      }
    } else if (storage instanceof DoubleStorage doubleStorage) {
      return convertDoubleStorage(doubleStorage, problemAggregator);
    } else if (storage instanceof BoolStorage boolStorage) {
      return convertBoolStorage(boolStorage, problemAggregator);
    } else if (storage instanceof BigIntegerStorage bigIntegerStorage) {
      return convertBigIntegerStorage(bigIntegerStorage, problemAggregator);
    } else if (storage instanceof BigDecimalStorage bigDecimalStorage) {
      return convertBigDecimalStorage(bigDecimalStorage, problemAggregator);
    } else if (storage.getType() instanceof AnyObjectType) {
      return castFromMixed(storage, problemAggregator);
    } else {
      throw new IllegalStateException(
          "No known strategy for casting storage " + storage + " to Integer.");
    }
  }

  /** Specialised innerLoop so that we can avoid boxing. */
  static Storage<Long> innerLoop(
      BuilderForLong builder, ColumnStorage storage, ObjIntConsumer<BuilderForLong> converter) {
    Context context = Context.getCurrent();

    long n = storage.getSize();
    if (n > Integer.MAX_VALUE) {
      throw new IllegalArgumentException(
          "Cannot currently operate on columns larger than " + Integer.MAX_VALUE + ".");
    }

    for (int i = 0; i < n; i++) {
      if (storage.isNothing(i)) {
        builder.appendNulls(1);
      } else {
        converter.accept(builder, i);
      }

      context.safepoint();
    }

    return builder.seal();
  }

  private Storage<Long> castFromMixed(
      Storage<?> mixedStorage, CastProblemAggregator problemAggregator) {
    return innerLoop(
        Builder.getForLong(targetType, mixedStorage.size(), problemAggregator),
        mixedStorage,
        (builder, i) -> {
          Object o = mixedStorage.getItemBoxed(i);
          if (o instanceof Boolean b) {
            builder.appendLong(booleanAsLong(b));
          } else if (NumericConverter.isCoercibleToLong(o)) {
            long x = NumericConverter.coerceToLong(o);
            builder.appendLong(x);
          } else if (NumericConverter.isFloatLike(o)) {
            double x = NumericConverter.coerceToDouble(o);
            if (targetType.fits(x)) {
              long converted = (long) x;
              builder.appendLong(converted);
            } else {
              problemAggregator.reportNumberOutOfRange(x);
              builder.appendNulls(1);
            }
          } else if (o instanceof BigInteger bigInteger) {
            if (targetType.fits(bigInteger)) {
              builder.appendLong(bigInteger.longValue());
            } else {
              problemAggregator.reportNumberOutOfRange(bigInteger);
              builder.appendNulls(1);
            }
          } else if (o instanceof BigDecimal bigDecimal) {
            BigInteger bigInteger = bigDecimal.toBigInteger();
            if (targetType.fits(bigInteger)) {
              builder.appendLong(bigInteger.longValue());
            } else {
              problemAggregator.reportNumberOutOfRange(bigDecimal);
              builder.appendNulls(1);
            }
          } else {
            problemAggregator.reportConversionFailure(o);
            builder.appendNulls(1);
          }
        });
  }

  private Storage<Long> convertBoolStorage(
      BoolStorage boolStorage, CastProblemAggregator problemAggregator) {
    return innerLoop(
        Builder.getForLong(targetType, boolStorage.size(), problemAggregator),
        boolStorage,
        (builder, i) -> {
          boolean value = boolStorage.getItem(i);
          builder.appendLong(booleanAsLong(value));
        });
  }

  private Storage<Long> convertDoubleStorage(
      DoubleStorage doubleStorage, CastProblemAggregator problemAggregator) {
    return innerLoop(
        Builder.getForLong(targetType, doubleStorage.size(), problemAggregator),
        doubleStorage,
        (builder, i) -> {
          double value = doubleStorage.getItemAsDouble(i);
          if (targetType.fits(value)) {
            long converted = (long) value;
            builder.appendLong(converted);
          } else {
            builder.appendNulls(1);
            problemAggregator.reportConversionFailure(value);
          }
        });
  }

  private Storage<Long> convertLongStorage(
      AbstractLongStorage longStorage, CastProblemAggregator problemAggregator) {
    boolean isWidening = targetType.fits(longStorage.getType());
    if (isWidening) {
      // If the target type is larger than the source type, we can just widen the storage without
      // doing any checks.
      return longStorage.widen(targetType);
    }

    return innerLoop(
        Builder.getForLong(targetType, longStorage.size(), problemAggregator),
        longStorage,
        (builder, i) -> {
          long value = longStorage.getItem(i);
          builder.appendLong(value);
        });
  }

  private Storage<Long> convertBigIntegerStorage(
      Storage<BigInteger> storage, CastProblemAggregator problemAggregator) {
    return innerLoop(
        Builder.getForLong(targetType, storage.size(), problemAggregator),
        storage,
        (builder, i) -> {
          BigInteger value = storage.getItemBoxed(i);
          if (targetType.fits(value)) {
            builder.appendLong(value.longValue());
          } else {
            builder.appendNulls(1);
            problemAggregator.reportNumberOutOfRange(value);
          }
        });
  }

  private Storage<Long> convertBigDecimalStorage(
      Storage<BigDecimal> storage, CastProblemAggregator problemAggregator) {
    return innerLoop(
        Builder.getForLong(targetType, storage.size(), problemAggregator),
        storage,
        (builder, i) -> {
          BigDecimal value = storage.getItemBoxed(i);
          BigInteger bigInteger = value.toBigInteger();
          if (targetType.fits(bigInteger)) {
            builder.appendLong(bigInteger.longValue());
          } else {
            builder.appendNulls(1);
            problemAggregator.reportNumberOutOfRange(value);
          }
        });
  }

  private static long booleanAsLong(boolean value) {
    return value ? 1L : 0L;
  }
}
