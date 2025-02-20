package org.enso.table.data.column.operation.cast;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;

public class ToIntegerStorageConverter implements StorageConverter<Long> {
  private final IntegerType targetType;

  public ToIntegerStorageConverter(IntegerType targetType) {
    this.targetType = targetType;
  }

  @Override
  public boolean canApply(StorageType sourceType) {
    return sourceType instanceof IntegerType
        || sourceType instanceof FloatType
        || sourceType instanceof BigDecimalType
        || sourceType instanceof BigIntegerType
        || sourceType instanceof BooleanType
        || sourceType instanceof NullType
        || sourceType instanceof AnyObjectType;
  }

  @Override
  public ColumnStorage<Long> cast(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    if (storage instanceof AbstractLongStorage longStorage) {
      if (longStorage.getType().equals(targetType)) {
        return longStorage;
      } else {
        return convertLongStorage(longStorage, problemAggregator);
      }
    }

    if (storage instanceof ColumnDoubleStorage doubleStorage) {
      return convertDoubleStorage(doubleStorage, problemAggregator);
    } else if (storage instanceof ColumnBooleanStorage boolStorage) {
      return convertBoolStorage(boolStorage, problemAggregator);
    } else if (storage instanceof BigIntegerStorage bigIntegerStorage) {
      return convertBigIntegerStorage(bigIntegerStorage, problemAggregator);
    } else if (storage instanceof BigDecimalStorage bigDecimalStorage) {
      return convertBigDecimalStorage(bigDecimalStorage, problemAggregator);
    } else if (canApply(storage.getType())) {
      return castFromObject(storage, problemAggregator);
    } else {
      throw new IllegalStateException(
          "No known strategy for casting storage " + storage + " to Integer.");
    }
  }

  private ColumnStorage<Long> castFromObject(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    return StorageIterators.buildOverStorage(
        storage,
        Builder.getForLong(targetType, storage.getSize(), problemAggregator),
        (builder, index, value) -> {
          if (value instanceof Boolean b) {
            builder.appendLong(booleanAsLong(b));
          } else if (NumericConverter.isCoercibleToLong(value)) {
            long x = NumericConverter.coerceToLong(value);
            builder.appendLong(x);
          } else if (NumericConverter.isFloatLike(value)) {
            double x = NumericConverter.coerceToDouble(value);
            if (targetType.fits(x)) {
              long converted = (long) x;
              builder.appendLong(converted);
            } else {
              problemAggregator.reportNumberOutOfRange(x);
              builder.appendNulls(1);
            }
          } else if (value instanceof BigInteger bigInteger) {
            if (targetType.fits(bigInteger)) {
              builder.appendLong(bigInteger.longValue());
            } else {
              problemAggregator.reportNumberOutOfRange(bigInteger);
              builder.appendNulls(1);
            }
          } else if (value instanceof BigDecimal bigDecimal) {
            BigInteger bigInteger = bigDecimal.toBigInteger();
            if (targetType.fits(bigInteger)) {
              builder.appendLong(bigInteger.longValue());
            } else {
              problemAggregator.reportNumberOutOfRange(bigDecimal);
              builder.appendNulls(1);
            }
          } else {
            problemAggregator.reportConversionFailure(value);
            builder.appendNulls(1);
          }
        });
  }

  private ColumnStorage<Long> convertBoolStorage(
      ColumnBooleanStorage boolStorage, CastProblemAggregator problemAggregator) {
    return StorageIterators.buildOverBooleanStorage(
        boolStorage,
        Builder.getForLong(targetType, boolStorage.getSize(), problemAggregator),
        (builder, index, value, isNothing) -> builder.appendLong(booleanAsLong(value)));
  }

  private ColumnStorage<Long> convertDoubleStorage(
      ColumnDoubleStorage doubleStorage, CastProblemAggregator problemAggregator) {
    return StorageIterators.buildOverDoubleStorage(
        doubleStorage,
        Builder.getForLong(targetType, doubleStorage.getSize(), problemAggregator),
        (builder, index, value, isNothing) -> {
          if (targetType.fits(value)) {
            long converted = (long) value;
            builder.appendLong(converted);
          } else {
            builder.appendNulls(1);
            problemAggregator.reportConversionFailure(value);
          }
        });
  }

  private ColumnStorage<Long> convertLongStorage(
      AbstractLongStorage longStorage, CastProblemAggregator problemAggregator) {
    boolean isWidening = targetType.fits(longStorage.getType());
    if (isWidening) {
      // If the target type is larger than the source type, we can just widen the storage without
      // doing any checks.
      return longStorage.widen(targetType);
    }

    return StorageIterators.buildOverLongStorage(
        longStorage,
        Builder.getForLong(targetType, longStorage.getSize(), problemAggregator),
        (builder, index, value, isNothing) -> builder.appendLong(value));
  }

  private ColumnStorage<Long> convertBigIntegerStorage(
      ColumnStorage<BigInteger> storage, CastProblemAggregator problemAggregator) {
    return StorageIterators.buildOverStorage(
        storage,
        Builder.getForLong(targetType, storage.getSize(), problemAggregator),
        (builder, index, value) -> {
          if (targetType.fits(value)) {
            builder.appendLong(value.longValue());
          } else {
            builder.appendNulls(1);
            problemAggregator.reportNumberOutOfRange(value);
          }
        });
  }

  private ColumnStorage<Long> convertBigDecimalStorage(
      ColumnStorage<BigDecimal> storage, CastProblemAggregator problemAggregator) {
    return StorageIterators.buildOverStorage(
        storage,
        Builder.getForLong(targetType, storage.getSize(), problemAggregator),
        (builder, index, value) -> {
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
