package org.enso.table.data.column.operation.cast;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.Bits;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;

public class ToFloatStorageConverter implements StorageConverter<Double> {
  public ToFloatStorageConverter(FloatType targetType) {
    if (targetType.bits() != Bits.BITS_64) {
      throw new IllegalStateException(
          "Internal error: Only 64-bit floats are currently supported.");
    }
  }

  @Override
  public boolean canApply(StorageType storageType) {
    return storageType instanceof IntegerType
        || storageType instanceof BigDecimalType
        || storageType instanceof BigIntegerType
        || storageType instanceof BooleanType
        || storageType instanceof NullType
        || storageType instanceof AnyObjectType;
  }

  @Override
  public ColumnStorage<Double> cast(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    if (storage instanceof ColumnLongStorage longStorage) {
      return convertLongStorage(longStorage, problemAggregator);
    } else if (storage instanceof ColumnBooleanStorage boolStorage) {
      return convertBoolStorage(boolStorage, problemAggregator);
    } else if (storage instanceof BigIntegerStorage bigIntegerStorage) {
      return convertBigIntegerStorage(bigIntegerStorage, problemAggregator);
    } else if (storage instanceof BigDecimalStorage bigDecimalStorage) {
      return convertBigDecimalStorage(bigDecimalStorage, problemAggregator);
    } else if (storage.getType() instanceof AnyObjectType
        || storage.getType() instanceof NullType) {
      return castFromObject(storage, problemAggregator);
    } else {
      throw new IllegalStateException(
          "No known strategy for casting storage " + storage + " to Float.");
    }
  }

  private ColumnStorage<Double> castFromObject(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    return StorageIterators.buildOverStorage(
        storage,
        Builder.getForDouble(FloatType.FLOAT_64, storage.getSize(), problemAggregator),
        (builder, index, value) -> {
          if (NumericConverter.isCoercibleToLong(value)) {
            builder.appendLong(NumericConverter.coerceToLong(value));
          } else if (NumericConverter.isFloatLike(value)) {
            builder.appendDouble(NumericConverter.coerceToDouble(value));
          } else {
            switch (value) {
              case Boolean b -> builder.appendDouble(booleanAsDouble(b));
              case BigInteger bigInteger -> builder.append(bigInteger);
              case BigDecimal bigDecimal -> builder.append(bigDecimal);
              default -> {
                problemAggregator.reportConversionFailure(value);
                builder.appendNulls(1);
              }
            }
          }
        });
  }

  private ColumnStorage<Double> convertLongStorage(
      ColumnLongStorage longStorage, CastProblemAggregator problemAggregator) {
    return StorageIterators.buildOverLongStorage(
        longStorage,
        Builder.getForDouble(FloatType.FLOAT_64, longStorage.getSize(), problemAggregator),
        (builder, index, value, isNothing) -> builder.appendLong(value));
  }

  private ColumnStorage<Double> convertBoolStorage(
      ColumnBooleanStorage boolStorage, CastProblemAggregator problemAggregator) {
    return StorageIterators.buildOverBooleanStorage(
        boolStorage,
        Builder.getForDouble(FloatType.FLOAT_64, boolStorage.getSize(), problemAggregator),
        (builder, index, value, isNothing) -> builder.appendDouble(booleanAsDouble(value)));
  }

  private static double booleanAsDouble(boolean value) {
    return value ? 1.0 : 0.0;
  }

  private ColumnStorage<Double> convertBigIntegerStorage(
      ColumnStorage<BigInteger> storage, CastProblemAggregator problemAggregator) {
    return StorageIterators.buildOverStorage(
        storage,
        Builder.getForDouble(FloatType.FLOAT_64, storage.getSize(), problemAggregator),
        (builder, index, value) -> builder.append(value));
  }

  private ColumnStorage<Double> convertBigDecimalStorage(
      ColumnStorage<BigDecimal> storage, CastProblemAggregator problemAggregator) {
    return StorageIterators.buildOverStorage(
        storage,
        Builder.getForDouble(FloatType.FLOAT_64, storage.getSize(), problemAggregator),
        (builder, index, value) -> builder.append(value));
  }
}
