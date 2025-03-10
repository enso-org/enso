package org.enso.table.data.column.operation.cast;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;

public class ToBigDecimalConverter implements StorageConverter<BigDecimal> {
  @Override
  public boolean canApply(StorageType sourceType) {
    return sourceType instanceof IntegerType
        || sourceType instanceof BigDecimalType
        || sourceType instanceof FloatType
        || sourceType instanceof BooleanType
        || sourceType instanceof AnyObjectType
        || sourceType instanceof NullType;
  }

  @Override
  public ColumnStorage<BigDecimal> cast(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    if (storage instanceof BigDecimalStorage bigDecimalStorage) {
      return bigDecimalStorage;
    } else if (storage instanceof ColumnLongStorage longStorage) {
      return convertLongStorage(longStorage);
    } else if (storage instanceof ColumnDoubleStorage doubleStorage) {
      return convertDoubleStorage(doubleStorage);
    } else if (storage instanceof BigIntegerStorage bigIntegerStorage) {
      return convertBigIntegerStorage(bigIntegerStorage);
    } else if (storage instanceof ColumnBooleanStorage boolStorage) {
      return convertBoolStorage(boolStorage);
    } else if (canApply(storage.getType())) {
      return castFromObject(storage, problemAggregator);
    } else {
      throw new IllegalStateException(
          "No known strategy for casting storage " + storage + " to BigDecimal.");
    }
  }

  private ColumnStorage<BigDecimal> convertDoubleStorage(ColumnDoubleStorage doubleStorage) {
    return StorageIterators.mapOverDoubleStorage(
        doubleStorage,
        Builder.getForBigDecimal(doubleStorage.getSize()),
        (index, value, isNothing) -> BigDecimal.valueOf(value));
  }

  private ColumnStorage<BigDecimal> convertLongStorage(ColumnLongStorage longStorage) {
    return StorageIterators.mapOverLongStorage(
        longStorage,
        Builder.getForBigDecimal(longStorage.getSize()),
        (index, value, isNothing) -> BigDecimal.valueOf(value));
  }

  private ColumnStorage<BigDecimal> convertBoolStorage(ColumnBooleanStorage boolStorage) {
    return StorageIterators.mapOverBooleanStorage(
        boolStorage,
        Builder.getForBigDecimal(boolStorage.getSize()),
        (index, value, isNothing) -> booleanAsBigDecimal(value));
  }

  private ColumnStorage<BigDecimal> convertBigIntegerStorage(
      ColumnStorage<BigInteger> bigIntegerStorage) {
    return StorageIterators.mapOverStorage(
        bigIntegerStorage,
        Builder.getForBigDecimal(bigIntegerStorage.getSize()),
        (index, value) -> new BigDecimal(value));
  }

  private ColumnStorage<BigDecimal> castFromObject(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    return StorageIterators.mapOverStorage(
        storage,
        Builder.getForBigDecimal(storage.getSize()),
        (index, value) ->
            switch (value) {
              case Boolean b -> booleanAsBigDecimal(b);
              case Long l -> BigDecimal.valueOf(l);
              case Double d -> BigDecimal.valueOf(d);
              case BigInteger bigInteger -> new BigDecimal(bigInteger);
              case BigDecimal bigDecimal -> bigDecimal;
              default -> {
                problemAggregator.reportConversionFailure(value);
                yield null;
              }
            });
  }

  private static BigDecimal booleanAsBigDecimal(boolean value) {
    return value ? BigDecimal.ONE : BigDecimal.ZERO;
  }
}
