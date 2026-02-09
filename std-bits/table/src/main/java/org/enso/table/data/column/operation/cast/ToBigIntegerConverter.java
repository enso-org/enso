package org.enso.table.data.column.operation.cast;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;

public class ToBigIntegerConverter implements StorageConverter<BigInteger> {
  @Override
  public boolean canApply(StorageType<?> sourceType) {
    return sourceType instanceof IntegerType
        || sourceType instanceof FloatType
        || sourceType instanceof BooleanType
        || sourceType instanceof BigDecimalType
        || sourceType instanceof AnyObjectType
        || sourceType instanceof NullType;
  }

  @Override
  public ColumnStorage<BigInteger> cast(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    var storageType = StorageType.ofStorage(storage);
    return switch (storageType) {
      case IntegerType it -> convertLongStorage(it.asTypedStorage(storage), problemAggregator);
      case FloatType ft -> convertDoubleStorage(ft.asTypedStorage(storage), problemAggregator);
      case BooleanType bt -> convertBoolStorage(bt.asTypedStorage(storage), problemAggregator);
      case BigDecimalType bdt ->
          convertBigDecimalStorage(bdt.asTypedStorage(storage), problemAggregator);
      default -> {
        if (!canApply(storageType)) {
          throw new IllegalStateException(
              "Cannot convert storage of type " + storageType + " to BigInteger.");
        }
        yield castFromObject(storage, problemAggregator);
      }
    };
  }

  private ColumnStorage<BigInteger> convertDoubleStorage(
      ColumnDoubleStorage doubleStorage, CastProblemAggregator problemAggregator) {
    return StorageIterators.mapOverDoubleStorage(
        doubleStorage,
        Builder.getForBigInteger(doubleStorage.getSize(), problemAggregator),
        (index, value, isNothing) -> BigDecimal.valueOf(value).toBigInteger());
  }

  private ColumnStorage<BigInteger> convertLongStorage(
      ColumnLongStorage longStorage, CastProblemAggregator problemAggregator) {
    return StorageIterators.mapOverLongStorage(
        longStorage,
        Builder.getForBigInteger(longStorage.getSize(), problemAggregator),
        (index, value, isNothing) -> BigInteger.valueOf(value));
  }

  private ColumnStorage<BigInteger> convertBoolStorage(
      ColumnBooleanStorage boolStorage, CastProblemAggregator problemAggregator) {
    return StorageIterators.mapOverBooleanStorage(
        boolStorage,
        Builder.getForBigInteger(boolStorage.getSize(), problemAggregator),
        (index, value, isNothing) -> booleanAsBigInteger(value));
  }

  private ColumnStorage<BigInteger> convertBigDecimalStorage(
      ColumnStorage<BigDecimal> bigDecimalStorage, CastProblemAggregator problemAggregator) {
    return StorageIterators.mapOverStorage(
        bigDecimalStorage,
        Builder.getForBigInteger(bigDecimalStorage.getSize(), problemAggregator),
        (index, value) -> value.toBigInteger());
  }

  private ColumnStorage<BigInteger> castFromObject(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    return StorageIterators.mapOverStorage(
        storage,
        Builder.getForBigInteger(storage.getSize(), problemAggregator),
        (index, value) ->
            switch (value) {
              case Boolean b -> booleanAsBigInteger(b);
              case Long l -> BigInteger.valueOf(l);
              case Double d -> BigDecimal.valueOf(d).toBigInteger();
              case BigInteger bigInteger -> bigInteger;
              case BigDecimal bigDecimal -> bigDecimal.toBigInteger();
              default -> {
                problemAggregator.reportConversionFailure(value);
                yield null;
              }
            });
  }

  private static BigInteger booleanAsBigInteger(boolean value) {
    return value ? BigInteger.ONE : BigInteger.ZERO;
  }
}
