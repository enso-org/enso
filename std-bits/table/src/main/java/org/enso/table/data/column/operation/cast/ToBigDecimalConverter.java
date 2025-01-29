package org.enso.table.data.column.operation.cast;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.NullType;

public class ToBigDecimalConverter implements StorageConverter<BigDecimal> {
  @Override
  public Storage<BigDecimal> cast(Storage<?> storage, CastProblemAggregator problemAggregator) {
    if (storage instanceof BigDecimalStorage bigDecimalStorage) {
      return bigDecimalStorage;
    } else if (storage instanceof ColumnLongStorage longStorage) {
      return convertLongStorage(longStorage, problemAggregator);
    } else if (storage instanceof ColumnDoubleStorage doubleStorage) {
      return convertDoubleStorage(doubleStorage, problemAggregator);
    } else if (storage instanceof BigIntegerStorage bigIntegerStorage) {
      return convertBigIntegerStorage(bigIntegerStorage, problemAggregator);
    } else if (storage instanceof ColumnBooleanStorage boolStorage) {
      return convertBoolStorage(boolStorage, problemAggregator);
    } else if (storage.getType() instanceof AnyObjectType
        || storage.getType() instanceof NullType) {
      return castFromMixed(storage, problemAggregator);
    } else {
      throw new IllegalStateException(
          "No known strategy for casting storage " + storage + " to BigDecimal.");
    }
  }

  private Storage<BigDecimal> convertDoubleStorage(
      ColumnDoubleStorage doubleStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForBigDecimal(doubleStorage.getSize()),
        doubleStorage,
        (i) -> {
          double x = doubleStorage.getItemAsDouble(i);
          return BigDecimal.valueOf(x);
        });
  }

  private Storage<BigDecimal> convertLongStorage(
      ColumnLongStorage longStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForBigDecimal(longStorage.getSize()),
        longStorage,
        (i) -> {
          long x = longStorage.getItemAsLong(i);
          return BigDecimal.valueOf(x);
        });
  }

  private Storage<BigDecimal> convertBoolStorage(
      ColumnBooleanStorage boolStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForBigDecimal(boolStorage.getSize()),
        boolStorage,
        (i) -> {
          boolean x = boolStorage.getItemAsBoolean(i);
          return booleanAsBigDecimal(x);
        });
  }

  private Storage<BigDecimal> convertBigIntegerStorage(
      Storage<BigInteger> bigIntegerStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForBigDecimal(bigIntegerStorage.getSize()),
        bigIntegerStorage,
        (i) -> {
          BigInteger x = bigIntegerStorage.getItemBoxed(i);
          return new BigDecimal(x);
        });
  }

  private Storage<BigDecimal> castFromMixed(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForBigDecimal(storage.getSize()),
        storage,
        (i) -> {
          Object o = storage.getItemBoxed(i);
          return switch (o) {
            case Boolean b -> booleanAsBigDecimal(b);
            case Long l -> BigDecimal.valueOf(l);
            case Double d -> BigDecimal.valueOf(d);
            case BigInteger bigInteger -> new BigDecimal(bigInteger);
            case BigDecimal bigDecimal -> bigDecimal;
            default -> {
              problemAggregator.reportConversionFailure(o);
              yield null;
            }
          };
        });
  }

  private static BigDecimal booleanAsBigDecimal(boolean value) {
    return value ? BigDecimal.ONE : BigDecimal.ZERO;
  }
}
