package org.enso.table.data.column.operation.cast;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;

public class ToBigDecimalConverter implements StorageConverter<BigDecimal> {
  @Override
  public Storage<BigDecimal> cast(Storage<?> storage, CastProblemAggregator problemAggregator) {
    if (storage instanceof BigDecimalStorage bigDecimalStorage) {
      return bigDecimalStorage;
    } else if (storage instanceof AbstractLongStorage longStorage) {
      return convertLongStorage(longStorage, problemAggregator);
    } else if (storage instanceof DoubleStorage doubleStorage) {
      return convertDoubleStorage(doubleStorage, problemAggregator);
    } else if (storage instanceof BigIntegerStorage bigIntegerStorage) {
      return convertBigIntegerStorage(bigIntegerStorage, problemAggregator);
    } else if (storage instanceof BoolStorage boolStorage) {
      return convertBoolStorage(boolStorage, problemAggregator);
    } else if (storage.getType() instanceof AnyObjectType) {
      return castFromMixed(storage, problemAggregator);
    } else {
      throw new IllegalStateException(
          "No known strategy for casting storage " + storage + " to BigDecimal.");
    }
  }

  private Storage<BigDecimal> convertDoubleStorage(
      DoubleStorage doubleStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForBigDecimal(doubleStorage.size()),
        doubleStorage,
        (i) -> {
          double x = doubleStorage.getItemAsDouble(i);
          return BigDecimal.valueOf(x);
        });
  }

  private Storage<BigDecimal> convertLongStorage(
      AbstractLongStorage longStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForBigDecimal(longStorage.size()),
        longStorage,
        (i) -> {
          long x = longStorage.getItem(i);
          return BigDecimal.valueOf(x);
        });
  }

  private Storage<BigDecimal> convertBoolStorage(
      BoolStorage boolStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForBigDecimal(boolStorage.size()),
        boolStorage,
        (i) -> {
          boolean x = boolStorage.getItem(i);
          return booleanAsBigDecimal(x);
        });
  }

  private Storage<BigDecimal> convertBigIntegerStorage(
      BigIntegerStorage bigIntegerStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForBigDecimal(bigIntegerStorage.size()),
        bigIntegerStorage,
        (i) -> {
          BigInteger x = bigIntegerStorage.getItem(i);
          return new BigDecimal(x);
        });
  }

  private Storage<BigDecimal> castFromMixed(
      Storage<?> storage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForBigDecimal(storage.size()),
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
