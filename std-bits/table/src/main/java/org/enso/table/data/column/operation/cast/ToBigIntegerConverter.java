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

public class ToBigIntegerConverter implements StorageConverter<BigInteger> {
  @Override
  public Storage<BigInteger> cast(Storage<?> storage, CastProblemAggregator problemAggregator) {
    if (storage instanceof BigIntegerStorage bigIntegerStorage) {
      return bigIntegerStorage;
    } else if (storage instanceof AbstractLongStorage longStorage) {
      return convertLongStorage(longStorage, problemAggregator);
    } else if (storage instanceof DoubleStorage doubleStorage) {
      return convertDoubleStorage(doubleStorage, problemAggregator);
    } else if (storage instanceof BoolStorage boolStorage) {
      return convertBoolStorage(boolStorage, problemAggregator);
    } else if (storage instanceof BigDecimalStorage bigDecimalStorage) {
      return convertBigDecimalStorage(bigDecimalStorage, problemAggregator);
    } else if (storage.getType() instanceof AnyObjectType) {
      return castFromMixed(storage, problemAggregator);
    } else {
      throw new IllegalStateException(
          "No known strategy for casting storage " + storage + " to BigInteger.");
    }
  }

  private Storage<BigInteger> convertDoubleStorage(
      DoubleStorage doubleStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForBigInteger(doubleStorage.size(), problemAggregator),
        doubleStorage,
        (i) -> {
          double x = doubleStorage.getItemAsDouble(i);
          return BigDecimal.valueOf(x).toBigInteger();
        });
  }

  private Storage<BigInteger> convertLongStorage(
      AbstractLongStorage longStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForBigInteger(longStorage.size(), problemAggregator),
        longStorage,
        (i) -> {
          long x = longStorage.getItem(i);
          return BigInteger.valueOf(x);
        });
  }

  private Storage<BigInteger> convertBoolStorage(
      BoolStorage boolStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForBigInteger(boolStorage.size(), problemAggregator),
        boolStorage,
        (i) -> {
          boolean x = boolStorage.getItem(i);
          return booleanAsBigInteger(x);
        });
  }

  private Storage<BigInteger> convertBigDecimalStorage(
      BigDecimalStorage bigDecimalStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForBigInteger(bigDecimalStorage.size(), problemAggregator),
        bigDecimalStorage,
        (i) -> {
          BigDecimal x = bigDecimalStorage.getItemBoxed(i);
          return x.toBigInteger();
        });
  }

  private Storage<BigInteger> castFromMixed(
      Storage<?> storage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForBigInteger(storage.size(), problemAggregator),
        storage,
        (i) -> {
          Object o = storage.getItemBoxed(i);
          return switch (o) {
            case Boolean b -> booleanAsBigInteger(b);
            case Long l -> BigInteger.valueOf(l);
            case Double d -> BigDecimal.valueOf(d).toBigInteger();
            case BigInteger bigInteger -> bigInteger;
            case BigDecimal bigDecimal -> bigDecimal.toBigInteger();
            default -> {
              problemAggregator.reportConversionFailure(o);
              yield null;
            }
          };
        });
  }

  private static BigInteger booleanAsBigInteger(boolean value) {
    return value ? BigInteger.ONE : BigInteger.ZERO;
  }
}
