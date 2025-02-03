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

public class ToBigIntegerConverter implements StorageConverter<BigInteger> {
  @Override
  public Storage<BigInteger> cast(Storage<?> storage, CastProblemAggregator problemAggregator) {
    if (storage instanceof BigIntegerStorage bigIntegerStorage) {
      return bigIntegerStorage;
    } else if (storage instanceof ColumnLongStorage longStorage) {
      return convertLongStorage(longStorage, problemAggregator);
    } else if (storage instanceof ColumnDoubleStorage doubleStorage) {
      return convertDoubleStorage(doubleStorage, problemAggregator);
    } else if (storage instanceof ColumnBooleanStorage boolStorage) {
      return convertBoolStorage(boolStorage, problemAggregator);
    } else if (storage instanceof BigDecimalStorage bigDecimalStorage) {
      return convertBigDecimalStorage(bigDecimalStorage, problemAggregator);
    } else if (storage.getType() instanceof AnyObjectType
        || storage.getType() instanceof NullType) {
      return castFromMixed(storage, problemAggregator);
    } else {
      throw new IllegalStateException(
          "No known strategy for casting storage " + storage + " to BigInteger.");
    }
  }

  private Storage<BigInteger> convertDoubleStorage(
      ColumnDoubleStorage doubleStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForBigInteger(doubleStorage.getSize(), problemAggregator),
        doubleStorage,
        (i) -> {
          double x = doubleStorage.getItemAsDouble(i);
          return BigDecimal.valueOf(x).toBigInteger();
        });
  }

  private Storage<BigInteger> convertLongStorage(
      ColumnLongStorage longStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForBigInteger(longStorage.getSize(), problemAggregator),
        longStorage,
        (i) -> {
          long x = longStorage.getItemAsLong(i);
          return BigInteger.valueOf(x);
        });
  }

  private Storage<BigInteger> convertBoolStorage(
      ColumnBooleanStorage boolStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForBigInteger(boolStorage.getSize(), problemAggregator),
        boolStorage,
        (i) -> {
          boolean x = boolStorage.getItemAsBoolean((int) i);
          return booleanAsBigInteger(x);
        });
  }

  private Storage<BigInteger> convertBigDecimalStorage(
      Storage<BigDecimal> bigDecimalStorage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForBigInteger(bigDecimalStorage.getSize(), problemAggregator),
        bigDecimalStorage,
        (i) -> {
          BigDecimal x = bigDecimalStorage.getItemBoxed(i);
          return x.toBigInteger();
        });
  }

  private Storage<BigInteger> castFromMixed(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    return StorageConverter.innerLoop(
        Builder.getForBigInteger(storage.getSize(), problemAggregator),
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
