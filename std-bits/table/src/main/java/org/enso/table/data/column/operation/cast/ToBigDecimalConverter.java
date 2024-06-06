package org.enso.table.data.column.operation.cast;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.builder.BigDecimalBuilder;
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
    int n = doubleStorage.size();
    BigDecimalBuilder builder = new BigDecimalBuilder(n, problemAggregator);
    for (int i = 0; i < n; i++) {
      if (doubleStorage.isNothing(i)) {
        builder.appendNulls(1);
      } else {
        double x = doubleStorage.getItemAsDouble(i);
        BigDecimal bigDecimal = BigDecimal.valueOf(x);
        builder.appendRawNoGrow(bigDecimal);
      }
    }
    return builder.seal();
  }

  private Storage<BigDecimal> convertLongStorage(
      AbstractLongStorage longStorage, CastProblemAggregator problemAggregator) {
    int n = longStorage.size();
    BigDecimalBuilder builder = new BigDecimalBuilder(n, problemAggregator);
    for (int i = 0; i < n; i++) {
      if (longStorage.isNothing(i)) {
        builder.appendNulls(1);
      } else {
        long x = longStorage.getItem(i);
        BigDecimal bigDecimal = BigDecimal.valueOf(x);
        builder.appendRawNoGrow(bigDecimal);
      }
    }
    return builder.seal();
  }

  private Storage<BigDecimal> convertBoolStorage(
      BoolStorage boolStorage, CastProblemAggregator problemAggregator) {
    int n = boolStorage.size();
    BigDecimalBuilder builder = new BigDecimalBuilder(n, problemAggregator);
    for (int i = 0; i < n; i++) {
      if (boolStorage.isNothing(i)) {
        builder.appendNulls(1);
      } else {
        boolean x = boolStorage.getItem(i);
        BigDecimal bigDecimal = booleanAsBigDecimal(x);
        builder.appendRawNoGrow(bigDecimal);
      }
    }
    return builder.seal();
  }

  private Storage<BigDecimal> convertBigIntegerStorage(
      BigIntegerStorage bigIntegerStorage, CastProblemAggregator problemAggregator) {
    int n = bigIntegerStorage.size();
    BigDecimalBuilder builder = new BigDecimalBuilder(n, problemAggregator);
    for (int i = 0; i < n; i++) {
      if (bigIntegerStorage.isNothing(i)) {
        builder.appendNulls(1);
      } else {
        BigInteger x = bigIntegerStorage.getItem(i);
        BigDecimal bigDecimal = new BigDecimal(x);
        builder.appendRawNoGrow(bigDecimal);
      }
    }
    return builder.seal();
  }

  private Storage<BigDecimal> castFromMixed(
      Storage<?> storage, CastProblemAggregator problemAggregator) {
    int n = storage.size();
    BigDecimalBuilder builder = new BigDecimalBuilder(n, problemAggregator);
    for (int i = 0; i < n; i++) {
      Object o = storage.getItemBoxed(i);
      switch (o) {
        case null -> builder.appendNulls(1);
        case Boolean b -> builder.appendRawNoGrow(booleanAsBigDecimal(b));
        case Long l -> builder.appendRawNoGrow(BigDecimal.valueOf(l));
        case Double d -> builder.appendRawNoGrow(BigDecimal.valueOf(d));
        case BigInteger bigInteger -> builder.appendRawNoGrow(new BigDecimal(bigInteger));
        case BigDecimal bigDecimal -> builder.appendRawNoGrow(bigDecimal);
        default -> {
          problemAggregator.reportConversionFailure(o);
          builder.appendNulls(1);
        }
      }
    }
    return builder.seal();
  }

  public static BigDecimal booleanAsBigDecimal(boolean value) {
    return value ? BigDecimal.ONE : BigDecimal.ZERO;
  }
}
