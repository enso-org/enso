package org.enso.table.data.column.operation.cast;

import org.enso.table.data.column.builder.BigIntegerBuilder;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;

import java.math.BigDecimal;
import java.math.BigInteger;

public class ToBigIntegerConverter implements StorageConverter<BigInteger> {
  @Override
  public Storage<BigInteger> cast(Storage<?> storage, CastProblemBuilder problemBuilder) {
    if (storage instanceof BigIntegerStorage bigIntegerStorage) {
      return bigIntegerStorage;
    } else if (storage instanceof AbstractLongStorage longStorage) {
      return convertLongStorage(longStorage, problemBuilder);
    } else if (storage instanceof DoubleStorage doubleStorage) {
      return convertDoubleStorage(doubleStorage, problemBuilder);
    } else if (storage instanceof BoolStorage boolStorage) {
      return convertBoolStorage(boolStorage, problemBuilder);
    } else if (storage.getType() instanceof AnyObjectType) {
      return castFromMixed(storage, problemBuilder);
    }else {
      throw new IllegalStateException("No known strategy for casting storage " + storage + " to BigInteger.");
    }
  }

  private Storage<BigInteger> convertDoubleStorage(DoubleStorage doubleStorage, CastProblemBuilder problemBuilder) {
    int n = doubleStorage.size();
    BigIntegerBuilder builder = new BigIntegerBuilder(n);
    for (int i = 0; i < n; i++) {
      if (doubleStorage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        double x = doubleStorage.getItemAsDouble(i);
        BigInteger bigInteger = BigDecimal.valueOf(x).toBigInteger();
        builder.appendRawNoGrow(bigInteger);
      }
    }
    return builder.seal();
  }

  private Storage<BigInteger> convertLongStorage(AbstractLongStorage longStorage, CastProblemBuilder problemBuilder) {
    int n = longStorage.size();
    BigIntegerBuilder builder = new BigIntegerBuilder(n);
    for (int i = 0; i < n; i++) {
      if (longStorage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        long x = longStorage.getItem(i);
        BigInteger bigInteger = BigInteger.valueOf(x);
        builder.appendRawNoGrow(bigInteger);
      }
    }
    return builder.seal();
  }

  private Storage<BigInteger> convertBoolStorage(BoolStorage boolStorage, CastProblemBuilder problemBuilder) {
    int n = boolStorage.size();
    BigIntegerBuilder builder = new BigIntegerBuilder(n);
    for (int i = 0; i < n; i++) {
      if (boolStorage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        boolean x = boolStorage.getItem(i);
        BigInteger bigInteger = booleanAsBigInteger(x);
        builder.appendRawNoGrow(bigInteger);
      }
    }
    return builder.seal();
  }

  private Storage<BigInteger> castFromMixed(Storage<?> storage, CastProblemBuilder problemBuilder) {
    int n = storage.size();
    BigIntegerBuilder builder = new BigIntegerBuilder(n);
    for (int i = 0; i < n; i++) {
      Object o = storage.getItemBoxed(i);
      switch (o) {
        case null -> builder.appendNulls(1);
        case Boolean b -> builder.appendRawNoGrow(booleanAsBigInteger(b));
        case Long l -> builder.appendRawNoGrow(BigInteger.valueOf(l));
        case Double d -> builder.appendRawNoGrow(BigDecimal.valueOf(d).toBigInteger());
        case BigInteger bigInteger -> builder.appendRawNoGrow(bigInteger);
        default -> {
          problemBuilder.reportConversionFailure(o);
          builder.appendNulls(1);
        }
      }
    }
    return builder.seal();
  }

  public static BigInteger booleanAsBigInteger(boolean value) {
    return value ? BigInteger.ONE : BigInteger.ZERO;
  }
}
