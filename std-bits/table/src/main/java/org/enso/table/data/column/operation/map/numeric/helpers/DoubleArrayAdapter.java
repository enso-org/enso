package org.enso.table.data.column.operation.map.numeric.helpers;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.problems.BlackholeProblemAggregator;

public interface DoubleArrayAdapter {
  double getItemAsDouble(long i);

  boolean isNothing(long i);

  long size();

  default Storage<Double> intoStorage() {
    long n = size();
    var builder = Builder.getForDouble(FloatType.FLOAT_64, n, BlackholeProblemAggregator.INSTANCE);
    for (long i = 0; i < n; i++) {
      if (isNothing(i)) {
        builder.appendNulls(1);
      } else {
        builder.appendDouble(getItemAsDouble(i));
      }
    }
    return builder.seal();
  }

  static DoubleArrayAdapter fromBigIntegerStorage(Storage<BigInteger> storage) {
    return new BigIntegerStorageAsDouble(storage);
  }

  static DoubleArrayAdapter fromBigDecimalStorage(Storage<BigDecimal> storage) {
    return new BigDecimalStorageAsDouble(storage);
  }

  static DoubleArrayAdapter fromStorage(ColumnLongStorage storage) {
    return new LongStorageAsDouble(storage);
  }

  static DoubleArrayAdapter fromStorage(DoubleStorage storage) {
    return storage;
  }

  static DoubleArrayAdapter fromAnyStorage(Storage<?> storage) {
    return switch (storage) {
      case DoubleStorage s -> fromStorage(s);
      case ColumnLongStorage s -> fromStorage(s);
      case BigIntegerStorage s -> fromBigIntegerStorage(s);
      case BigDecimalStorage s -> fromBigDecimalStorage(s);
      default -> throw new IllegalStateException(
          "Unsupported storage: " + storage.getClass().getCanonicalName());
    };
  }

  class LongStorageAsDouble implements DoubleArrayAdapter {
    private final ColumnLongStorage storage;

    private LongStorageAsDouble(ColumnLongStorage storage) {
      this.storage = storage;
    }

    @Override
    public double getItemAsDouble(long i) {
      long x = storage.getItemAsLong(i);
      return (double) x;
    }

    @Override
    public boolean isNothing(long i) {
      return storage.isNothing(i);
    }

    @Override
    public long size() {
      return storage.getSize();
    }
  }

  class BigIntegerStorageAsDouble implements DoubleArrayAdapter {
    private final Storage<BigInteger> storage;

    private BigIntegerStorageAsDouble(Storage<BigInteger> storage) {
      this.storage = storage;
    }

    @Override
    public double getItemAsDouble(long i) {
      BigInteger x = storage.getItemBoxed(i);
      return x.doubleValue();
    }

    @Override
    public boolean isNothing(long i) {
      return storage.isNothing(i);
    }

    @Override
    public long size() {
      return storage.getSize();
    }
  }

  class BigDecimalStorageAsDouble implements DoubleArrayAdapter {
    private final Storage<BigDecimal> storage;

    private BigDecimalStorageAsDouble(Storage<BigDecimal> storage) {
      this.storage = storage;
    }

    @Override
    public double getItemAsDouble(long i) {
      BigDecimal x = storage.getItemBoxed(i);
      return x.doubleValue();
    }

    @Override
    public boolean isNothing(long i) {
      return storage.isNothing(i);
    }

    @Override
    public long size() {
      return storage.getSize();
    }
  }
}
