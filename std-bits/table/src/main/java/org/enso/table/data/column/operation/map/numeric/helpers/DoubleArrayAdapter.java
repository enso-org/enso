package org.enso.table.data.column.operation.map.numeric.helpers;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.problems.BlackholeProblemAggregator;

public interface DoubleArrayAdapter {
  double getItemAsDouble(long i);

  boolean isNothing(long i);

  int size();

  default Storage<Double> intoStorage() {
    int n = size();
    var builder = Builder.getForDouble(FloatType.FLOAT_64, n, BlackholeProblemAggregator.INSTANCE);
    for (int i = 0; i < n; i++) {
      if (isNothing(i)) {
        builder.appendNulls(1);
      } else {
        builder.appendDouble(getItemAsDouble(i));
      }
    }
    return builder.seal();
  }

  static DoubleArrayAdapter fromStorage(BigIntegerStorage storage) {
    return new BigIntegerStorageAsDouble(storage);
  }

  static DoubleArrayAdapter fromStorage(BigDecimalStorage storage) {
    return new BigDecimalStorageAsDouble(storage);
  }

  static DoubleArrayAdapter fromStorage(AbstractLongStorage storage) {
    return new LongStorageAsDouble(storage);
  }

  static DoubleArrayAdapter fromStorage(DoubleStorage storage) {
    return storage;
  }

  static DoubleArrayAdapter fromAnyStorage(Storage<?> storage) {
    return switch (storage) {
      case DoubleStorage s -> fromStorage(s);
      case AbstractLongStorage s -> fromStorage(s);
      case BigIntegerStorage s -> fromStorage(s);
      case BigDecimalStorage s -> fromStorage(s);
      default -> throw new IllegalStateException(
          "Unsupported storage: " + storage.getClass().getCanonicalName());
    };
  }

  class LongStorageAsDouble implements DoubleArrayAdapter {
    private final AbstractLongStorage storage;

    private LongStorageAsDouble(AbstractLongStorage storage) {
      this.storage = storage;
    }

    @Override
    public double getItemAsDouble(long i) {
      long x = storage.getItemLong(i);
      return (double) x;
    }

    @Override
    public boolean isNothing(long i) {
      return storage.isNothing(i);
    }

    @Override
    public int size() {
      // ToDo: Will remove these adapters in the next step.
      return (int) storage.getSize();
    }
  }

  class BigIntegerStorageAsDouble implements DoubleArrayAdapter {
    private final BigIntegerStorage storage;

    private BigIntegerStorageAsDouble(BigIntegerStorage storage) {
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
    public int size() {
      // ToDo: Will remove these adapters in the next step.
      return (int) storage.getSize();
    }
  }

  class BigDecimalStorageAsDouble implements DoubleArrayAdapter {
    private final BigDecimalStorage storage;

    private BigDecimalStorageAsDouble(BigDecimalStorage storage) {
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
    public int size() {
      // ToDo: Will remove these adapters in the next step.
      return (int) storage.getSize();
    }
  }
}
