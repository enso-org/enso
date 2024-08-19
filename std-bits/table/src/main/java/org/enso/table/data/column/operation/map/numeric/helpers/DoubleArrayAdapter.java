package org.enso.table.data.column.operation.map.numeric.helpers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.BitSet;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;

public interface DoubleArrayAdapter {
  double getItemAsDouble(int i);

  boolean isNothing(long i);

  int size();

  default DoubleStorage intoStorage() {
    int n = size();
    long[] values = new long[n];
    BitSet isNothing = new BitSet();
    for (int i = 0; i < n; i++) {
      if (isNothing(i)) {
        isNothing.set(i);
      } else {
        values[i] = Double.doubleToRawLongBits(getItemAsDouble(i));
      }
    }
    return new DoubleStorage(values, n, isNothing);
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
    public double getItemAsDouble(int i) {
      long x = storage.getItem(i);
      return (double) x;
    }

    @Override
    public boolean isNothing(long i) {
      return storage.isNothing(i);
    }

    @Override
    public int size() {
      return storage.size();
    }
  }

  class BigIntegerStorageAsDouble implements DoubleArrayAdapter {
    private final BigIntegerStorage storage;

    private BigIntegerStorageAsDouble(BigIntegerStorage storage) {
      this.storage = storage;
    }

    @Override
    public double getItemAsDouble(int i) {
      BigInteger x = storage.getItem(i);
      return x.doubleValue();
    }

    @Override
    public boolean isNothing(long i) {
      return storage.getItem(i) == null;
    }

    @Override
    public int size() {
      return storage.size();
    }
  }

  class BigDecimalStorageAsDouble implements DoubleArrayAdapter {
    private final BigDecimalStorage storage;

    private BigDecimalStorageAsDouble(BigDecimalStorage storage) {
      this.storage = storage;
    }

    @Override
    public double getItemAsDouble(int i) {
      BigDecimal x = storage.getItem(i);
      return x.doubleValue();
    }

    @Override
    public boolean isNothing(long i) {
      return storage.getItem(i) == null;
    }

    @Override
    public int size() {
      return storage.size();
    }
  }
}
