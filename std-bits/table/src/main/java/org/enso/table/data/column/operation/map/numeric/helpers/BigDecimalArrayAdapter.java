package org.enso.table.data.column.operation.map.numeric.helpers;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;

public interface BigDecimalArrayAdapter {
  BigDecimal getItem(int i);

  int size();

  static BigDecimalArrayAdapter fromStorage(BigDecimalStorage storage) {
    return new BigDecimalStorageAsBigDecimal(storage);
  }

  static BigDecimalArrayAdapter fromStorage(BigIntegerStorage storage) {
    return new BigIntegerStorageAsBigDecimal(storage);
  }

  static BigDecimalArrayAdapter fromStorage(AbstractLongStorage storage) {
    return new LongStorageAsBigDecimal(storage);
  }

  static BigDecimalArrayAdapter fromStorage(DoubleStorage storage) {
    return new DoubleStorageAsBigDecimal(storage);
  }

  static BigDecimalArrayAdapter fromAnyStorage(Storage<?> storage) {
    return switch (storage) {
      case DoubleStorage s -> fromStorage(s);
      case AbstractLongStorage s -> fromStorage(s);
      case BigIntegerStorage s -> fromStorage(s);
      case BigDecimalStorage s -> fromStorage(s);
      default -> throw new IllegalStateException(
          "Unsupported storage: " + storage.getClass().getCanonicalName());
    };
  }


  class BigDecimalStorageAsBigDecimal implements BigDecimalArrayAdapter {
    private final BigDecimalStorage storage;

    private BigDecimalStorageAsBigDecimal(BigDecimalStorage storage) {
      this.storage = storage;
    }

    @Override
    public BigDecimal getItem(int i) {
      return storage.getItemBoxed(i);
    }

    @Override
    public int size() {
      return storage.size();
    }
  }

  class BigIntegerStorageAsBigDecimal implements BigDecimalArrayAdapter {
    private final BigIntegerStorage storage;

    private BigIntegerStorageAsBigDecimal(BigIntegerStorage storage) {
      this.storage = storage;
    }

    @Override
    public BigDecimal getItem(int i) {
      return new BigDecimal(storage.getItemBoxed(i));
    }

    @Override
    public int size() {
      return storage.size();
    }
  }

  class LongStorageAsBigDecimal implements BigDecimalArrayAdapter {
    private final AbstractLongStorage storage;

    private LongStorageAsBigDecimal(AbstractLongStorage storage) {
      this.storage = storage;
    }

    @Override
    public BigDecimal getItem(int i) {
      if (storage.isNothing(i)) {
        return null;
      } else {
        long x = storage.getItem(i);
        return BigDecimal.valueOf(x);
      }
    }

    @Override
    public int size() {
      return storage.size();
    }
  }

  class DoubleStorageAsBigDecimal implements BigDecimalArrayAdapter {
    private final DoubleStorage storage;

    private DoubleStorageAsBigDecimal(DoubleStorage storage) {
      this.storage = storage;
    }

    @Override
    public BigDecimal getItem(int i) {
      if (storage.isNothing(i)) {
        return null;
      } else {
        double x = storage.getItemAsDouble(i);
        return BigDecimal.valueOf(x);
      }
    }

    @Override
    public int size() {
      return storage.size();
    }
  }
}
