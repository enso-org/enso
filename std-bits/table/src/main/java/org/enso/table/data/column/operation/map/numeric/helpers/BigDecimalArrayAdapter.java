package org.enso.table.data.column.operation.map.numeric.helpers;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;

public interface BigDecimalArrayAdapter {
  BigDecimal getItem(long i);

  long size();

  default Storage<BigDecimal> intoStorage() {
    long n = size();
    var builder = Builder.getForBigDecimal(n);
    for (long i = 0; i < n; i++) {
      builder.append(getItem(i));
    }
    return builder.seal();
  }

  static BigDecimalArrayAdapter fromBigDecimalStorage(ColumnStorage<BigDecimal> storage) {
    return new BigDecimalStorageAsBigDecimal(storage);
  }

  static BigDecimalArrayAdapter fromBigIntegerStorage(ColumnStorage<BigInteger> storage) {
    return new BigIntegerStorageAsBigDecimal(storage);
  }

  static BigDecimalArrayAdapter fromStorage(ColumnLongStorage storage) {
    return new LongStorageAsBigDecimal(storage);
  }

  static BigDecimalArrayAdapter fromStorage(ColumnDoubleStorage storage) {
    return new DoubleStorageAsBigDecimal(storage);
  }

  static BigDecimalArrayAdapter fromAnyStorage(ColumnStorage<?> storage) {
    return switch (storage) {
      case ColumnDoubleStorage s -> fromStorage(s);
      case ColumnLongStorage s -> fromStorage(s);
      case BigIntegerStorage s -> new BigIntegerStorageAsBigDecimal(s);
      case BigDecimalStorage s -> new BigDecimalStorageAsBigDecimal(s);
      default -> throw new IllegalStateException(
          "Unsupported storage: " + storage.getClass().getCanonicalName());
    };
  }

  class BigDecimalStorageAsBigDecimal implements BigDecimalArrayAdapter {
    private final ColumnStorage<BigDecimal> storage;

    private BigDecimalStorageAsBigDecimal(ColumnStorage<BigDecimal> storage) {
      this.storage = storage;
    }

    @Override
    public BigDecimal getItem(long i) {
      return storage.getItemBoxed(i);
    }

    @Override
    public long size() {
      return storage.getSize();
    }

    @Override
    public Storage<BigDecimal> intoStorage() {
      if (storage instanceof Storage<BigDecimal> specialized) {
        return specialized;
      } else {
        return BigDecimalArrayAdapter.super.intoStorage();
      }
    }
  }

  class BigIntegerStorageAsBigDecimal implements BigDecimalArrayAdapter {
    private final ColumnStorage<BigInteger> storage;

    private BigIntegerStorageAsBigDecimal(ColumnStorage<BigInteger> storage) {
      this.storage = storage;
    }

    @Override
    public BigDecimal getItem(long i) {
      return new BigDecimal(storage.getItemBoxed(i));
    }

    @Override
    public long size() {
      return storage.getSize();
    }
  }

  class LongStorageAsBigDecimal implements BigDecimalArrayAdapter {
    private final ColumnLongStorage storage;

    private LongStorageAsBigDecimal(ColumnLongStorage storage) {
      this.storage = storage;
    }

    @Override
    public BigDecimal getItem(long i) {
      if (storage.isNothing(i)) {
        return null;
      } else {
        long x = storage.getItemAsLong(i);
        return BigDecimal.valueOf(x);
      }
    }

    @Override
    public long size() {
      return storage.getSize();
    }
  }

  class DoubleStorageAsBigDecimal implements BigDecimalArrayAdapter {
    private final ColumnDoubleStorage storage;

    private DoubleStorageAsBigDecimal(ColumnDoubleStorage storage) {
      this.storage = storage;
    }

    @Override
    public BigDecimal getItem(long i) {
      if (storage.isNothing(i)) {
        return null;
      } else {
        double x = storage.getItemAsDouble(i);
        return BigDecimal.valueOf(x);
      }
    }

    @Override
    public long size() {
      return storage.getSize();
    }
  }
}
