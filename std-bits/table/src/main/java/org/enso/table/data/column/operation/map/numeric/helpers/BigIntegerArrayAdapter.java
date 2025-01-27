package org.enso.table.data.column.operation.map.numeric.helpers;

import java.math.BigInteger;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.problems.BlackholeProblemAggregator;

public interface BigIntegerArrayAdapter {
  BigInteger getItem(long i);

  long size();

  default Storage<BigInteger> intoStorage() {
    long n = size();
    var builder = Builder.getForBigInteger(n, BlackholeProblemAggregator.INSTANCE);
    for (long i = 0; i < n; i++) {
      builder.append(getItem(i));
    }
    return builder.seal();
  }

  static BigIntegerArrayAdapter fromStorage(Storage<BigInteger> storage) {
    return new BigIntegerStorageAsBigInteger(storage);
  }

  static BigIntegerArrayAdapter fromStorage(ColumnLongStorage storage) {
    return new LongStorageAsBigInteger(storage);
  }

  class BigIntegerStorageAsBigInteger implements BigIntegerArrayAdapter {
    private final Storage<BigInteger> storage;

    private BigIntegerStorageAsBigInteger(Storage<BigInteger> storage) {
      this.storage = storage;
    }

    @Override
    public BigInteger getItem(long i) {
      return storage.getItemBoxed(i);
    }

    @Override
    public long size() {
      return storage.getSize();
    }

    @Override
    public Storage<BigInteger> intoStorage() {
      return storage;
    }
  }

  class LongStorageAsBigInteger implements BigIntegerArrayAdapter {
    private final ColumnLongStorage storage;

    private LongStorageAsBigInteger(ColumnLongStorage storage) {
      this.storage = storage;
    }

    @Override
    public BigInteger getItem(long i) {
      if (storage.isNothing(i)) {
        return null;
      } else {
        long x = storage.getItemAsLong(i);
        return BigInteger.valueOf(x);
      }
    }

    @Override
    public long size() {
      return storage.getSize();
    }
  }
}
