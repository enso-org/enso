package org.enso.table.data.column.operation.map.numeric.helpers;

import java.math.BigInteger;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;

public interface BigIntegerArrayAdapter {
  BigInteger getItem(int i);

  int size();

  static BigIntegerArrayAdapter fromStorage(BigIntegerStorage storage) {
    return new BigIntegerStorageAsBigInteger(storage);
  }

  static BigIntegerArrayAdapter fromStorage(AbstractLongStorage storage) {
    return new LongStorageAsBigInteger(storage);
  }

  class BigIntegerStorageAsBigInteger implements BigIntegerArrayAdapter {
    private final BigIntegerStorage storage;

    private BigIntegerStorageAsBigInteger(BigIntegerStorage storage) {
      this.storage = storage;
    }

    @Override
    public BigInteger getItem(int i) {
      return storage.getItemBoxed(i);
    }

    @Override
    public int size() {
      return storage.size();
    }
  }

  class LongStorageAsBigInteger implements BigIntegerArrayAdapter {
    private final AbstractLongStorage storage;

    private LongStorageAsBigInteger(AbstractLongStorage storage) {
      this.storage = storage;
    }

    @Override
    public BigInteger getItem(int i) {
      if (storage.isNa(i)) {
        return null;
      } else {
        long x = storage.getItem(i);
        return BigInteger.valueOf(x);
      }
    }

    @Override
    public int size() {
      return storage.size();
    }
  }
}
