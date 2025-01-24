package org.enso.table.data.column.operation.map.numeric.helpers;

import java.math.BigInteger;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;

public interface BigIntegerArrayAdapter {
  BigInteger getItem(int i);

  int size();

  default Storage<BigInteger> intoStorage() {
    int n = size();
    var builder = Builder.getForBigInteger(n, null);
    for (int i = 0; i < n; i++) {
      builder.append(getItem(i));
    }
    return builder.seal();
  }

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
      return storage.getBoxed(i);
    }

    @Override
    public int size() {
      // ToDo: Will remove these adapters in the next step.
      return (int)storage.getSize();
    }

    @Override
    public BigIntegerStorage intoStorage() {
      return storage;
    }
  }

  class LongStorageAsBigInteger implements BigIntegerArrayAdapter {
    private final AbstractLongStorage storage;

    private LongStorageAsBigInteger(AbstractLongStorage storage) {
      this.storage = storage;
    }

    @Override
    public BigInteger getItem(int i) {
      if (storage.isNothing(i)) {
        return null;
      } else {
        long x = storage.getPrimitive(i);
        return BigInteger.valueOf(x);
      }
    }

    @Override
    public int size() {
      // ToDo: Will remove these adapters in the next step.
      return (int)storage.getSize();
    }
  }
}
