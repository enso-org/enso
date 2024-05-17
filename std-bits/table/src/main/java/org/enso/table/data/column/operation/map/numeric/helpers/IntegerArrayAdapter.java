package org.enso.table.data.column.operation.map.numeric.helpers;

import java.math.BigInteger;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;

public interface IntegerArrayAdapter {
  Integer getItemAsInteger(int i, MapOperationProblemAggregator problemAggregator);

  boolean isNothing(long i);

  int size();

  static IntegerArrayAdapter  fromStorage(BigIntegerStorage storage) {
    return new BigIntegerStorageAsInteger(storage);
  }

  static IntegerArrayAdapter  fromStorage(AbstractLongStorage storage) {
    return new LongStorageAsInteger(storage);
  }

  class LongStorageAsInteger implements IntegerArrayAdapter {
    private final AbstractLongStorage storage;

    private LongStorageAsInteger(AbstractLongStorage storage) {
      this.storage = storage;
    }

    @Override
    public Integer getItemAsInteger(int i, MapOperationProblemAggregator problemAggregator) {
      long x = storage.getItem(i);
      if (x > Integer.MAX_VALUE || x < Integer.MIN_VALUE) {
        problemAggregator.reportIllegalArgumentError("The exponent in Decimal.pow (^) must be an Integer (between -2147483648 and 2147483647), but was "+x, i);
        return null;
      }
      return (int) x;
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

  class BigIntegerStorageAsInteger implements IntegerArrayAdapter {
    private final BigIntegerStorage storage;

    private BigIntegerStorageAsInteger(BigIntegerStorage storage) {
      this.storage = storage;
    }

    @Override
    public Integer getItemAsInteger(int i, MapOperationProblemAggregator problemAggregator) {
      BigInteger x = storage.getItem(i);
      if (x.compareTo(NumericConverter.INTEGER_MAX_VALUE_BIG_INTEGER) > 0 || x.compareTo(NumericConverter.INTEGER_MIN_VALUE_BIG_INTEGER) < 0) {
        problemAggregator.reportIllegalArgumentError("The exponent in Decimal.pow (^) must be an Integer (between -2147483648 and 2147483647), but was "+x, i);
        return null;
      }
      return x.intValue();
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
}
