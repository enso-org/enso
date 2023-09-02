package org.enso.table.data.column.builder;

import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.problems.UnsupportedFeature;
import org.enso.table.problems.AggregatedProblems;

import java.math.BigInteger;
import java.util.Arrays;

// For now the BigInteger builder is just a stub, reusing the ObjectBuilder and adding a warning.
public class BigIntegerBuilder extends TypedBuilderImpl<BigInteger> {
  @Override
  protected BigInteger[] newArray(int size) {
    return new BigInteger[size];
  }

  public BigIntegerBuilder(int size) {
    super(size);
  }

  @Override
  public void writeTo(Object[] items) {
    super.writeTo(items);
  }

  @Override
  public boolean canRetypeTo(StorageType type) {
    return type instanceof FloatType || type instanceof AnyObjectType;
  }

  @Override
  public TypedBuilder retypeTo(StorageType type) {
    if (type instanceof FloatType) {
      DoubleBuilder res = NumericBuilder.createDoubleBuilder(currentSize);
      for (int i = 0; i < currentSize; i++) {
        if (data[i] == null) {
          res.appendNulls(1);
        } else {
          res.appendBigInteger(data[i]);
        }
      }
      return res;
    } else if (type instanceof AnyObjectType) {
      Object[] widenedData = Arrays.copyOf(data, data.length, Object[].class);
      ObjectBuilder res = new MixedBuilder(widenedData);
      res.setCurrentSize(currentSize);
      res.setPreExistingProblems(getProblems());
      return res;
    } else {
      throw new UnsupportedOperationException();
    }
  }

  private boolean wasSealed = false;

  @Override
  protected Storage<BigInteger> doSeal() {
    wasSealed = true;
    return new BigIntegerStorage(data, currentSize);
  }

  @Override
  public StorageType getType() {
    return BigIntegerType.INSTANCE;
  }

  @Override
  public boolean accepts(Object o) {
    return NumericConverter.isCoercibleToLong(o) || o instanceof BigInteger;
  }

  @Override
  public void appendNoGrow(Object o) {
    if (o == null) {
      data[currentSize++] = null;
    } else if (o instanceof BigInteger i) {
      data[currentSize++] = i;
    } else {
      long x = NumericConverter.coerceToLong(o);
      data[currentSize++] = BigInteger.valueOf(x);
    }
  }

  public void appendRawNoGrow(BigInteger value) {
    data[currentSize++] = value;
  }

  @Override
  public AggregatedProblems getProblems() {
    AggregatedProblems parent = super.getProblems();
    if (wasSealed) {
      parent.add(new UnsupportedFeature("Big integer arithmetic is currently not supported. You can cast to Value_Type.Integer which will replace the big integer values with Nothing."));
    }
    return parent;
  }

  public static BigIntegerBuilder retypeFromLongBuilder(LongBuilder longBuilder) {
    int n = longBuilder.currentSize;
    BigIntegerBuilder res = new BigIntegerBuilder(n);
    for (int i = 0; i < n; i++) {
      res.appendNoGrow(BigInteger.valueOf(longBuilder.data[i]));
    }
    return res;
  }
}
