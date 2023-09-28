package org.enso.table.data.column.builder;

import java.math.BigInteger;
import java.util.Arrays;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.error.ValueTypeMismatchException;
import org.graalvm.polyglot.Context;

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

  @Override
  protected Storage<BigInteger> doSeal() {
    return new BigIntegerStorage(data, currentSize);
  }

  @Override
  public StorageType getType() {
    return BigIntegerType.INSTANCE;
  }

  @Override
  public boolean accepts(Object o) {
    return NumericConverter.isCoercibleToBigInteger(o);
  }

  @Override
  public void appendNoGrow(Object o) {
    if (o == null) {
      data[currentSize++] = null;
    } else {
      try {
        data[currentSize++] = NumericConverter.coerceToBigInteger(o);
      } catch (UnsupportedOperationException e) {
        throw new ValueTypeMismatchException(BigIntegerType.INSTANCE, o);
      }
    }
  }

  public void appendRawNoGrow(BigInteger value) {
    data[currentSize++] = value;
  }

  public static BigIntegerBuilder retypeFromLongBuilder(LongBuilder longBuilder) {
    BigIntegerBuilder res = new BigIntegerBuilder(longBuilder.data.length);
    int n = longBuilder.currentSize;
    Context context = Context.getCurrent();
    for (int i = 0; i < n; i++) {
      res.appendNoGrow(BigInteger.valueOf(longBuilder.data[i]));
      context.safepoint();
    }
    return res;
  }
}
