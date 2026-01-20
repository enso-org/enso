package org.enso.table.data.column.storage.type;

import java.math.BigInteger;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.problems.ProblemAggregator;

public final class BigIntegerType implements StorageType<BigInteger>, NumericType {
  public static final BigIntegerType INSTANCE = new BigIntegerType();

  private BigIntegerType() {}

  @Override
  public char typeChar() {
    return 'E';
  }

  @Override
  public boolean isNumeric() {
    return true;
  }

  @Override
  public boolean isOfType(StorageType<?> other) {
    return other instanceof BigIntegerType;
  }

  @Override
  public BigInteger valueAsType(Object value) {
    if (NumericConverter.isCoercibleToBigInteger(value)) {
      return NumericConverter.coerceToBigInteger(value);
    }
    return null;
  }

  @Override
  public BuilderForType<BigInteger> makeBuilder(
      long initialCapacity, ProblemAggregator problemAggregator) {
    return Builder.getForBigInteger(initialCapacity, problemAggregator);
  }

  @Override
  public ColumnStorage<BigInteger> asTypedStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof BigIntegerType) {
      @SuppressWarnings("unchecked")
      var output = (ColumnStorage<BigInteger>) storage;
      return output;
    }
    throw new IllegalArgumentException("Storage is not of BigIntegerType");
  }
}
