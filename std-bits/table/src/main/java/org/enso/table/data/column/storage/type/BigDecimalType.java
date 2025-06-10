package org.enso.table.data.column.storage.type;

import java.math.BigDecimal;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.problems.ProblemAggregator;

public record BigDecimalType() implements StorageType<BigDecimal>, NumericType {
  public static final BigDecimalType INSTANCE = new BigDecimalType();

  @Override
  public boolean isNumeric() {
    return true;
  }

  @Override
  public boolean hasDate() {
    return false;
  }

  @Override
  public boolean hasTime() {
    return false;
  }

  @Override
  public boolean isOfType(StorageType<?> other) {
    return other instanceof BigDecimalType;
  }

  @Override
  public BigDecimal valueAsType(Object value) {
    if (value instanceof BigDecimal bigDecimal) {
      return bigDecimal;
    }

    if (NumericConverter.isCoercibleToBigInteger(value)) {
      return new BigDecimal(NumericConverter.coerceToBigInteger(value));
    }

    if (NumericConverter.isFloatLike(value)) {
      double doubleValue = NumericConverter.coerceToDouble(value);
      return BigDecimal.valueOf(doubleValue);
    }

    return null;
  }

  @Override
  public BuilderForType<BigDecimal> makeBuilder(
      long initialCapacity, ProblemAggregator problemAggregator) {
    return Builder.getForBigDecimal(initialCapacity);
  }

  @Override
  public ColumnStorage<BigDecimal> asTypedStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof BigDecimalType) {
      @SuppressWarnings("unchecked")
      var output = (ColumnStorage<BigDecimal>) storage;
      return output;
    }
    throw new IllegalArgumentException("Storage is not of BigDecimalType");
  }
}
