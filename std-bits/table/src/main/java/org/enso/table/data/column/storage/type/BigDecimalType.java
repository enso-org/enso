package org.enso.table.data.column.storage.type;

import java.math.BigDecimal;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.problems.ProblemAggregator;

public final class BigDecimalType implements StorageType<BigDecimal>, NumericType {
  public static final BigDecimalType INSTANCE = new BigDecimalType();

  private final int scale;

  private BigDecimalType() {
    this.scale = -1;
  }

  /**
   * Creates a BigDecimalType with a specified scale. Used for going to databases - In-Memory Enso
   * always uses scale -1.
   *
   * @param scale the scale to be used for BigDecimal values
   */
  public BigDecimalType(int scale) {
    this.scale = scale;
  }

  public int getScale() {
    return scale;
  }

  @Override
  public char typeChar() {
    return 'D';
  }

  @Override
  public boolean isNumeric() {
    return true;
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
