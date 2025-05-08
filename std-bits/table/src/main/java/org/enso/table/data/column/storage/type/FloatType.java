package org.enso.table.data.column.storage.type;

import java.math.BigDecimal;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForDouble;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.problems.ProblemAggregator;

public record FloatType(Bits bits) implements StorageType<Double>, NumericType {
  public static final FloatType FLOAT_64 = new FloatType(Bits.BITS_64);

  public FloatType {
    if (bits != Bits.BITS_64) {
      throw new IllegalArgumentException("Only 64-bit floats are currently supported.");
    }
  }

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
    return other instanceof FloatType;
  }

  @Override
  public Double valueAsType(Object value) {
    if (NumericConverter.isCoercibleToDouble(value) || value instanceof BigDecimal) {
      return NumericConverter.coerceToDouble(value);
    }
    return null;
  }

  @Override
  public BuilderForDouble makeBuilder(long initialCapacity, ProblemAggregator problemAggregator) {
    return Builder.getForDouble(this, initialCapacity, problemAggregator);
  }

  @Override
  public ColumnDoubleStorage asTypedStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof FloatType) {
      var output = (ColumnDoubleStorage) storage;
      return output;
    }
    throw new IllegalArgumentException("Storage is not of FloatType");
  }
}
