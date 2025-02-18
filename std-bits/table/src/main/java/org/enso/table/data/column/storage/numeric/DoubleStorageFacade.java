package org.enso.table.data.column.storage.numeric;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.function.ToDoubleFunction;
import org.enso.table.data.column.storage.*;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.StorageType;

/** A facade for a column storage that converts the stored type to a double. */
public final class DoubleStorageFacade<T> implements ColumnDoubleStorage {
  private final ColumnStorage<T> parent;
  private final ToDoubleFunction<T> converter;

  public DoubleStorageFacade(ColumnStorage<T> parent, ToDoubleFunction<T> converter) {
    this.parent = parent;
    this.converter = converter;
  }

  public static ColumnDoubleStorage forBigInteger(ColumnStorage<BigInteger> parent) {
    return new DoubleStorageFacade<>(parent, BigInteger::doubleValue);
  }

  public static ColumnDoubleStorage forBigDecimal(ColumnStorage<BigDecimal> parent) {
    return new DoubleStorageFacade<>(parent, BigDecimal::doubleValue);
  }

  @Override
  public double getItemAsDouble(long index) throws ValueIsNothingException {
    if (isNothing(index)) {
      throw new ValueIsNothingException(index);
    }
    T item = parent.getItemBoxed(index);
    return converter.applyAsDouble(item);
  }

  @Override
  public long getSize() {
    return parent.getSize();
  }

  @Override
  public StorageType getType() {
    return FloatType.FLOAT_64;
  }

  @Override
  public boolean isNothing(long index) {
    return parent.isNothing(index);
  }

  @Override
  public Double getItemBoxed(long index) {
    T item = parent.getItemBoxed(index);
    return item == null ? null : converter.applyAsDouble(item);
  }

  @Override
  public ColumnDoubleStorageIterator iterator() {
    return new BaseDoubleStorageIterator(this);
  }

  private static class BaseDoubleStorageIterator extends Storage.StorageIterator<Double>
      implements ColumnDoubleStorageIterator {
    public BaseDoubleStorageIterator(ColumnDoubleStorage parent) {
      super(parent);
    }

    @Override
    public double getItemAsDouble() {
      Double d = getItemBoxed();
      return d == null ? Double.NaN : d;
    }
  }
}
