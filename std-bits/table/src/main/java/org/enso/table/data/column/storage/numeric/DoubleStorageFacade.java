package org.enso.table.data.column.storage.numeric;

import java.util.function.ToDoubleFunction;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ValueIsNothingException;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.StorageType;

/** A facade for a column storage that converts the stored type to a double. */
public class DoubleStorageFacade<T> implements ColumnDoubleStorage {
  private final ColumnStorage<T> parent;
  private final ToDoubleFunction<T> converter;

  public DoubleStorageFacade(ColumnStorage<T> parent, ToDoubleFunction<T> converter) {
    this.parent = parent;
    this.converter = converter;
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
}
