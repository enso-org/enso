package org.enso.table.data.column.storage.numeric;

import java.util.function.ToLongFunction;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnLongStorageIterator;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ValueIsNothingException;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;

/** A facade for a column storage that converts the stored type to a long. */
public class LongStorageFacade<T> implements ColumnLongStorage {
  private final ColumnStorage<T> parent;
  private final ToLongFunction<T> converter;

  public LongStorageFacade(ColumnStorage<T> parent, ToLongFunction<T> converter) {
    this.parent = parent;
    this.converter = converter;
  }

  @Override
  public long getItemAsLong(long index) throws ValueIsNothingException {
    if (isNothing(index)) {
      throw new ValueIsNothingException(index);
    }
    T item = parent.getItemBoxed(index);
    return converter.applyAsLong(item);
  }

  @Override
  public long getSize() {
    return parent.getSize();
  }

  @Override
  public StorageType<Long> getType() {
    return IntegerType.INT_64;
  }

  @Override
  public boolean isNothing(long index) {
    return parent.isNothing(index);
  }

  @Override
  public Long getItemBoxed(long index) {
    T item = parent.getItemBoxed(index);
    return item == null ? null : converter.applyAsLong(item);
  }

  @Override
  public ColumnLongStorageIterator iterator() {
    return new AbstractLongStorage.BaseLongStorageIterator(this);
  }
}
