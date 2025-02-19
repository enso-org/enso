package org.enso.table.data.column.storage;

import java.util.function.Function;
import org.enso.table.data.column.storage.type.StorageType;

/** A facade for a column storage that converts the stored type to another type. */
public final class ColumnStorageFacade<S, T> implements ColumnStorage<T> {
  private final ColumnStorage<S> parent;
  private final Function<S, T> converter;

  public ColumnStorageFacade(ColumnStorage<S> parent, Function<S, T> converter) {
    this.parent = parent;
    this.converter = converter;
  }

  @Override
  public long getSize() {
    return parent.getSize();
  }

  @Override
  public StorageType getType() {
    throw new UnsupportedOperationException("Not implemented");
  }

  @Override
  public boolean isNothing(long index) {
    return parent.isNothing(index);
  }

  @Override
  public T getItemBoxed(long index) {
    S item = parent.getItemBoxed(index);
    return item == null ? null : converter.apply(item);
  }

  @Override
  public ColumnStorageIterator<T> iterator() {
    return new Storage.StorageIterator<>(this);
  }
}
