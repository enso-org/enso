package org.enso.table.data.column.storage;

import org.enso.table.data.column.storage.type.StorageType;

/** Basic interface of a column storage. */
public interface ColumnStorage<T> extends Iterable<T> {
  /* Gets the size of the storage. */
  long getSize();

  /* Gets the value type of the storage. */
  StorageType getType();

  /**
   * Checks whether the value at idx is Nothing.
   *
   * @param index – the index to check.
   * @return whether the value is Nothing.
   */
  boolean isNothing(long index);

  /* Gets the value at a given index. */
  T getItemBoxed(long index);

  @Override
  ColumnStorageIterator<T> iterator();
}
