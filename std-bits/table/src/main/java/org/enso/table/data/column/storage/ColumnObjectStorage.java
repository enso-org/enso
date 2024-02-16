package org.enso.table.data.column.storage;

public interface ColumnObjectStorage<T> extends ColumnStorage {
  /**
   * Gets the value at a given index.
   * Returns null if the index isNothing.
   * */
  T get(long Item);
}
